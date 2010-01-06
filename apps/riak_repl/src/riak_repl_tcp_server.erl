-module(riak_repl_tcp_server).
-include("riak_repl.hrl").
-behaviour(gen_fsm).
-export([start/1]).
-export([init/1, 
         state_name/3, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([wait_peerinfo/2,
         merkle_exchange/2]).
-record(state, 
        {
          socket,
          client,
          my_pi,
          update_q=[]
         }
       ).

start(Socket) ->
    gen_fsm:start(?MODULE, [Socket], []).

init([Socket]) ->
    inet:setopts(Socket, [{active, once}, {packet, 4}]),
    riak_repl_eventer:subscribe(self()),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    ok = send(Socket, {peerinfo, MyPeerInfo}),
    {ok, wait_peerinfo, #state{socket=Socket,
                               client=Client,
                               my_pi=MyPeerInfo}}.

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            {next_state, merkle_exchange, State};
        false ->
            {stop, normal, State}
    end.

merkle_exchange({req_merkle, Partition}, State=#state{socket=Socket}) ->
    io:format("got merk exchange request for partition ~p~n", [Partition]),
    case get_merkle(State, Partition) of
        {error, Reason} ->
            io:format("got error ~p~n", [Reason]),
            nop;
        {ok, MerkleTree} ->
            io:format("sending merkle tree~n"),
            ok = send(Socket, {merkle, Partition, MerkleTree})
    end,
    {next_state, merkle_exchange, State};
merkle_exchange({diff_vclocks, Partition, DiffVClocks}, 
                State=#state{client=Client, socket=Socket}) ->
    Actions = vclock_diff(Partition, DiffVClocks, State),
    {GetMsg, SendMsg} = vclock_diff_response(Actions, Client, [], []),
    io:format("sending ~p get reqs and ~p send reqs~n", [length(element(2, GetMsg)),
                                                         length(element(2, SendMsg))]),
    ok = send(Socket, {diff_response, Partition, GetMsg, SendMsg}),
    {next_state, merkle_exchange, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    R = ?MODULE:StateName(binary_to_term(Data), State),
    inet:setopts(Socket, [{active, once}]),
    R;
handle_info({local_update, Obj}, StateName, State=#state{update_q=Q, socket=Socket}) ->
    case queue_full(State) of
        true ->
            ok = send(Socket, {updates, lists:reverse(Q)}),
            {next_state, StateName, State#state{update_q=[]}};
        false ->
            BinObj = zlib:zip(term_to_binary(Obj)),
            {next_state, StateName, enqueue_update(State, BinObj)}
    end.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

enqueue_update(State=#state{update_q=Q}, Obj) -> State#state{update_q=[Obj|Q]}.
queue_full(#state{update_q=Q}) -> length(Q) >= 100.


send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, term_to_binary(Data, [compressed])).

get_merkle(#state{my_pi=#peer_info{ring=Ring}}, Partition) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, Reason} ->
            io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            {error, Reason};
        MerkleTree -> {ok, MerkleTree}
    end.


should_get(V1, V2) -> vclock:descends(V1, V2) =:= false.
should_send(V1, V2) -> vclock:descends(V2, V1) =:= false.
vclock_diff(Partition, DiffVClocks, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    Keys = [K || {K, _V} <- DiffVClocks],
    case riak_repl_util:vnode_master_call(OwnerNode, {get_vclocks, Partition, Keys}) of
        {error, Reason} ->
            io:format("~p:error getting vclocks for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            [];
        OurVClocks ->
            vclock_diff1(DiffVClocks, OurVClocks, [])
    end.

vclock_diff1([], _, Acc) ->
    lists:reverse(Acc);
vclock_diff1([{K,VC}|T], OurVClocks, Acc) ->
    case proplists:get_value(K, OurVClocks) of
        undefined ->
            vclock_diff1(T, OurVClocks, Acc);
        VC ->
            vclock_diff1(T, OurVClocks, Acc);
        OurVClock ->
            case should_get(OurVClock, VC) of
                true ->
                    case should_send(OurVClock, VC) of
                        true ->
                            vclock_diff1(T, OurVClocks, lists:append([{get,K},{send,K}],Acc));
                        false ->
                            vclock_diff1(T, OurVClocks, [{get, K}|Acc])
                    end;
                false ->
                    case should_send(OurVClock, VC) of
                        true ->
                            vclock_diff1(T, OurVClocks, [{send,K}|Acc]);
                        false  ->
                            vclock_diff1(T, OurVClocks, Acc)
                    end
            end
    end.


vclock_diff_response([], _Client, Gets, Sends) ->
    {{get, lists:reverse(Gets)}, {send, lists:reverse(Sends)}};
vclock_diff_response([{get,{_B,K}}|T], Client, Gets, Sends) ->
    vclock_diff_response(T, Client, [K|Gets], Sends);
vclock_diff_response([{send,{B,K}}|T], Client, Gets, Sends) ->
    case Client:get(B, K, 1, ?REPL_FSM_TIMEOUT) of
        {ok, Obj} ->
            vclock_diff_response(T, Client, Gets, [Obj|Sends]);
        _ ->
            vclock_diff_response(T, Client, Gets, Sends)
    end.    

