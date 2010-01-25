-module(riak_repl_tcp_server).
-include("riak_repl.hrl").
-behaviour(gen_fsm).
-export([start/1]).
-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([wait_peerinfo/2,
         merkle_send/2,
         merkle_wait_ack/2,
         connected/2]).
-record(state, 
        {
          socket,
          client,
          my_pi,
          partitions=[]
         }
       ).

start(Socket) ->
    gen_fsm:start(?MODULE, [Socket], []).

init([Socket]) ->
    %%io:format("~p starting, sock=~p~n", [?MODULE, Socket]),
    inet:setopts(Socket, [{active, once}, {packet, 4}]),
    riak_repl_eventer:subscribe(self()),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    Partitions = riak_repl_util:get_partitions(MyPeerInfo),
    ok = send(Socket, {peerinfo, MyPeerInfo}),
    {ok, wait_peerinfo, #state{socket=Socket,
                               client=Client,
                               partitions=Partitions,
                               my_pi=MyPeerInfo}}.

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo, socket=_Socket}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            %%io:format("peer info ok!~n"),
            {next_state, merkle_send, State, 0};
        false ->
            {stop, normal, State}
    end.

merkle_send(timeout, State=#state{socket=_Socket, partitions=[]}) ->
    %%io:format("sent last merkle~n"),
    {next_state, connected, State};
merkle_send(timeout, State=#state{socket=Socket, partitions=[Partition|T]}) ->
    case get_merkle(State, Partition) of
        {error, Reason} ->
            io:format("get_merkle error ~p for partition ~p~n", [Reason, Partition]),
            {next_state, merkle_send, State, 0};
        {ok, MerkleTree} ->
            %%io:format("sending merkle tree for partition ~p~n", [Partition]),
            ok = send(Socket, {merkle, Partition, MerkleTree}),
            {next_state, merkle_wait_ack, State#state{partitions=T}}
    end.

merkle_wait_ack({ack, _Partition, []}, State) ->
    {next_state, merkle_send, State, 0};
merkle_wait_ack({ack, Partition, DiffVClocks}, State=#state{client=Client, socket=Socket}) ->
    Actions = vclock_diff(Partition, DiffVClocks, State),
    {_GetMsg, SendMsg} = vclock_diff_response(Actions, Client, [], []),
    %%io:format("sending ~p get reqs and ~p send reqs~n", [length(element(2, _GetMsg)),
    %%                                                     length(element(2, SendMsg))]), 
    ok = send(Socket, {diff_response, Partition, SendMsg}),
    {next_state, merkle_wait_ack, State}.

connected(_E, State) ->
    {next_state, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    %%io:format("tcp socket closed ~p~n", [Socket]),
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    case binary_to_term(zlib:unzip(Data)) of
        {remote_update, Obj} ->
            riak_repl_util:do_repl_put(Obj),
            inet:setopts(Socket, [{active, once}]), 
            %%io:format("wrote remote update~n"), 
            {next_state, StateName, State};
        Msg ->
            R = ?MODULE:StateName(Msg, State),
            inet:setopts(Socket, [{active, once}]),            
            R
    end;
handle_info({local_update, Obj}, StateName, State=#state{socket=Socket}) ->
    %%io:format("got local update~n"),
    ok = send(Socket, {remote_update, Obj}),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    riak_repl_eventer:unsubscribe(self()),
    ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, zlib:zip(term_to_binary(Data))).
    
get_merkle(#state{my_pi=#peer_info{ring=Ring}}, Partition) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, Reason} ->
            %%io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
            %%          [?MODULE, Partition, OwnerNode, Reason]),
            {error, Reason};
        MerkleTree -> {ok, MerkleTree}
    end.


should_get(V1, V2) -> vclock:descends(V1, V2) =:= false.
should_send(V1, V2) -> vclock:descends(V2, V1) =:= false.
vclock_diff(Partition, DiffVClocks, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    Keys = [K || {K, _V} <- DiffVClocks],
    case riak_repl_util:vnode_master_call(OwnerNode, {get_vclocks, Partition, Keys}) of
        {error, _Reason} ->
            %%io:format("~p:error getting vclocks for ~p from ~p: ~p~n",
            %%          [?MODULE, Partition, OwnerNode, Reason]),
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

