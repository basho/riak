-module(riak_repl_tcp_client).
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
         merkle_exchange/2,
         connected/2]).

-record(state, {
          socket,
          client,
          my_pi,
          merk_q=[],
          update_q=[]
         }).

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
            MerkReqs = [{req_merkle, P} 
                        || P <- riak_repl_util:get_partitions(TheirPeerInfo)],
            {next_state, merkle_exchange, State#state{merk_q=MerkReqs}, 0};
        false ->
            {stop, normal, State}
    end.

merkle_exchange(timeout, State=#state{socket=Socket, merk_q=[]}) ->
    riak_repl_eventer:subscribe(self()),
    ok = send(Socket, event_subscribe),
    {next_state, connected, State};
merkle_exchange(timeout, State=#state{socket=Socket, merk_q=[H|T]}) ->
    ok = send(Socket, H),
    {next_state, merkle_exchange, State#state{merk_q=T}};
merkle_exchange({merkle, Partition, MerkleTree}, State=#state{socket=Socket}) ->
    %io:format("got merkle tree for partition ~p~n", [Partition]),
    case diff_merkle(Partition, MerkleTree, State) of
        [] ->
            %io:format("no merkle diff~n"),
            {next_state, merkle_exchange, State, 30000};
        DiffVClocks ->
            %io:format("got merkle diff of length ~p~n", [length(DiffVClocks)]),
            ok = send(Socket, {diff_vclocks, Partition, DiffVClocks}),
            {next_state, merkle_exchange, State}
    end;
merkle_exchange({diff_response,_Partition,{get, _Gets},{send, Sends}},
                State=#state{update_q=RQ}) ->
    NewState = State#state{update_q=lists:append(Sends, RQ)},
    {next_state, merkle_exchange, write_next(NewState), 30000};
merkle_exchange({remote_update, Obj}, State) ->
    NewState = enqueue_update(State, Obj),
    {next_state, connected, write_next(NewState)}.

connected({remote_update, Obj}, State) ->
    NewState = enqueue_update(State, Obj),
    {next_state, connected, write_next(NewState)};
connected({updates, ObjList}, State=#state{update_q=Q}) ->
    {next_state, connected, State#state{update_q=lists:append(ObjList, Q)}};
connected(_, State) ->
    {next_state, connected, State}.

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
handle_info({local_update, Obj}, StateName, State=#state{socket=Socket}) ->
    ok = send(Socket, {remote_update, Obj}),
    {next_state, StateName, State};
handle_info({_ReqId, _Status}, StateName, State) ->
    %io:format("repl fsm ~p finished: ~p~n", [_ReqId, _Status]),
    {next_state, StateName, write_next(State)}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

enqueue_update(State=#state{update_q=Q}, Obj) -> State#state{update_q=[Obj|Q]}.

send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, term_to_binary(Data, [compressed])).

diff_merkle(Partition, MerkleTree, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, _Reason} ->
            %io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
            %[?MODULE, Partition, OwnerNode, Reason]),
            [];
        OurMerkleTree ->
            DiffKeys = merkerl:diff(MerkleTree, OurMerkleTree),
            case riak_repl_util:vnode_master_call(OwnerNode,
                                                  {get_vclocks, Partition, DiffKeys}) of
                {error, _Reason} ->
                    %io:format("~p:error getting vclock list for ~p from ~p: ~p~n",
                    %[?MODULE, Partition, OwnerNode, Reason]),
                    [];
                VClocks ->
                    VClocks
            end
    end.

write_next(State=#state{update_q=[]}) -> State;
write_next(State=#state{update_q=[H|T]}) ->
    riak_repl_fsm:start(erlang:phash2(erlang:now()), H, 1, 1, ?REPL_FSM_TIMEOUT, self()),
    State#state{update_q=T}.
