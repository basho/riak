-module(riak_repl_peer).
-behaviour(gen_fsm).
-include("riak_repl.hrl").
-export([start/2]).
-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([connected/2,
         merkle_send/2,
         merkle_wait/2,
         wait_peer_info/2]).

-record(state, {socket, 
                is_client,
                my_peer_info, 
                update_q, 
                repl_q, 
                merk_q, 
                client}).

start(Socket, IsClient) ->
    gen_fsm:start(?MODULE, [Socket, IsClient], []).

init([Socket, IsClient]) ->
    inet:setopts(Socket, [{active, once}, {packet, 4}]),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    ok = send(Socket, MyPeerInfo),
    {ok, Client} = riak:local_client(),
    {ok, wait_peer_info, #state{socket=Socket, 
                                is_client=IsClient,
                                my_peer_info=MyPeerInfo,
                                update_q=[],
                                repl_q=[],
                                merk_q=riak_repl_util:get_partitions(MyPeerInfo),
                                client=Client}}.
wait_peer_info(TheirPeerInfo=#peer_info{}, State=#state{my_peer_info=MyPeerInfo, 
                                                        is_client=IsClient}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            case IsClient of
                true ->
                    {next_state, merkle_send, send_next_merkle(State)};
                false ->
                    {next_state, merkle_wait, State}
            end;
        false ->
            {stop, normal, State}
    end;
wait_peer_info(timeout, State=#state{socket=Socket, update_q=Q}) ->
    ok = send(Socket, {updates, lists:reverse(Q)}),
    {next_state, connected, State#state{update_q=[]}}.

merkle_wait(_Event, State=#state{merk_q=[]}) ->
    {next_state, connected, State};
merkle_wait({take_merkle, Partition, MerkleTree}, State=#state{socket=Socket}) ->
    case diff_merkle(Partition, MerkleTree, State) of
        [] ->
            {next_state, merkle_wait, State};
        DiffVClocks ->
            ok = send(Socket, {diff_vclocks, Partition, DiffVClocks}),
            {next_state, merkle_wait, State}
    end;
merkle_wait({diff_response,Partition,{get, _Gets},{send, Sends}},State=#state{merk_q=MQ,
                                                                              repl_q=RQ}) ->
    NewMQ = lists:delete(Partition, MQ),
    NewState = State#state{repl_q=lists:append(Sends, RQ), merk_q=NewMQ},
    {next_state, merkle_wait, write_next(NewState)};
merkle_wait({updates, ObjList}, State=#state{repl_q=RQ}) ->
    NewState = State#state{repl_q=lists:append(RQ, ObjList)},
    {next_state, merkle_wait, write_next(NewState)}.

merkle_send(_Event, State=#state{merk_q=[]}) ->
    {next_state, connected, State};
merkle_send({diff_vclocks, Partition, DiffVClocks}, State=#state{client=Client, 
                                                                 socket=Socket}) ->
    Actions = vclock_diff(Partition, DiffVClocks, State),
    {GetMsg, SendMsg} = vclock_diff_response(Actions, Client, [], []),
    ok = send(Socket, {diff_response, Partition, GetMsg, SendMsg}),
    {next_state, merkle_send, send_next_merkle(State)};
merkle_send(timeout, State=#state{socket=Socket, update_q=Q}) ->
    ok = send(Socket, {updates, lists:reverse(Q)}),
    {next_state, merkle_send, State#state{update_q=[]}};
merkle_send({updates, ObjList}, State=#state{repl_q=RQ}) ->
    NewState = State#state{repl_q=lists:append(RQ, ObjList)},
    {next_state, merkle_send, send_next_merkle(write_next(NewState))}.

connected(timeout, State=#state{socket=Socket, update_q=Q}) ->
    ok = send(Socket, {updates, lists:reverse(Q)}),
    {next_state, connected, State#state{update_q=[]}};
connected({updates, ObjList}, State=#state{repl_q=RQ}) ->
    NewState = State#state{repl_q=lists:append(RQ, ObjList)},
    {next_state, connected, write_next(NewState)}.

handle_event({local_update, Obj}, StateName, State=#state{update_q=Q, socket=Socket}) ->
    case queue_full(State) of
        true ->
            ok = send(Socket, {updates, lists:reverse(Q)}),
            {next_state, StateName, State#state{update_q=[]}, ?REPL_QUEUE_TIMEOUT};
        false ->
            {next_state, StateName, enqueue_update(State, Obj), ?REPL_QUEUE_TIMEOUT}
    end.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    R = ?MODULE:StateName(binary_to_term(Data), State),
    inet:setopts(Socket, [{active, once}]),
    R;
handle_info({_ReqId, _Status}, StateName, State) ->
    {next_state, StateName, write_next(State)}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, term_to_binary(Data, [compressed])).

enqueue_update(State=#state{update_q=Q}, Obj) -> State#state{update_q=[Obj|Q]}.
queue_full(#state{update_q=Q}) -> length(Q) >= 100.

should_get(V1, V2) -> vclock:descends(V1, V2) =:= false.
should_send(V1, V2) -> vclock:descends(V2, V1) =:= false.

vnode_master_call(Node, Message) ->
    gen_server:call({riak_vnode_master, Node}, Message, ?REPL_MERK_TIMEOUT).

vclock_diff(Partition, DiffVClocks, #state{my_peer_info=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    Keys = [K || {K, _V} <- DiffVClocks],
    case vnode_master_call(OwnerNode, {get_vclocks, Partition, Keys}) of
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
                          
diff_merkle(Partition, MerkleTree, #state{my_peer_info=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    case gen_server:call({riak_vnode_master, OwnerNode}, {get_merkle, Partition}, ?REPL_MERK_TIMEOUT) of
        {error, Reason} ->
            io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            [];
        OurMerkleTree ->
            DiffKeys = merkerl:diff(MerkleTree, OurMerkleTree),
            case gen_server:call({riak_vnode_master, OwnerNode}, 
                                 {get_vclocks, Partition, DiffKeys}, ?REPL_MERK_TIMEOUT) of
                {error, Reason} ->
                    io:format("~p:error getting vclock list for ~p from ~p: ~p~n",
                              [?MODULE, Partition, OwnerNode, Reason]),
                    [];
                VClocks ->
                    VClocks
            end
    end.

send_next_merkle(State=#state{merk_q=[]}) -> State;
send_next_merkle(State=#state{merk_q=[H|T],socket=Socket,my_peer_info=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, H),
    case gen_server:call({riak_vnode_master, OwnerNode}, {get_merkle, H}, ?REPL_MERK_TIMEOUT) of
        {error, Reason} ->
            io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
                      [?MODULE, H, OwnerNode, Reason]);
        undefined ->
            send_next_merkle(State#state{merk_q=T});
        MerkleTree ->
            ok = send(Socket, {take_merkle, H, MerkleTree})
    end,
    State#state{merk_q=T}.

write_next(State=#state{repl_q=[]}) -> State;
write_next(State=#state{repl_q=[H|T]}) ->
    riak_repl_fsm:start(erlang:phash2(erlang:now()), H, 1, 1, ?REPL_FSM_TIMEOUT, self()),
    State#state{repl_q=T}.

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
