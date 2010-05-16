%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_tcp_server).
-author('Andy Gross <andy@basho.com').
-include("riak_repl.hrl").
-include_lib("kernel/include/file.hrl").
-behaviour(gen_fsm).
-export([start/2]).
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
          socket :: port(),
          sitename :: string(),
          client :: tuple(),
          my_pi :: #peer_info{},
          merkle_fp :: term(),
          work_dir :: string(),
          partitions=[] :: list()
         }
       ).

start(Socket, SiteName) -> gen_fsm:start(?MODULE, [Socket, SiteName], []).

init([Socket, SiteName]) ->
    process_flag(trap_exit, true),
    %%io:format("~p starting, sock=~p~n", [?MODULE, Socket]),
    ok = inet:setopts(Socket, [{active, once}, {packet, 4}]),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    Partitions = riak_repl_util:get_partitions(MyPeerInfo),
    {ok, WorkRoot} = application:get_env(riak_repl, work_dir),
    WorkDir = filename:join(WorkRoot, SiteName),
    ok = filelib:ensure_dir(filename:join(WorkDir, "empty")),
    case maybe_redirect(Socket, MyPeerInfo) of
        ok ->
            riak_repl_leader:add_receiver_pid(self()),
            {ok, wait_peerinfo, #state{socket=Socket,
                                       sitename=SiteName,
                                       client=Client,
                                       work_dir=WorkDir,
                                       partitions=Partitions,
                                       my_pi=MyPeerInfo}};
        redirect -> ignore
    end.

maybe_redirect(Socket, PeerInfo) ->
    case riak_repl_leader:leader_node() =:= node() of
        true -> ok = send(Socket, {peerinfo, PeerInfo});
        false -> send(Socket, {redirect, "127.0.0.1", 9011}), redirect
    end.

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true -> {next_state, merkle_send, State, 0};
        false -> {stop, normal, State}
    end.

merkle_send(timeout, State=#state{partitions=[], sitename=SiteName}) ->
    error_logger:info_msg("Fullsync with site ~p complete~n", [SiteName]),
    {next_state, connected, State};
merkle_send(timeout, State=#state{socket=Socket, 
                                  partitions=[Partition|T],
                                  work_dir=WorkDir}) ->
    case riak_repl_util:make_merkle(Partition, WorkDir) of
        {error, Reason} ->
            error_logger:error_msg("get_merkle error ~p for partition ~p~n", 
                                   [Reason, Partition]),
            {next_state, merkle_send, State, 0};
        {ok, MerkleFile, MerklePid, Root} ->
            couch_merkle:close(MerklePid),
            io:format("our root: ~p~n", [Root]),
            {ok, FileInfo} = file:read_file_info(MerkleFile),
            FileSize = FileInfo#file_info.size,
            {ok, FP} = file:open(MerkleFile, [read, raw, binary, read_ahead]),
            ok = send(Socket, {merkle, FileSize, Partition}),
            error_logger:info_msg("Synchronizing partition ~p~n", [Partition]),
            ok = send_chunks(FP, Socket),
            file:delete(MerkleFile),
            {next_state, merkle_wait_ack, State#state{partitions=T}}
    end.

send_chunks(FP, Socket) ->
    case file:read(FP, ?MERKLE_CHUNKSZ) of
        {ok, Data} ->
            ok = send(Socket, {merk_chunk, Data}),
            send_chunks(FP, Socket);
        eof -> ok = file:close(FP)
    end.

merkle_wait_ack({ack, _Partition, []}, State) ->
    {next_state, merkle_send, State, 0};
merkle_wait_ack({ack, Partition, DiffVClocks}, State=#state{socket=Socket}) ->
    vclock_diff(Partition, DiffVClocks, State),
    ok = send(Socket, {diff_response, Partition, {send, []}}),
    {next_state, merkle_wait_ack, State}.

connected(_E, State) -> {next_state, connected, State}.
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Event,_From,StateName,State) -> {reply,ok,StateName,State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    io:format("tcp socket closed ~p~n", [Socket]),
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    R = ?MODULE:StateName(binary_to_term(zlib:unzip(Data)), State),
    ok = inet:setopts(Socket, [{active, once}]),            
    R;
handle_info({repl, RObj}, StateName, State=#state{socket=Socket}) ->
    ok = send(Socket, {diff_obj, RObj}),
    {next_state, StateName, State};
handle_info(_I, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.


send(Socket, Data) when is_binary(Data) -> gen_tcp:send(Socket, zlib:zip(Data));
send(Socket, Data) -> gen_tcp:send(Socket, zlib:zip(term_to_binary(Data))).

vclock_diff(Partition, DiffVClocks, #state{client=Client, socket=Socket}) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    Keys = [K || {K, _V} <- DiffVClocks],
    case riak_repl_util:vnode_master_call(OwnerNode, 
                                          {get_vclocks, Partition, Keys}) of
        {error, Reason} ->
            error_logger:error_msg("~p:getting vclocks for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            [];
        OurVClocks ->
            vclock_diff1(DiffVClocks, OurVClocks, Client, Socket, 0)
    end.

vclock_diff1([],_,_,_,Count) -> Count;
vclock_diff1([{K,VC}|T], OurVClocks, Client, Socket, Count) ->
    case proplists:get_value(K, OurVClocks) of
        undefined -> vclock_diff1(T, OurVClocks, Client, Socket, Count);
        VC -> vclock_diff1(T, OurVClocks, Client, Socket, Count);
        OurVClock -> 
            maybe_send(K, OurVClock, VC, Client, Socket),
            vclock_diff1(T, OurVClocks, Client, Socket, Count+1)
    end.

maybe_send(BKey, V1, V2, Client, Socket) ->
    case vclock:descends(V2, V1) of true -> nop; false -> ok = do_send(BKey, Client, Socket) end.
            
do_send({B,K}, Client, Socket) ->
    case Client:get(B, K, 1, ?REPL_FSM_TIMEOUT) of
        {ok, Obj} -> ok = send(Socket, {diff_obj, Obj});
        _ -> ok
    end.
