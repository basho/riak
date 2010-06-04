%% Riak EnterpriseDS
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
          socket :: repl_socket(),       %% peer socket
          sitename :: repl_sitename(),   %% repl site identifier
          client :: tuple(),      %% local riak client
          my_pi :: #peer_info{},  %% peer info record 
          merkle_fp :: term(),    %% current merkle filedesc
          work_dir :: string(),   %% working directory for this repl session
          partitions=[] :: list(),%% list of local partitions
          merkle_wip=[] :: list(),%% merkle work in progress
          fullsync_ival :: undefined|non_neg_integer()
         }
       ).

start(Socket, SiteName) -> gen_fsm:start(?MODULE, [Socket, SiteName], []).

init([Socket, SiteName]) ->
    %io:format("~p starting, sock=~p, sitename=~p~n", 
    %          [?MODULE, Socket, SiteName]),
    Props = riak_repl_fsm:common_init(Socket, SiteName),
    State = #state{
      socket=Socket,
      sitename=SiteName,
      client=proplists:get_value(client, Props),
      my_pi=proplists:get_value(my_pi, Props),
      work_dir=proplists:get_value(work_dir,Props),
      partitions=proplists:get_value(partitions, Props)},
    case maybe_redirect(Socket,  State#state.my_pi) of
        ok ->
            riak_repl_leader:add_receiver_pid(self()),
            {ok, wait_peerinfo, State};
        redirect -> ignore
    end.

maybe_redirect(Socket, PeerInfo) ->
    OurNode = node(),
    case riak_repl_leader:leader_node()  of
        OurNode -> ok = send(Socket, {peerinfo, PeerInfo});
        OtherNode -> 
            OtherListener = listener_for_node(OtherNode),
            {Ip, Port} = OtherListener#repl_listener.listen_addr,
            send(Socket, {redirect, Ip, Port}),
            redirect
    end.

listener_for_node(Node) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    ReplConfig = riak_repl_ring:get_repl_config(Ring),
    Listeners = dict:fetch(listeners, ReplConfig),
    NodeListeners = [L || L <- Listeners,
                          L#repl_listener.nodename =:= Node],
    hd(NodeListeners).

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true -> {next_state, merkle_send, State, 0};
        false -> {stop, normal, State}
    end.

merkle_send(timeout, State=#state{partitions=[], 
                                  sitename=SiteName}) ->
    error_logger:info_msg("Full-sync with ~p complete~n", [SiteName]),
    case State#state.fullsync_ival of
        undefined ->
            {ok, FSI} = application:get_env(riak_repl, fullsync_interval),
            erlang:send_after(timer:minutes(FSI), self(), fullsync),
            {next_state, connected, State#state{fullsync_ival=FSI}};
        _ ->
            erlang:send_after(timer:minutes(State#state.fullsync_ival), self(), fullsync),
            {next_state, connected, State}
    end;
merkle_send(timeout, State=#state{socket=Socket, 
                                  sitename=SiteName,
                                  partitions=[Partition|T],
                                  work_dir=WorkDir}) ->
    case riak_repl_util:make_merkle(Partition, WorkDir) of
        {error, node_not_available} ->
            {next_state, merkle_send, State#state{partitions=T}, 0};            
        {error, Reason} ->
            error_logger:error_msg("get_merkle error ~p for partition ~p~n", 
                                   [Reason, Partition]),
            {next_state, merkle_send, State#state{partitions=T}, 0};

        {ok, MerkleFile, MerklePid, _Root} ->
            couch_merkle:close(MerklePid),
            {ok, FileInfo} = file:read_file_info(MerkleFile),
            FileSize = FileInfo#file_info.size,
            {ok, FP} = file:open(MerkleFile, [read,raw,binary,read_ahead]),
            ok = send(Socket, {merkle, FileSize, Partition}),
            error_logger:info_msg("Syncing partition ~p with site ~p~n",
                                  [Partition, SiteName]),
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
merkle_wait_ack({ack,Partition,DiffVClocks}, State=#state{socket=Socket}) ->
    vclock_diff(Partition, DiffVClocks, State),
    ok = send(Socket, {partition_complete, Partition}),
    {next_state, merkle_send, State, 0}.

connected(_E, State) -> {next_state, connected, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    R = ?MODULE:StateName(binary_to_term(zlib:unzip(Data)), State),
    ok = inet:setopts(Socket, [{active, once}]),            
    riak_repl_stats:increment_counter(bytes_recvd, size(Data)),
    R;
handle_info({repl, RObj}, StateName, State=#state{socket=Socket}) ->
    ok = send(Socket, {diff_obj, RObj}),
    {next_state, StateName, State};
handle_info(fullsync, connected, State) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Partitions = riak_repl_util:get_partitions(Ring),
    {next_state, merkle_send, State#state{partitions=Partitions}, 0};
%% no-ops
handle_info(_I, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
handle_event(_E, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_E,_F,StateName,State) -> {reply,ok,StateName,State}.

send(Sock,Data) when is_binary(Data) -> 
    Msg = zlib:zip(Data),
    R = gen_tcp:send(Sock,Msg),
    riak_repl_stats:increment_counter(bytes_sent, size(Msg)),
    R;
send(Sock,Data) -> 
    Msg = zlib:zip(term_to_binary(Data)),
    R = gen_tcp:send(Sock, Msg),
    riak_repl_stats:increment_counter(bytes_sent, size(Msg)),
    R.
vclock_diff(Partition, DiffVClocks, #state{client=Client, socket=Socket}) ->
    Keys = [K || {K, _V} <- DiffVClocks],
    case riak_repl_fsm:get_vclocks(Partition, Keys) of
        {error, node_not_available} ->
            [];
        {error, Reason} ->
            error_logger:error_msg("~p:getting vclocks for ~p:~p~n",
                                   [?MODULE, Partition, Reason]),
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
    case vclock:descends(V2, V1) of 
        true -> nop; 
        false -> ok = do_send(BKey, Client, Socket) end.
            
do_send({B,K}, Client, Socket) ->
    case Client:get(B, K, 1, ?REPL_FSM_TIMEOUT) of
        {ok, Obj} -> ok = send(Socket, {diff_obj, Obj});
        _ -> ok
    end.
