-module(riak_repl_tcp_client).
-include("riak_repl.hrl").
-behaviour(gen_fsm).
-export([start/3]).
-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([wait_peerinfo/2,
         merkle_recv/2,
         diff_merkle/3,
         merkle_exchange/2]).

-record(state, {
          socket,
          sitename,
          connector_pid,
          client,
          my_pi,
          work_dir,
          merkle_pt,
          merkle_fp,
          merkle_fn,
          merkle_sz
         }).

start(Socket, SiteName, ConnectorPid) -> 
    gen_fsm:start(?MODULE, [Socket, SiteName, ConnectorPid], []).

init([Socket, SiteName, ConnectorPid]) ->
    process_flag(trap_exit, true),
    io:format("~p starting, sock=~p, site=~p, pid=~p~n", [?MODULE, Socket, SiteName, self()]),
    ok = gen_tcp:send(Socket, SiteName),
    ok = inet:setopts(Socket, [{active, once}, {packet, 4}]),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    {ok, WorkRoot} = application:get_env(riak_repl, work_dir),
    WorkDir = filename:join(WorkRoot, SiteName),
    ok = filelib:ensure_dir(filename:join(WorkDir, "empty")),
    ok = send(Socket, term_to_binary({peerinfo, MyPeerInfo})),
    {ok, wait_peerinfo, #state{socket=Socket,
                               work_dir=WorkDir,
                               sitename=SiteName,
                               connector_pid=ConnectorPid,
                               client=Client,
                               my_pi=MyPeerInfo}}.

wait_peerinfo({redirect, IP, Port}, State=#state{connector_pid=P}) ->
    P ! {redirect, IP, Port},
    {stop, normal, State};
wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo, 
                                                      socket=_Socket,
                                                      sitename=SiteName}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            PIPath = filename:join([riak_repl_util:site_root_dir(SiteName), "ring"]),
            ok = file:write_file(PIPath, term_to_binary(TheirPeerInfo)),
            {next_state, merkle_exchange, State};
        false ->
            io:format("invalid peer_info ~p~n", [TheirPeerInfo]),
            {stop, normal, State}
    end.
merkle_exchange({merkle, FileSize, Partition}, State=#state{socket=_Socket, 
                                                            work_dir=WorkDir}) ->
    MerkleFN0 = integer_to_list(Partition) ++ ".theirs",
    MerkleFN = filename:join(WorkDir, MerkleFN0),
    {ok, FP} = file:open(MerkleFN, [write, raw, binary]),
    {next_state, merkle_recv, State#state{merkle_fp=FP, merkle_fn=MerkleFN,
                                          merkle_sz=FileSize, 
                                          merkle_pt=Partition}};
merkle_exchange({diff_response,Partition,{send, Sends}}, State=#state{socket=Socket}) ->
    io:format("got diff_response with ~p sendreqs~n", [length(Sends)]),
    ok = send(Socket, term_to_binary({ack, Partition, []})),
    {next_state, merkle_exchange, State}.

merkle_recv({merk_chunk, Data}, State=#state{merkle_fp=FP, 
                                             merkle_sz=SZ, 
                                             merkle_fn=FN,
                                             merkle_pt=PT,
                                             work_dir=WorkDir,
                                             socket=Socket,
                                             my_pi=PI}) ->
    ok = file:write(FP, Data),
    LeftBytes = SZ - size(Data),
    case LeftBytes of
        0 ->
            ok = file:sync(FP),
            ok = file:close(FP),
            {ok, MerkleFN} = riak_repl_util:make_merkle(PI, PT, WorkDir),
            {ok, OurMerkle} = couch_merkle:open(MerkleFN),
            {ok, TheirMerkle} = couch_merkle:open(FN),
            DiffKeys0 = couch_merkle:diff(TheirMerkle, OurMerkle),
            couch_merkle:close(OurMerkle),
            couch_merkle:close(TheirMerkle),
            file:delete(MerkleFN),
            file:delete(FN),
            DiffKeys = [riak_repl_util:binunpack_bkey(K) || {K,_} <- DiffKeys0],
            {ok, Ring} = riak_core_ring_manager:get_my_ring(),
            OwnerNode = riak_core_ring:index_owner(Ring, PT),
            case riak_repl_util:vnode_master_call(OwnerNode,
                                                  {get_vclocks, 
                                                   PT, DiffKeys}) of
                {error, Reason} ->
                    io:format("~p:error getting vclocks for ~p from ~p: ~p~n",
                              [?MODULE, PT, OwnerNode, Reason]),
                    ok = send(Socket, term_to_binary({ack, PT, []}));
                VClocks -> 
                    ok = send(Socket, 
                              term_to_binary({ack, PT, VClocks}, [compressed]))
            end,
            {next_state, merkle_exchange, State};
        _ ->
            {next_state, merkle_recv, State#state{merkle_sz=LeftBytes}}
    end.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(_Ev, _F, StateName, State) -> {reply, ok, StateName, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    io:format("tcp socket closed ~p~n", [Socket]),
    {stop, normal, State};

handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    case binary_to_term(Data) of
        {diff_obj, Obj} ->
            riak_repl_util:do_repl_put(Obj),
            %io:format("wrote remote update~n"),
            ok = inet:setopts(Socket, [{active, once}]),            
            {next_state, StateName, State};            
        Msg ->
            R = ?MODULE:StateName(Msg, State),
            ok = inet:setopts(Socket, [{active, once}]),            
            R
    end;
handle_info(_I, StateName, State) ->  {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->  ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

send(Socket, Data) ->  ok = gen_tcp:send(Socket, Data).

diff_merkle(Partition, MerkleTree, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, Reason} ->
            io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            [];
        {ok, OurMerkleTree} ->
            MT = binary_to_term(OurMerkleTree),
            DiffKeys = merkerl:diff(MerkleTree, MT),
            case riak_repl_util:vnode_master_call(OwnerNode,
                                                  {get_vclocks, Partition, DiffKeys}) of
                {error, Reason} ->
                    io:format("~p:error getting vclock list for ~p from ~p: ~p~n",
                    [?MODULE, Partition, OwnerNode, Reason]),
                    [];
                VClocks -> VClocks
            end
    end.
