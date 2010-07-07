%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_tcp_client).
-author('Andy Gross <andy@basho.com').
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
         merkle_exchange/2]).

-record(state, {
          socket :: port(), 
          sitename :: string(),
          connector_pid :: pid(),
          client :: tuple(),
          my_pi :: #peer_info{},
          partitions=[] :: list(),
          work_dir :: string(),
          merkle_pt :: non_neg_integer(),
          merkle_fp :: term(),
          merkle_fn :: string(),
          merkle_sz :: non_neg_integer()
         }).

start(Socket, SiteName, ConnectorPid) -> 
    gen_fsm:start(?MODULE, [Socket, SiteName, ConnectorPid], []).
    

init([Socket, SiteName, ConnectorPid]) ->
    %io:format("~p starting, sock=~p, site=~p, pid=~p~n", 
    %          [?MODULE, Socket, SiteName, self()]),
    ok = gen_tcp:send(Socket, SiteName),
    Props = riak_repl_fsm:common_init(Socket, SiteName),
    State = #state{
      socket=Socket, 
      sitename=SiteName,
      connector_pid=ConnectorPid,
      work_dir=proplists:get_value(work_dir, Props),
      client=proplists:get_value(client, Props),
      my_pi=proplists:get_value(my_pi, Props),
      partitions=proplists:get_value(partitions, Props)},
    ok = send(Socket, {peerinfo, State#state.my_pi}),
    {ok, wait_peerinfo, State}.

wait_peerinfo({redirect, IP, Port}, State=#state{connector_pid=P}) ->
    P ! {redirect, IP, Port},
    {stop, normal, State};
wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo, 
                                                      sitename=SiteName}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            update_site_ips(riak_repl_ring:get_repl_config(
                              TheirPeerInfo#peer_info.ring), SiteName),
            {next_state, merkle_exchange, State};
        false ->
            error_logger:error_msg("invalid peer_info ~p~n",[TheirPeerInfo]),
            {stop, normal, State}
    end;
wait_peerinfo({diff_obj, Obj}, State) ->
    riak_repl_util:do_repl_put(Obj),
    {next_state, wait_peerinfo, State}.
merkle_exchange({merkle,Size,Partition},State=#state{work_dir=WorkDir}) ->
    MerkleFN = filename:join(WorkDir,integer_to_list(Partition)++".theirs"),
    {ok, FP} = file:open(MerkleFN, [write, raw, binary, delayed_write]),
    {next_state, merkle_recv, State#state{merkle_fp=FP, 
                                          merkle_fn=MerkleFN,
                                          merkle_sz=Size, 
                                          merkle_pt=Partition}};
merkle_exchange({partition_complete,_Partition}, State) ->
    {next_state, merkle_exchange, State};
merkle_exchange({diff_obj, Obj}, State) ->
    riak_repl_util:do_repl_put(Obj),
    {next_state, merkle_exchange, State}.    

merkle_recv({diff_obj, Obj}, State) ->
    riak_repl_util:do_repl_put(Obj),
    {next_state, merkle_recv, State};   
merkle_recv({merk_chunk, Data}, State=#state{merkle_fp=FP, merkle_sz=SZ, 
                                             merkle_fn=FN, merkle_pt=PT,
                                             work_dir=WorkDir, 
                                             socket=Socket}) ->
    ok = file:write(FP, Data),
    LeftBytes = SZ - size(Data),
    case LeftBytes of
        0 ->
            ok = file:sync(FP),
            ok = file:close(FP),
            case riak_repl_util:make_merkle(PT, WorkDir) of
                {error, _} ->
                    ok = send(Socket, {ack, PT, []}),
                    {next_state, merkle_exchange, State};                    
                {ok, MerkleFN, OurMerkle, _OurRoot} ->
                    {ok, TheirMerkle} = couch_merkle:open(FN),
                    MerkleDiff = couch_merkle:diff(TheirMerkle, OurMerkle),
                    [couch_merkle:close(M) || M <- [OurMerkle, TheirMerkle]],
                    [file:delete(F) || F <- [MerkleFN, FN]],
                    case MerkleDiff of
                        [] -> 
                            ok = send(Socket, {ack, PT, []}),
                            {next_state, merkle_exchange, State};
                        DiffKeys0 ->  
                            DiffKeys = [riak_repl_util:binunpack_bkey(K) || 
                                           {K,_} <- DiffKeys0],
                            case riak_repl_fsm:get_vclocks(PT, DiffKeys) of
                                {error, node_not_available} ->
                                    ok = send(Socket, {ack, PT, []});
                                {error, Reason} ->
                                    error_logger:error_msg(
                                      "~p:getting vclocks for ~p: ~p~n",
                                      [?MODULE, PT, Reason]),
                                    ok = send(Socket, {ack, PT, []});
                                VClocks -> ok = send(Socket, {ack, PT, VClocks})
                            end,
                            {next_state, merkle_exchange, State}
                    end
            end;
        _ ->
            {next_state, merkle_recv, State#state{merkle_sz=LeftBytes}}
    end.

handle_info({tcp_closed, _Socket}, _StateName, State) -> {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    R = ?MODULE:StateName(binary_to_term(zlib:unzip(Data)), State),
    ok = inet:setopts(Socket, [{active, once}]),            
    riak_repl_stats:increment_counter(bytes_recvd, size(Data)),
    R;
%% no-ops
handle_info(_I, StateName, State) ->  {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->  ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Ev, _F, StateName, State) -> {reply, ok, StateName, State}.

send(Socket, Data) when is_binary(Data) -> 
    Msg = zlib:zip(Data),
    R = gen_tcp:send(Socket, Msg),
    riak_repl_stats:increment_counter(bytes_sent, size(Msg)),
    R;
send(Socket, Data) ->
    Msg = zlib:zip(term_to_binary(Data)),
    R = gen_tcp:send(Socket, Msg),
    riak_repl_stats:increment_counter(bytes_sent, size(Msg)),
    R.

update_site_ips(TheirReplConfig, SiteName) ->
    {ok, OurRing} = riak_core_ring_manager:get_my_ring(),
    MyListeners = dict:fetch(listeners, riak_repl_ring:get_repl_config(OurRing)),
    MyIPAddrs = sets:from_list([R#repl_listener.listen_addr || R <- MyListeners]),
    TheirListeners = dict:fetch(listeners, TheirReplConfig),
    TheirIPAddrs = sets:from_list([R#repl_listener.listen_addr || R <- TheirListeners]),
    ToRemove = sets:subtract(MyIPAddrs, TheirIPAddrs),
    ToAdd = sets:subtract(TheirIPAddrs, MyIPAddrs),
    OurRing1 = lists:foldl(fun(E,A) -> riak_repl_ring:del_site_addr(A, SiteName, E) end,
                           OurRing, sets:to_list(ToRemove)),
    OurRing2 = lists:foldl(fun(E,A) -> riak_repl_ring:add_site_addr(A, SiteName, E) end,
                           OurRing1, sets:to_list(ToAdd)),
    MyNewRC = riak_repl_ring:get_repl_config(OurRing2),
    F = fun(InRing, ReplConfig) ->
                {new_ring, riak_repl_ring:set_repl_config(InRing, ReplConfig)}
        end,
    riak_core_ring_manager:ring_trans(F, MyNewRC),
    ok.    


    
