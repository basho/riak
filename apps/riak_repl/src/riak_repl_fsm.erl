%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_fsm).
-author('Andy Gross <andy@basho.com').
-include("riak_repl.hrl").
-export([common_init/2,
         get_vclocks/2]).

common_init(Socket, SiteName) ->
    process_flag(trap_exit, true),        
    ok = inet:setopts(Socket, ?FSM_SOCKOPTS),
    {ok, Client} = riak:local_client(),
    PI = riak_repl_util:make_peer_info(),
    Partitions = riak_repl_util:get_partitions(PI#peer_info.ring),    
    {ok, WorkRoot} = application:get_env(riak_repl, work_dir),
    WorkDir = filename:join(WorkRoot, SiteName),
    ok = filelib:ensure_dir(filename:join(WorkDir, "empty")),
    [{client, Client},
     {partitions, Partitions},
     {work_dir, WorkDir},
     {my_pi, PI}].

get_vclocks(Partition, KeyList) 
  when is_integer(Partition) andalso is_list(KeyList) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    case lists:member(OwnerNode, riak_core_node_watcher:nodes(riak_kv)) of
        true ->
            riak_kv_vnode:get_vclocks({Partition, OwnerNode}, KeyList);
        false ->
            {error, node_not_available}
    end.
    
