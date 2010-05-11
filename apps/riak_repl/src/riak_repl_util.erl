-module(riak_repl_util).
-include("riak_repl.hrl").
-export([make_peer_info/0,
         vnode_master_call/2,
         validate_peer_info/2,
         get_partitions/1,
         wait_for_riak/1,
         do_repl_put/1,
         site_root_dir/1]).

make_peer_info() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    {ok, RiakVSN} = application:get_key(riak_kv, vsn),
    {ok, ReplVSN} = application:get_key(riak_repl, vsn),
    #peer_info{riak_version=RiakVSN,
               repl_version=ReplVSN,
               ring=Ring}.
    

validate_peer_info(T=#peer_info{}, M=#peer_info{}) ->
    TheirPartitions = get_partitions(T),
    OurPartitions = get_partitions(M),
    TheirPartitions =:= OurPartitions.

get_partitions(#peer_info{ring=Ring}) ->
    lists:sort([P || {P, _} <- riak_core_ring:all_owners(Ring)]).

vnode_master_call(Node, Message) ->
    gen_server:call({riak_kv_vnode_master, Node}, Message, ?REPL_MERK_TIMEOUT).

wait_for_riak(PPid) ->
    case [A || {A,_,_} <- application:which_applications(), A =:= riak_kv] of
        [] -> 
            timer:sleep(1000),
            wait_for_riak(PPid);
        _ -> PPid ! riak_up
    end.

do_repl_put(Object) ->
    ReqId = erlang:phash2(erlang:now()),
    riak_repl_fsm:start(ReqId, Object, 1, 1, ?REPL_FSM_TIMEOUT, self()),
    receive
        {ReqId, _} ->
            ok
    end.

site_root_dir(Site) ->
    {ok, DataRootDir} = application:get_env(riak_repl, data_root),
    SitesRootDir = filename:join([DataRootDir, "sites"]),
    filename:join([SitesRootDir, Site]).

