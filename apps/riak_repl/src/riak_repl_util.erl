%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_util).
-author('Andy Gross <andy@basho.com>').
-include("riak_repl.hrl").
-include_lin("riak_core/include/riak_core_vnode.hrl").
-export([make_peer_info/0,
         validate_peer_info/2,
         get_partitions/1,
         do_repl_put/1,
         site_root_dir/1,
         ensure_site_dir/1,
         binpack_bkey/1,
         binunpack_bkey/1,
         make_merkle/2]).

make_peer_info() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    {ok, RiakVSN} = application:get_key(riak_kv, vsn),
    {ok, ReplVSN} = application:get_key(riak_repl, vsn),
    #peer_info{riak_version=RiakVSN, repl_version=ReplVSN, ring=Ring}.

validate_peer_info(T=#peer_info{}, M=#peer_info{}) ->
    TheirPartitions = get_partitions(T#peer_info.ring),
    OurPartitions = get_partitions(M#peer_info.ring),
    TheirPartitions =:= OurPartitions.

get_partitions(_Ring) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    lists:sort([P || {P, _} <- riak_core_ring:all_owners(Ring)]).

do_repl_put(Object) ->
    ReqId = erlang:phash2(erlang:now()),
    spawn(
      fun() ->
              riak_repl_put_fsm:start(ReqId,Object,1,1,?REPL_FSM_TIMEOUT,self())
      end).

site_root_dir(Site) ->
    {ok, DataRootDir} = application:get_env(riak_repl, data_root),
    SitesRootDir = filename:join([DataRootDir, "sites"]),
    filename:join([SitesRootDir, Site]).

ensure_site_dir(Site) ->
    ok = filelib:ensure_dir(
           filename:join([riak_repl_util:site_root_dir(Site), ".empty"])).

binpack_bkey({B, K}) ->
    SB = size(B),
    SK = size(K),
    <<SB:32/integer, B/binary, SK:32/integer, K/binary>>.

binunpack_bkey(<<SB:32/integer,B:SB/binary,SK:32/integer,K:SK/binary>>) -> 
    {B,K}.

make_merkle(Partition, Dir) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    case lists:member(OwnerNode, riak_core_node_watcher:nodes(riak_kv)) of
        true ->
            FileName = filename:join(Dir,integer_to_list(Partition)++".merkle"),
            {ok, DMerkle} = couch_merkle:open(FileName),
            MakerPid = spawn(fun() -> merkle_maker(DMerkle, [], 0) end),
            F = fun(K, V, MPid) -> MPid ! {K, erlang:phash2(V)}, MPid end,
            riak_kv_vnode:fold({Partition,OwnerNode}, F, MakerPid),
            MakerPid ! {finish, self()},
            receive ok -> ok end,
            {ok, FileName, DMerkle, couch_merkle:root(DMerkle)};
        false ->
            {error, node_not_available}
    end.

merkle_maker(DMerklePid, Buffer, Size) ->
    receive 
        {finish, Pid}  ->
            couch_merkle:update_many(DMerklePid, Buffer),
            Pid ! ok;
        {K, H} ->
            PackedKey = binpack_bkey(K),
            NewSize = Size+size(PackedKey)+4,
            NewBuf = [{PackedKey, H}|Buffer],
            case NewSize >= ?MERKLE_BUFSZ of 
                true ->
                    couch_merkle:update_many(DMerklePid, NewBuf),
                    merkle_maker(DMerklePid, [], 0);
                false ->
                    merkle_maker(DMerklePid, NewBuf, NewSize)
            end
    end.
            
            
