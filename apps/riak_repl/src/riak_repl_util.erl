%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_util).
-author('Andy Gross <andy@basho.com>').
-include("riak_repl.hrl").
-export([make_peer_info/0,
         vnode_master_call/2,
         validate_peer_info/2,
         get_partitions/1,
         wait_for_riak/1,
         do_repl_put/1,
         site_root_dir/1,
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

get_partitions(Ring) ->
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
    spawn(
      fun() ->
              riak_repl_fsm:start(ReqId,Object,1,1,?REPL_FSM_TIMEOUT,self())
      end).

site_root_dir(Site) ->
    {ok, DataRootDir} = application:get_env(riak_repl, data_root),
    SitesRootDir = filename:join([DataRootDir, "sites"]),
    filename:join([SitesRootDir, Site]).

binpack_bkey({B, K}) ->
    SB = size(B),
    SK = size(K),
    <<SB:32/integer, B/binary, SK:32/integer, K/binary>>.

binunpack_bkey(<<SB:32/integer,B:SB/binary,SK:32/integer,K:SK/binary>>) -> 
    {B,K}.

make_merkle(Partition, Dir) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    FileName = filename:join(Dir, integer_to_list(Partition) ++ ".merkle"),
    {ok, DMerkle} = couch_merkle:open(FileName),
    MakerPid = spawn(fun() -> merkle_maker(DMerkle, [], 0) end),
    F = fun(K, V, MPid) -> MPid ! {K, erlang:phash2(V)}, MPid end,
    riak_repl_util:vnode_master_call(OwnerNode, {fold,{Partition,F,MakerPid}}),
    MakerPid ! {finish, self()},
    receive 
        {ok, RestKeys} -> couch_merkle:update_many(DMerkle, RestKeys)
    end,
    {ok, FileName, DMerkle, couch_merkle:root(DMerkle)}.

merkle_maker(DMerklePid, Buffer, Size) ->
    receive 
        {finish, Pid} ->
            Pid ! {ok, Buffer};
        {K, H} ->
            PackedKey = binpack_bkey(K),
            NewSize = Size+size(PackedKey)+4,
            case NewSize >= ?MERKLE_BUFSZ of 
                true ->
                    couch_merkle:update_many(DMerklePid, Buffer),
                    merkle_maker(DMerklePid, [], 0);
                false ->
                    merkle_maker(DMerklePid, [{PackedKey, H}|Buffer], NewSize)
            end
    end.
            
            
