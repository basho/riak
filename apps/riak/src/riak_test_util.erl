-module(riak_test_util).
-export([standard_backend_test/2,setup_mockring1/0]).
-include_lib("eunit/include/eunit.hrl").

standard_backend_test(BackendMod, Config) ->
    {ok, S} = BackendMod:start(42, Config),
    ?assertEqual(ok, BackendMod:put(S,{<<"b1">>,<<"k1">>},<<"v1">>)),
    ?assertEqual(ok, BackendMod:put(S,{<<"b2">>,<<"k2">>},<<"v2">>)),
    ?assertEqual({ok,<<"v2">>}, BackendMod:get(S,{<<"b2">>,<<"k2">>})),
    ?assertEqual({error, notfound}, BackendMod:get(S, {<<"b1">>,<<"k3">>})),
    ?assertEqual([{<<"b1">>,<<"k1">>},{<<"b2">>,<<"k2">>}],
                 lists:sort(BackendMod:list(S))),
    ?assertEqual([<<"k2">>], BackendMod:list_bucket(S, <<"b2">>)),
    ?assertEqual([<<"k1">>], BackendMod:list_bucket(S, <<"b1">>)),
    ?assertEqual([<<"k1">>], BackendMod:list_bucket(
                               S, {filter, <<"b1">>, fun(_K) -> true end})),
    ?assertEqual([], BackendMod:list_bucket(
                       S, {filter, <<"b1">>, fun(_K) -> false end})),
    BucketList = BackendMod:list_bucket(S, '_'),
    ?assert(lists:member(<<"b1">>, BucketList)),
    ?assert(lists:member(<<"b2">>, BucketList)),
    ?assertEqual(ok, BackendMod:delete(S,{<<"b2">>,<<"k2">>})),
    ?assertEqual({error, notfound}, BackendMod:get(S, {<<"b2">>, <<"k2">>})),
    ?assertEqual([{<<"b1">>, <<"k1">>}], BackendMod:list(S)),
    Folder = fun(K, V, A) -> [{K,V}|A] end,
    case BackendMod of
        riak_multi_backend ->
            ?assertEqual([{{<<"b1">>,<<"k1">>},<<"v1">>},
                          {{<<"b1">>,<<"k1">>},<<"v1">>}], BackendMod:fold(S, Folder, []));
        _ ->
            ?assertEqual([{{<<"b1">>,<<"k1">>},<<"v1">>}], BackendMod:fold(S, Folder, []))
    end,
    ?assertEqual(false, BackendMod:is_empty(S)),
    ?assertEqual(ok, BackendMod:delete(S,{<<"b1">>,<<"k1">>})),
    ?assertEqual(true, BackendMod:is_empty(S)),
    ok = BackendMod:stop(S).

setup_mockring1() ->
    % requires a running riak_ring_manager, in test-mode is ok
    Ring0 = lists:foldl(fun(_,R) ->
                               riak_ring:transfer_node(
                                 hd(riak_ring:my_indices(R)),
                                 othernode@otherhost, R) end,
                       riak_ring:fresh(16,node()),[1,2,3,4,5,6]),
    Ring = lists:foldl(fun(_,R) ->
                               riak_ring:transfer_node(
                                 hd(riak_ring:my_indices(R)),
                                 othernode2@otherhost2, R) end,
                       Ring0,[1,2,3,4,5,6]),
    riak_ring_manager:set_my_ring(Ring).
