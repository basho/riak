-module(riak_test_util).
-export([standard_backend_test/1]).
-include_lib("eunit/include/eunit.hrl").

standard_backend_test(BackendMod) ->
    {ok, S} = BackendMod:start(42),
    ?assertEqual(ok, BackendMod:put(S,{b1,<<"k1">>},<<"v1">>)),
    ?assertEqual(ok, BackendMod:put(S,{b2,<<"k2">>},<<"v2">>)),
    ?assertEqual({ok,<<"v2">>}, BackendMod:get(S,{b2,<<"k2">>})),
    ?assertEqual({error, notfound}, BackendMod:get(S, {b1,<<"k3">>})),
    ?assertEqual([{b1,<<"k1">>},{b2,<<"k2">>}],
                 lists:sort(BackendMod:list(S))),
    ?assertEqual([<<"k2">>], BackendMod:list_bucket(S, b2)),
    ?assertEqual([<<"k1">>], BackendMod:list_bucket(S, b1)),
    ?assertEqual(ok, BackendMod:delete(S,{b2,<<"k2">>})),
    ?assertEqual({error, notfound}, BackendMod:get(S, {b2, <<"k2">>})),
    ?assertEqual([{b1, <<"k1">>}], BackendMod:list(S)),
    ok = BackendMod:stop(S).
