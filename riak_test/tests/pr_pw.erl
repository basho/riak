-module(pr_pw).

-behavior(riak_test).
-export([confirm/0]).

-include_lib("eunit/include/eunit.hrl").

confirm() ->
    application:start(inets),
    lager:info("Deploy some nodes"),
    Nodes = rt:build_cluster(4),

    %% calculate the preflist for foo/bar
    {ok, Ring} = rpc:call(hd(Nodes), riak_core_ring_manager, get_my_ring, []),
    UpNodes = rpc:call(hd(Nodes), riak_core_node_watcher, nodes, [riak_kv]),
    DocIdx = rpc:call(hd(Nodes), riak_core_util, chash_key, [{<<"foo">>,
                <<"bar">>}]),
    N = 3,
    Preflist2 = riak_core_apl:get_apl_ann(DocIdx, N, Ring, UpNodes),
    lager:info("Preflist is ~p", [Preflist2]),
    PLNodes = [Node || {{_Index, Node}, _Status} <- Preflist2],
    lager:info("Nodes in preflist ~p", [PLNodes]),
    [SafeNode] = Nodes -- PLNodes,
    lager:info("Node not involved in this preflist ~p", [SafeNode]),
    %% connect to the only node in the preflist we won't break, to avoid
    %% random put forwarding
    {ok, C} = riak:client_connect(hd(PLNodes)),
    NodeUrl = rt:http_url(hd(PLNodes)),
    UrlFun=fun(Key, Value, Params) ->
            lists:flatten(io_lib:format("~s/riak/~s/~s~s",
                    [NodeUrl, Key, Value, Params]))
    end,

    Obj = riak_object:new(<<"foo">>, <<"bar">>, <<42:32/integer>>),
    ?assertEqual(ok, C:put(Obj, [{pw, all}])),
    ?assertMatch({ok, _}, C:get(<<"foo">>, <<"bar">>, [{pr, all}])),

    %% check pr/pw can't be violated
    ?assertEqual({error, {pw_val_violation, evil}}, C:put(Obj, [{pw, evil}])),
    ?assertEqual({error, {pr_val_violation, evil}}, C:get(<<"foo">>, <<"bar">>,
            [{pr, evil}])),

    ?assertMatch({ok, {{_, 400, _}, _, "pr query parameter must be"++_}},
        httpc:request(get, {UrlFun(<<"foo">>, <<"bar">>, <<"?pr=evil">>), []}, [], [])),

    ?assertMatch({ok, {{_, 400, _}, _, "pw query parameter must be"++_}},
        httpc:request(put, {UrlFun(<<"foo">>, <<"bar">>,
                    <<"?pw=evil">>), [], "text/plain", <<42:32/integer>>}, [], [])),

    %% install an intercept to emulate a node that kernel paniced or
    %% something where it can take some time for the node_watcher to spot the
    %% downed node
    {{Index, Node}, _} = lists:last(Preflist2),
    make_intercepts_tab(Node, Index),
    rt_intercept:add(Node, {riak_kv_vnode,  [{{do_get,4}, drop_do_get},
                {{do_put, 7}, drop_do_put}]}),
    lager:info("disabling do_get for index ~p on ~p", [Index, Node]),
    rt:log_to_nodes(Nodes, "disabling do_get for index ~p on ~p", [Index, Node]),
    timer:sleep(100),

    %% one vnode will never return, so we get timeouts
    ?assertEqual({error, timeout},
        C:get(<<"foo">>, <<"bar">>, [{pr, all}])),
    ?assertEqual({error, timeout}, C:put(Obj, [{pw, all}])),

    %% we can still meet quorum, though
    ?assertEqual(ok, C:put(Obj, [{pw, quorum}])),
    ?assertMatch({ok, _},
        C:get(<<"foo">>, <<"bar">>, [{pr, quorum}])),

    rt:stop_and_wait(Node),

    %% there's now a fallback in the preflist, so PR/PW won't be satisfied
    %% anymore
    ?assertEqual({error, {pr_val_unsatisfied, 3, 2}},
        C:get(<<"foo">>, <<"bar">>, [{pr, all}])),
    ?assertEqual({error, {pw_val_unsatisfied, 3, 2}}, C:put(Obj, [{pw, all}])),

    ?assertMatch({ok, {{_, 503, _}, _, "PR-value unsatisfied: 2/3\n"}},
        httpc:request(get, {UrlFun(<<"foo">>, <<"bar">>, <<"?pr=all">>), []}, [], [])),

    ?assertMatch({ok, {{_, 503, _}, _, "PW-value unsatisfied: 2/3\n"}},
        httpc:request(put, {UrlFun(<<"foo">>, <<"bar">>,
                    <<"?pw=all">>), [], "text/plain", <<42:32/integer>>}, [], [])),

    %% emulate another node failure
    {{Index2, Node2}, _} = lists:nth(2, Preflist2),
    make_intercepts_tab(Node2, Index2),
    rt_intercept:add(Node2, {riak_kv_vnode,  [{{do_get,4}, drop_do_get},
                {{do_put, 7}, drop_do_put}]}),
    lager:info("disabling do_get for index ~p on ~p", [Index2, Node2]),
    rt:log_to_nodes(Nodes, "disabling do_get for index ~p on ~p", [Index2, Node2]),
    timer:sleep(100),

    %% can't even meet quorum now
    ?assertEqual({error, timeout},
        C:get(<<"foo">>, <<"bar">>, [{pr, quorum}])),
    ?assertEqual({error, timeout}, C:put(Obj, [{pw, quorum}])),

    %% restart the node
    rt:start_and_wait(Node),
    rt:wait_for_service(Node, riak_kv),

    %% we can make quorum again
    ?assertEqual(ok, C:put(Obj, [{pw, quorum}])),
    ?assertMatch({ok, _}, C:get(<<"foo">>, <<"bar">>, [{pr, quorum}])),
    %% intercepts still in force on second node, so we'll get timeouts
    ?assertEqual({error, timeout},
        C:get(<<"foo">>, <<"bar">>, [{pr, all}])),
    ?assertEqual({error, timeout}, C:put(Obj, [{pw, all}])),

    %% reboot the node
    rt:stop_and_wait(Node2),
    rt:start_and_wait(Node2),
    rt:wait_for_service(Node2, riak_kv),

    %% everything is happy again
    ?assertEqual(ok, C:put(Obj, [{pw, all}])),
    ?assertMatch({ok, _}, C:get(<<"foo">>, <<"bar">>, [{pr, all}])),

    %% make a vnode start to fail puts
    make_intercepts_tab(Node2, Index2),
    rt_intercept:add(Node2, {riak_kv_vnode,  [{{do_put, 7}, error_do_put}]}),
    lager:info("failing do_put for index ~p on ~p", [Index2, Node2]),
    rt:log_to_nodes(Nodes, "failing do_put for index ~p on ~p", [Index2, Node2]),
    timer:sleep(100),

    %% there's now a failing vnode in the preflist, so PW/DW won't be satisfied
    %% anymore
    ?assertEqual({error, {pw_val_unsatisfied, 3, 2}}, C:put(Obj, [{pw, all}])),
    ?assertEqual({error, {dw_val_unsatisfied, 3, 2}}, C:put(Obj, [{dw, all}])),

    ?assertMatch({ok, {{_, 503, _}, _, "PW-value unsatisfied: 2/3\n"}},
        httpc:request(put, {UrlFun(<<"foo">>, <<"bar">>,
                    <<"?pw=all">>), [], "text/plain", <<42:32/integer>>}, [], [])),
    ?assertMatch({ok, {{_, 503, _}, _, "DW-value unsatisfied: 2/3\n"}},
        httpc:request(put, {UrlFun(<<"foo">>, <<"bar">>,
                    <<"?dw=all">>), [], "text/plain", <<42:32/integer>>}, [], [])),
    pass.

make_intercepts_tab(Node, Partition) ->
    SupPid = rpc:call(Node, erlang, whereis, [sasl_safe_sup]),
    intercepts_tab = rpc:call(Node, ets, new, [intercepts_tab, [named_table,
                public, set, {heir, SupPid, {}}]]),
    true = rpc:call(Node, ets, insert, [intercepts_tab, {drop_do_get_partitions,
                [Partition]}]),
    true = rpc:call(Node, ets, insert, [intercepts_tab, {drop_do_put_partitions,
                [Partition]}]).
