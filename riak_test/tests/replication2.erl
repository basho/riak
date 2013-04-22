-module(replication2).
-behavior(riak_test).
-export([confirm/0, replication/3]).
-include_lib("eunit/include/eunit.hrl").

-import(rt, [deploy_nodes/2,
             join/2,
             log_to_nodes/2,
             log_to_nodes/3,
             wait_until_nodes_ready/1,
             wait_until_no_pending_changes/1]).

confirm() ->
    NumNodes = rt:config(num_nodes, 6),
    ClusterASize = rt:config(cluster_a_size, 3),

    lager:info("Deploy ~p nodes", [NumNodes]),
    Conf = [
            {riak_kv,
                [
                    {anti_entropy, {off, []}}
                ]
            },
            {riak_repl,
             [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled},
                {diff_batch_size, 10}
             ]}
    ],

    Nodes = deploy_nodes(NumNodes, Conf),

    {ANodes, BNodes} = lists:split(ClusterASize, Nodes),
    lager:info("ANodes: ~p", [ANodes]),
    lager:info("BNodes: ~p", [BNodes]),

    lager:info("Build cluster A"),
    repl_util:make_cluster(ANodes),

    lager:info("Build cluster B"),
    repl_util:make_cluster(BNodes),

    replication(ANodes, BNodes, false),
    pass.

replication([AFirst|_] = ANodes, [BFirst|_] = BNodes, Connected) ->

    AllNodes = ANodes ++ BNodes,
    log_to_nodes(AllNodes, "Starting replication2 test"),

    TestHash =  list_to_binary([io_lib:format("~2.16.0b", [X]) ||
                <<X>> <= erlang:md5(term_to_binary(os:timestamp()))]),
    TestBucket = <<TestHash/binary, "-systest_a">>,
    FullsyncOnly = <<TestHash/binary, "-fullsync_only">>,
    RealtimeOnly = <<TestHash/binary, "-realtime_only">>,
    NoRepl = <<TestHash/binary, "-no_repl">>,

    case Connected of
        false ->
            %% clusters are not connected, connect them

            %% write some initial data to A
            lager:info("Writing 100 keys to ~p", [AFirst]),
            ?assertEqual([], repl_util:do_write(AFirst, 1, 100, TestBucket, 2)),

            repl_util:name_cluster(AFirst, "A"),
            repl_util:name_cluster(BFirst, "B"),

            %% we'll need to wait for cluster names before continuing
            rt:wait_until_ring_converged(ANodes),
            rt:wait_until_ring_converged(BNodes),

            lager:info("waiting for leader to converge on cluster A"),
            ?assertEqual(ok, repl_util:wait_until_leader_converge(ANodes)),
            lager:info("waiting for leader to converge on cluster B"),
            ?assertEqual(ok, repl_util:wait_until_leader_converge(BNodes)),

            %% get the leader for the first cluster
            LeaderA = rpc:call(AFirst, riak_core_cluster_mgr, get_leader, []),

            {ok, {_IP, Port}} = rpc:call(BFirst, application, get_env,
                [riak_core, cluster_mgr]),

            lager:info("connect cluster A:~p to B on port ~p", [LeaderA, Port]),
            repl_util:connect_cluster(LeaderA, "127.0.0.1", Port),
            ?assertEqual(ok, repl_util:wait_for_connection(LeaderA, "B")),

            repl_util:enable_realtime(LeaderA, "B"),
            rt:wait_until_ring_converged(ANodes),
            repl_util:start_realtime(LeaderA, "B"),
            rt:wait_until_ring_converged(ANodes),
            repl_util:enable_fullsync(LeaderA, "B"),
            rt:wait_until_ring_converged(ANodes);
        _ ->
            lager:info("clusters should already be connected"),
            lager:info("waiting for leader to converge on cluster A"),
            ?assertEqual(ok, repl_util:wait_until_leader_converge(ANodes)),
            lager:info("waiting for leader to converge on cluster B"),
            ?assertEqual(ok, repl_util:wait_until_leader_converge(BNodes)),
            %% get the leader for the first cluster
            LeaderA = rpc:call(AFirst, riak_core_cluster_mgr, get_leader, []),
            lager:info("Leader on cluster A is ~p", [LeaderA]),
            lager:info("BFirst on cluster B is ~p", [BFirst]),
            {ok, {_IP, Port}} = rpc:call(BFirst, application, get_env,
                [riak_core, cluster_mgr]),
            lager:info("B is ~p with port ~p", [BFirst, Port])
    end,

    %% make sure we are connected
    lager:info("Wait for cluster connection A:~p -> B:~p:~p", [LeaderA, BFirst, Port]),
    ?assertEqual(ok, repl_util:wait_for_connection(LeaderA, "B")),

    log_to_nodes(AllNodes, "Write data to A, verify replication to B via realtime"),
    %% write some data on A
    %io:format("~p~n", [rpc:call(LeaderA, riak_repl_console, status, [quiet])]),
    lager:info("Writing 100 more keys to ~p", [LeaderA]),
    ?assertEqual([], repl_util:do_write(LeaderA, 101, 200, TestBucket, 2)),

    %% verify data is replicated to B
    lager:info("Reading 100 keys written to ~p from ~p", [LeaderA, BFirst]),
    ?assertEqual(0, repl_util:wait_for_reads(BFirst, 101, 200, TestBucket, 2)),

    case Connected of
        false ->
            %% check that the keys we wrote initially aren't replicated yet, because
            %% we've disabled fullsync_on_connect
            lager:info("Check keys written before repl was connected are not present"),
            Res2 = rt:systest_read(BFirst, 1, 100, TestBucket, 2),
            ?assertEqual(100, length(Res2)),

            log_to_nodes(AllNodes, "Test fullsync with leader ~p", [LeaderA]),
            repl_util:start_and_wait_until_fullsync_complete(LeaderA),

            lager:info("Check keys written before repl was connected are present"),
            ?assertEqual(0, repl_util:wait_for_reads(BFirst, 1, 200, TestBucket, 2));
        _ ->
            ok
    end,

    %% disconnect the other cluster, so realtime doesn't happen
    lager:info("disconnect the 2 clusters"),
    repl_util:disable_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:disconnect_cluster(LeaderA, "B"),
    repl_util:wait_until_no_connection(LeaderA),
    rt:wait_until_ring_converged(ANodes),


    lager:info("write 2000 keys"),
    ?assertEqual([], repl_util:do_write(LeaderA, 50000, 52000,
            TestBucket, 2)),

    lager:info("reconnect the 2 clusters"),
    repl_util:connect_cluster(LeaderA, "127.0.0.1", Port),
    ?assertEqual(ok, repl_util:wait_for_connection(LeaderA, "B")),
    rt:wait_until_ring_converged(ANodes),
    repl_util:enable_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:enable_fullsync(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:start_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    ?assertEqual(ok, repl_util:wait_until_connection(LeaderA)),

    repl_util:start_and_wait_until_fullsync_complete(LeaderA),

    lager:info("read 2000 keys"),
    ?assertEqual(0, repl_util:wait_for_reads(BFirst, 50000, 52000, TestBucket, 2)),

    %%
    %% Failover tests
    %%

    log_to_nodes(AllNodes, "Failover tests"),
    log_to_nodes(AllNodes, "Testing master failover: stopping ~p", [LeaderA]),

    lager:info("Testing master failover: stopping ~p", [LeaderA]),
    rt:stop(LeaderA),
    rt:wait_until_unpingable(LeaderA),
    ASecond = hd(ANodes -- [LeaderA]),
    repl_util:wait_until_leader(ASecond),

    LeaderA2 = rpc:call(ASecond, riak_core_cluster_mgr, get_leader, []),

    lager:info("New leader is ~p", [LeaderA2]),

    ?assertEqual(ok, repl_util:wait_until_connection(LeaderA2)),

    lager:info("Writing 100 more keys to ~p now that the old leader is down",
        [ASecond]),

    ?assertEqual([], repl_util:do_write(ASecond, 201, 300, TestBucket, 2)),

    %% verify data is replicated to B
    lager:info("Reading 100 keys written to ~p from ~p", [ASecond, BFirst]),
    ?assertEqual(0, repl_util:wait_for_reads(BFirst, 201, 300, TestBucket, 2)),

    %% get the leader for the first cluster
    LeaderB = rpc:call(BFirst, riak_core_cluster_mgr, get_leader, []),

    log_to_nodes(AllNodes, "Testing client failover: stopping ~p", [LeaderB]),

    lager:info("Testing client failover: stopping ~p", [LeaderB]),
    rt:stop(LeaderB),
    rt:wait_until_unpingable(LeaderB),
    BSecond = hd(BNodes -- [LeaderB]),
    repl_util:wait_until_leader(BSecond),

    LeaderB2 = rpc:call(BSecond, riak_core_cluster_mgr, get_leader, []),

    lager:info("New leader is ~p", [LeaderB2]),

    ?assertEqual(ok, repl_util:wait_until_connection(LeaderA2)),

    lager:info("Writing 100 more keys to ~p now that the old leader is down",
        [ASecond]),

    ?assertEqual([], repl_util:do_write(ASecond, 301, 400, TestBucket, 2)),

    %% verify data is replicated to B
    lager:info("Reading 101 keys written to ~p from ~p", [ASecond, BSecond]),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 301, 400, TestBucket, 2)),

    %% Testing fullsync with downed nodes
    log_to_nodes(AllNodes, "Test fullsync with ~p and ~p down", [LeaderA, LeaderB]),
    lager:info("Re-running fullsync with ~p and ~p down", [LeaderA, LeaderB]),

    repl_util:start_and_wait_until_fullsync_complete(LeaderA2),

    %%
    %% Per-bucket repl settings tests
    %%

    log_to_nodes(AllNodes, "Test fullsync after restarting ~p", [LeaderA]),

    lager:info("Restarting down node ~p", [LeaderA]),
    rt:start(LeaderA),
    rt:wait_until_pingable(LeaderA),
    repl_util:start_and_wait_until_fullsync_complete(LeaderA2),

    log_to_nodes(AllNodes, "Starting Joe's Repl Test"),
    lager:info("Starting Joe's Repl Test"),

    %% @todo add stuff
    %% At this point, realtime sync should still work, but, it doesn't because of a bug in 1.2.1 
    %% Check that repl leader is LeaderA
    %% Check that LeaderA2 has ceeded socket back to LeaderA

    lager:info("Leader: ~p", [rpc:call(ASecond, riak_core_cluster_mgr, get_leader, [])]),
    lager:info("LeaderA: ~p", [LeaderA]),
    lager:info("LeaderA2: ~p", [LeaderA2]),

    ?assertEqual(ok, repl_util:wait_until_connection(LeaderA)),

    log_to_nodes(AllNodes, "Simulate partition to force leader re-election"),

    lager:info("Simulation partition to force leader re-election"),

    OldCookie = rpc:call(LeaderA2, erlang, get_cookie, []),
    NewCookie = list_to_atom(lists:reverse(atom_to_list(OldCookie))),
    rpc:call(LeaderA2, erlang, set_cookie, [LeaderA2, NewCookie]),

    [ rpc:call(LeaderA2, erlang, disconnect_node, [Node]) || Node <- ANodes -- [LeaderA2]],
    [ rpc:call(Node, erlang, disconnect_node, [LeaderA2]) || Node <- ANodes -- [LeaderA2]],

    repl_util:wait_until_new_leader(hd(ANodes -- [LeaderA2]), LeaderA2),
    InterimLeader = rpc:call(LeaderA, riak_core_cluster_mgr, get_leader, []),
    lager:info("Interim leader: ~p", [InterimLeader]),

    %rpc:call(LeaderA2, erlang, apply, [fun() -> [net_adm:ping(N) || N <- ANodes] end, []]),
    rpc:call(LeaderA2, erlang, set_cookie, [LeaderA2, OldCookie]),

    [ rpc:call(LeaderA2, net_adm, ping, [Node]) || Node <- ANodes -- [LeaderA2]],
    [ rpc:call(Node, net_adm, ping, [LeaderA2]) || Node <- ANodes -- [LeaderA2]],

    %% there's no point in writing anything until the leaders converge, as we
    %% can drop writes in the middle of an election
    repl_util:wait_until_leader_converge(ANodes),

    lager:info("Leader: ~p", [rpc:call(ASecond, riak_core_cluster_mgr, get_leader, [])]),
    lager:info("Writing 2 more keys to ~p", [LeaderA]),
    ?assertEqual([], repl_util:do_write(LeaderA, 1301, 1302, TestBucket, 2)),

    %% verify data is replicated to B
    lager:info("Reading 2 keys written to ~p from ~p", [LeaderA, BSecond]),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 1301, 1302, TestBucket, 2)),

    log_to_nodes(AllNodes, "Finished Joe's Section"),
    lager:info("Finished Joe's Section"),

    lager:info("Restarting down node ~p", [LeaderB]),
    rt:start(LeaderB),
    rt:wait_until_pingable(LeaderB),

    lager:info("Nodes restarted"),

    replication:make_bucket(ANodes, NoRepl, [{repl, false}]),

    replication:make_bucket(ANodes, RealtimeOnly, [{repl, realtime}]),
    replication:make_bucket(ANodes, FullsyncOnly, [{repl, fullsync}]),

    %% disconnect the other cluster, so realtime doesn't happen
    lager:info("disconnect the 2 clusters"),
    repl_util:disable_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:disconnect_cluster(LeaderA, "B"),
    repl_util:wait_until_no_connection(LeaderA),
    rt:wait_until_ring_converged(ANodes),

    lager:info("write 100 keys to a realtime only bucket"),
    ?assertEqual([], repl_util:do_write(ASecond, 1, 100,
            RealtimeOnly, 2)),

    lager:info("reconnect the 2 clusters"),
    repl_util:connect_cluster(LeaderA, "127.0.0.1", Port),
    ?assertEqual(ok, repl_util:wait_for_connection(LeaderA, "B")),
    rt:wait_until_ring_converged(ANodes),
    repl_util:enable_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:enable_fullsync(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    repl_util:start_realtime(LeaderA, "B"),
    rt:wait_until_ring_converged(ANodes),
    ?assertEqual(ok, repl_util:wait_until_connection(LeaderA)),

    LeaderA3 = rpc:call(ASecond, riak_core_cluster_mgr, get_leader, []),

    log_to_nodes(AllNodes, "Test fullsync and realtime independence"),

    lager:info("write 100 keys to a {repl, false} bucket"),
    ?assertEqual([], repl_util:do_write(ASecond, 1, 100, NoRepl, 2)),

    lager:info("write 100 keys to a fullsync only bucket"),
    ?assertEqual([], repl_util:do_write(ASecond, 1, 100,
            FullsyncOnly, 2)),

    lager:info("Check the fullsync only bucket didn't replicate the writes"),
    Res6 = rt:systest_read(BSecond, 1, 100, FullsyncOnly, 2),
    ?assertEqual(100, length(Res6)),

    lager:info("Check the realtime only bucket that was written to offline "
        "isn't replicated"),
    Res7 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
    ?assertEqual(100, length(Res7)),

    lager:info("Check the {repl, false} bucket didn't replicate"),
    Res8 = rt:systest_read(BSecond, 1, 100, NoRepl, 2),
    ?assertEqual(100, length(Res8)),

    %% do a fullsync, make sure that fullsync_only is replicated, but
    %% realtime_only and no_repl aren't
    repl_util:start_and_wait_until_fullsync_complete(LeaderA3),

    lager:info("Check fullsync only bucket is now replicated"),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 1, 100,
            FullsyncOnly, 2)),

    lager:info("Check realtime only bucket didn't replicate"),
    Res10 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
    ?assertEqual(100, length(Res10)),


    lager:info("Write 100 more keys into realtime only bucket on ~p",
        [ASecond]),
    ?assertEqual([], repl_util:do_write(ASecond, 101, 200,
            RealtimeOnly, 2)),

    timer:sleep(5000),

    lager:info("Check the realtime keys replicated"),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 101, 200,
            RealtimeOnly, 2)),

    lager:info("Check the older keys in the realtime bucket did not replicate"),
    Res12 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
    ?assertEqual(100, length(Res12)),

    lager:info("Check {repl, false} bucket didn't replicate"),
    Res13 = rt:systest_read(BSecond, 1, 100, NoRepl, 2),
    ?assertEqual(100, length(Res13)),

    log_to_nodes(AllNodes, "Testing offline realtime queueing"),
    lager:info("Testing offline realtime queueing"),

    LeaderA4 = rpc:call(ASecond, riak_core_cluster_mgr, get_leader, []),

    lager:info("Stopping realtime, queue will build"),
    repl_util:stop_realtime(LeaderA4, "B"),
    rt:wait_until_ring_converged(ANodes),

    lager:info("Writing 100 keys"),
    ?assertEqual([], repl_util:do_write(LeaderA4, 800, 900,
            TestBucket, 2)),

    lager:info("Starting realtime"),
    repl_util:start_realtime(LeaderA4, "B"),
    rt:wait_until_ring_converged(ANodes),
    timer:sleep(3000),

    lager:info("Reading keys written while repl was stopped"),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 800, 900,
            TestBucket, 2)),

    log_to_nodes(AllNodes, "Testing realtime migration on node shutdown"),
    lager:info("Testing realtime migration on node shutdown"),
    Target = hd(ANodes -- [LeaderA4]),

    lager:info("Stopping realtime, queue will build"),
    repl_util:stop_realtime(LeaderA4, "B"),
    rt:wait_until_ring_converged(ANodes),

    lager:info("Writing 100 keys"),
    ?assertEqual([], repl_util:do_write(Target, 900, 1000,
            TestBucket, 2)),

    io:format("queue status: ~p",
              [rpc:call(Target, riak_repl2_rtq, status, [])]),

    lager:info("Stopping node ~p", [Target]),

    rt:stop(Target),
    rt:wait_until_unpingable(Target),

    lager:info("Starting realtime"),
    repl_util:start_realtime(LeaderA4, "B"),
    timer:sleep(3000),

    lager:info("Reading keys written while repl was stopped"),
    ?assertEqual(0, repl_util:wait_for_reads(BSecond, 900, 1000,
            TestBucket, 2)),

    lager:info("Restarting node ~p", [Target]),

    rt:start(Target),
    rt:wait_until_pingable(Target),
    rt:wait_for_service(Target, riak_repl),
    timer:sleep(5000),

    pb_write_during_shutdown(Target, BSecond, TestBucket),
    timer:sleep(5000),
    http_write_during_shutdown(Target, BSecond, TestBucket),

    lager:info("Test passed"),
    fin.

pb_write_during_shutdown(Target, BSecond, TestBucket) ->
    ConnInfo = proplists:get_value(Target, rt:connection_info([Target])),
    {IP, Port} = proplists:get_value(pb, ConnInfo),
    lager:info("Connecting to pb socket ~p:~p on ~p", [IP, Port, Target]),
    PBSock = rt:pbc(Target),

    %% do the stop in the background while we're writing keys
    spawn(fun() ->
                timer:sleep(500),
                lager:info("Stopping node ~p again", [Target]),
                rt:stop(Target),
                lager:info("Node stopped")
           end),

    lager:info("Writing 10,000 keys"),
    WriteErrors =
        try
          pb_write(PBSock, 1000, 11000, TestBucket, 2)
        catch
          _:_ ->
            lager:info("Shutdown timeout caught"),
            []
        end,
    lager:info("got ~p write failures", [length(WriteErrors)]),
    timer:sleep(3000),
    lager:info("checking number of read failures on secondary cluster"),
    ReadErrors = rt:systest_read(BSecond, 1000, 11000, TestBucket, 2),
    lager:info("got ~p read failures", [length(ReadErrors)]),

    %% ensure node is down before we try to start it up again.
    lager:info("pb_write_during_shutdown: Ensure node ~p is down before restart", [Target]),
    ?assertEqual(ok, rt:wait_until_unpingable(Target)),

    rt:start(Target),
    rt:wait_until_pingable(Target),
    rt:wait_for_service(Target, riak_repl),
    ReadErrors2 = rt:systest_read(Target, 1000, 11000, TestBucket, 2),
    lager:info("got ~p read failures on ~p", [length(ReadErrors2), Target]),
    case length(WriteErrors) >= length(ReadErrors) of
        true ->
            ok;
        false ->
            lager:error("Got more read errors on ~p: ~p than write "
                "errors on ~p: ~p",
                        [BSecond, length(ReadErrors), Target,
                            length(WriteErrors)]),
            FailedKeys = lists:foldl(fun({Key, _}, Acc) ->
                        case lists:keyfind(Key, 1, WriteErrors) of
                            false ->
                                [Key|Acc];
                            _ ->
                                Acc
                        end
                end, [], ReadErrors),
            lager:info("failed keys ~p", [FailedKeys]),
            ?assert(false)
    end.

http_write_during_shutdown(Target, BSecond, TestBucket) ->
    ConnInfo = proplists:get_value(Target, rt:connection_info([Target])),
    {IP, Port} = proplists:get_value(http, ConnInfo),
    lager:info("Connecting to http socket ~p:~p on ~p", [IP, Port, Target]),
    C = rt:httpc(Target),

    %% do the stop in the background while we're writing keys
    spawn(fun() ->
                timer:sleep(500),
                lager:info("Stopping node ~p again", [Target]),
                rt:stop(Target),
                lager:info("Node stopped")
           end),

    lager:info("Writing 10,000 keys"),
    WriteErrors =
        try
          http_write(C, 12000, 22000, TestBucket, 2)
        catch
          _:_ ->
            lager:info("Shutdown timeout caught"),
            []
        end,
    lager:info("got ~p write failures to ~p", [length(WriteErrors), Target]),
    timer:sleep(3000),
    lager:info("checking number of read failures on secondary cluster node, ~p", [BSecond]),
    {ok, [{_IP, Port2}|_]} = rpc:call(BSecond, application, get_env, [riak_core, http]),
    C2 = rhc:create("127.0.0.1", Port2, "riak", []),
    ReadErrors = http_read(C2, 12000, 22000, TestBucket, 2),
    lager:info("got ~p read failures from ~p", [length(ReadErrors), BSecond]),

    %% ensure node is down before we try to start it up again.
    lager:info("http: write_during_shutdown: Ensure node ~p is down before restart", [Target]),
    ?assertEqual(ok, rt:wait_until_unpingable(Target)),

    rt:start(Target),
    rt:wait_until_pingable(Target),
    rt:wait_for_service(Target, riak_repl),
    ReadErrors2 = http_read(C, 12000, 22000, TestBucket, 2),
    lager:info("got ~p read failures on ~p", [length(ReadErrors2), Target]),
    case length(WriteErrors) >= length(ReadErrors) of
        true ->
            ok;
        false ->
            lager:error("Got more read errors on ~p: ~p than write "
                "errors on ~p: ~p",
                        [BSecond, length(ReadErrors), Target,
                            length(WriteErrors)]),
            FailedKeys = lists:foldl(fun({Key, _}, Acc) ->
                        case lists:keyfind(Key, 1, WriteErrors) of
                            false ->
                                [Key|Acc];
                            _ ->
                                Acc
                        end
                end, [], ReadErrors),
            lager:info("failed keys ~p", [FailedKeys]),
            ?assert(false)
    end.

client_iterate(_Sock, [], _Bucket, _W, Acc, _Fun, Parent) ->
    Parent ! {result, self(), Acc},
    Acc;

client_iterate(Sock, [N | NS], Bucket, W, Acc, Fun, Parent) ->
    NewAcc = try Fun(Sock, Bucket, N, W) of
        ok ->
            Acc;
        Other ->
            [{N, Other} | Acc]
    catch
        What:Why ->
            [{N, {What, Why}} | Acc]
    end,
    client_iterate(Sock, NS, Bucket, W, NewAcc, Fun, Parent).

http_write(Sock, Start, End, Bucket, W) ->
    F = fun(S, B, K, WVal) ->
            X = list_to_binary(integer_to_list(K)),
            Obj = riakc_obj:new(B, X, X),
            rhc:put(S, Obj, [{dw, WVal}])
    end,
    Keys = lists:seq(Start, End),
    Partitions = partition_keys(Keys, 8),
    Parent = self(),
    Workers = [spawn_monitor(fun() -> client_iterate(Sock, K, Bucket, W, [], F,
                    Parent) end) || K <- Partitions],
    collect_results(Workers, []).

pb_write(Sock, Start, End, Bucket, W) ->
    %pb_iterate(Sock, lists:seq(Start, End), Bucket, W, []).
    F = fun(S, B, K, WVal) ->
            Obj = riakc_obj:new(B, <<K:32/integer>>, <<K:32/integer>>),
            riakc_pb_socket:put(S, Obj, [{dw, WVal}])
    end,
    Keys = lists:seq(Start, End),
    Partitions = partition_keys(Keys, 8),
    Parent = self(),
    Workers = [spawn_monitor(fun() -> client_iterate(Sock, K, Bucket, W, [], F,
                    Parent) end) || K <- Partitions],
    collect_results(Workers, []).

http_read(Sock, Start, End, Bucket, R) ->
    F = fun(S, B, K, RVal) ->
            X = list_to_binary(integer_to_list(K)),
            case rhc:get(S, B, X, [{r, RVal}]) of
                {ok, _} ->
                    ok;
                Error ->
                    Error
            end
    end,
    client_iterate(Sock, lists:seq(Start, End), Bucket, R, [], F, self()).

partition_keys(Keys, PC) ->
    partition_keys(Keys, PC, lists:duplicate(PC, [])).

partition_keys([] , _, Acc) ->
    Acc;
partition_keys(Keys, PC, Acc) ->
    In = lists:sublist(Keys, PC),
    Rest = try lists:nthtail(PC, Keys)
        catch _:_ -> []
    end,
    NewAcc = lists:foldl(fun(K, [H|T]) ->
                T ++ [[K|H]]
        end, Acc, In),
    partition_keys(Rest, PC, NewAcc).

collect_results([], Acc) ->
    Acc;
collect_results(Workers, Acc) ->
    receive
        {result, Pid, Res} ->
            collect_results(lists:keydelete(Pid, 1, Workers), Res ++ Acc);
        {'DOWN', _, _, Pid, _Reason} ->
            collect_results(lists:keydelete(Pid, 1, Workers), Acc)
    end.
