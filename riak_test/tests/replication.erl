-module(replication).
-behavior(riak_test).
-export([confirm/0]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(rt, [deploy_nodes/2,
             join/2,
             wait_until_nodes_ready/1,
             wait_until_no_pending_changes/1]).

%% export functions shared with other replication tests...
-export([make_bucket/3]).

confirm() ->
    NumNodes = rt:config(num_nodes, 6),
    ClusterASize = rt:config(cluster_a_size, 3),

    lager:info("Deploy ~p nodes", [NumNodes]),
    Conf = [
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
    rt:log_to_nodes(Nodes, "Build cluster A"),
    repl_util:make_cluster(ANodes),

    lager:info("Build cluster B"),
    rt:log_to_nodes(Nodes, "Build cluster B"),
    repl_util:make_cluster(BNodes),

    replication(ANodes, BNodes, false),
    pass.

replication([AFirst|_] = ANodes, [BFirst|_] = BNodes, Connected) ->

    AllNodes = ANodes ++ BNodes,

    rt:log_to_nodes(AllNodes, "Starting replication test"),

    TestHash = erlang:md5(term_to_binary(os:timestamp())),
    TestBucket = <<TestHash/binary, "-systest_a">>,
    FullsyncOnly = <<TestHash/binary, "-fullsync_only">>,
    RealtimeOnly = <<TestHash/binary, "-realtime_only">>,
    NoRepl = <<TestHash/binary, "-no_repl">>,

    case Connected of
        false ->
            %% clusters are not connected, connect them

            %% write some initial data to A
            lager:info("Writing 100 keys to ~p", [AFirst]),
            ?assertEqual([], do_write(AFirst, 1, 100, TestBucket, 2)),

            rt:log_to_nodes(AllNodes, "Adding listeners"),
            %% setup servers/listeners on A
            Listeners = add_listeners(ANodes),
            rt:wait_until_ring_converged(ANodes),

            %% verify servers are visible on all nodes
            verify_listeners(Listeners),

            lager:info("waiting for leader to converge on cluster A"),
            ?assertEqual(ok, wait_until_leader_converge(ANodes)),
            lager:info("waiting for leader to converge on cluster B"),
            ?assertEqual(ok, wait_until_leader_converge(BNodes)),

            %% get the leader for the first cluster
            LeaderA = rpc:call(AFirst, riak_repl_leader, leader_node, []),

            %% list of listeners not on the leader node
            NonLeaderListeners = lists:keydelete(LeaderA, 3, Listeners),

            rt:log_to_nodes(AllNodes, "Setup replication sites"),
            %% setup sites on B
            %% TODO: make `NumSites' an argument
            NumSites = 4,
            {Ip, Port, _} = hd(NonLeaderListeners),
            add_site(hd(BNodes), {Ip, Port, "site1"}),
            rt:wait_until_ring_converged(BNodes),
            FakeListeners = gen_fake_listeners(NumSites-1),
            add_fake_sites(BNodes, FakeListeners),
            rt:wait_until_ring_converged(BNodes),

            rt:log_to_nodes(AllNodes, "Verify replication sites"),
            %% verify sites are distributed on B
            verify_sites_balanced(NumSites, BNodes),

            wait_until_connection(LeaderA),
            %% check the listener IPs were all imported into the site
            verify_site_ips(BFirst, "site1", Listeners);
        _ ->
            lager:info("waiting for leader to converge on cluster A"),
            ?assertEqual(ok, wait_until_leader_converge(ANodes)),
            lager:info("waiting for leader to converge on cluster B"),
            ?assertEqual(ok, wait_until_leader_converge(BNodes)),
            %% get the leader for the first cluster
            LeaderA = rpc:call(AFirst, riak_repl_leader, leader_node, []),
            lager:info("Leader on cluster A is ~p", [LeaderA]),
            [{Ip, Port, _}|_] = get_listeners(LeaderA)
    end,

    rt:log_to_nodes(AllNodes, "Write data to A"),
    %% write some data on A
    ?assertEqual(ok, wait_until_connection(LeaderA)),
    %io:format("~p~n", [rpc:call(LeaderA, riak_repl_console, status, [quiet])]),
    lager:info("Writing 100 more keys to ~p", [LeaderA]),
    ?assertEqual([], do_write(LeaderA, 101, 200, TestBucket, 2)),

    rt:log_to_nodes(AllNodes, "Verify data received on B"),
    %% verify data is replicated to B
    lager:info("Reading 100 keys written to ~p from ~p", [LeaderA, BFirst]),
    ?assertEqual(0, wait_for_reads(BFirst, 101, 200, TestBucket, 2)),

    case Connected of
        false ->
            %% check that the keys we wrote initially aren't replicated yet, because
            %% we've disabled fullsync_on_connect
            lager:info("Check keys written before repl was connected are not present"),
            Res2 = rt:systest_read(BFirst, 1, 100, TestBucket, 2),
            ?assertEqual(100, length(Res2)),

            start_and_wait_until_fullsync_complete(LeaderA),

            lager:info("Check keys written before repl was connected are present"),
            ?assertEqual(0, wait_for_reads(BFirst, 1, 200, TestBucket, 2));
        _ ->
            ok
    end,

    ASecond = hd(ANodes -- [LeaderA]),

    %% disconnect the other cluster, so realtime doesn't happen
    lager:info("disconnect the 2 clusters"),
    del_site(BNodes, "site1"),
    ?assertEqual(ok, wait_until_no_connection(LeaderA)),

    lager:info("write 2000 keys"),
    ?assertEqual([], do_write(ASecond, 50000, 52000,
            TestBucket, 2)),

    lager:info("reconnect the 2 clusters"),
    add_site(hd(BNodes), {Ip, Port, "site1"}),
    ?assertEqual(ok, wait_until_connection(LeaderA)),

    start_and_wait_until_fullsync_complete(LeaderA),

    lager:info("read 2000 keys"),
    ?assertEqual(0, wait_for_reads(BFirst, 50000, 52000, TestBucket, 2)),

    %%
    %% Failover tests
    %%

    rt:log_to_nodes(AllNodes, "Testing master failover: stopping ~p", [LeaderA]),
    lager:info("Testing master failover: stopping ~p", [LeaderA]),
    rt:stop(LeaderA),
    rt:wait_until_unpingable(LeaderA),
    wait_until_leader(ASecond),

    LeaderA2 = rpc:call(ASecond, riak_repl_leader, leader_node, []),

    lager:info("New leader is ~p", [LeaderA2]),

    ?assertEqual(ok, wait_until_connection(LeaderA2)),

    lager:info("Writing 100 more keys to ~p now that the old leader is down",
        [ASecond]),

    ?assertEqual([], do_write(ASecond, 201, 300, TestBucket, 2)),

    %% verify data is replicated to B
    lager:info("Reading 100 keys written to ~p from ~p", [ASecond, BFirst]),
    ?assertEqual(0, wait_for_reads(BFirst, 201, 300, TestBucket, 2)),

    %% get the leader for the first cluster
    LeaderB = rpc:call(BFirst, riak_repl_leader, leader_node, []),

    lager:info("Testing client failover: stopping ~p", [LeaderB]),
    rt:stop(LeaderB),
    rt:wait_until_unpingable(LeaderB),
    BSecond = hd(BNodes -- [LeaderB]),
    wait_until_leader(BSecond),

    LeaderB2 = rpc:call(BSecond, riak_repl_leader, leader_node, []),

    lager:info("New leader is ~p", [LeaderB2]),

    ?assertEqual(ok, wait_until_connection(LeaderA2)),

    lager:info("Writing 100 more keys to ~p now that the old leader is down",
        [ASecond]),

    ?assertEqual([], do_write(ASecond, 301, 400, TestBucket, 2)),

    %% verify data is replicated to B
    rt:wait_until_pingable(BSecond),
    lager:info("Reading 101 keys written to ~p from ~p", [ASecond, BSecond]),
    ?assertEqual(0, wait_for_reads(BSecond, 301, 400, TestBucket, 2)),

    %% Testing fullsync with downed nodes
    lager:info("Re-running fullsync with ~p and ~p down", [LeaderA, LeaderB]),

    start_and_wait_until_fullsync_complete(LeaderA2),

    %%
    %% Per-bucket repl settings tests
    %%

    lager:info("Restarting down node ~p", [LeaderA]),
    rt:start(LeaderA),
    rt:wait_until_pingable(LeaderA),
    start_and_wait_until_fullsync_complete(LeaderA2),

    case nodes_all_have_version(ANodes, "1.2.2") of
        true ->

            lager:info("Starting Joe's Repl Test"),

            %% At this point, realtime sync should still work, but, it doesn't
            %% because of a bug in 1.2.1.
            %% Check that repl leader is LeaderA
            %% Check that LeaderA2 has ceeded socket back to LeaderA

            lager:info("Leader: ~p", [rpc:call(ASecond, riak_repl_leader, leader_node, [])]),
            lager:info("LeaderA: ~p", [LeaderA]),
            lager:info("LeaderA2: ~p", [LeaderA2]),

            ?assertEqual(ok, wait_until_connection(LeaderA)),

            lager:info("Simulation partition to force leader re-election"),

            OldCookie = rpc:call(LeaderA2, erlang, get_cookie, []),
            NewCookie = list_to_atom(lists:reverse(atom_to_list(OldCookie))),
            rpc:call(LeaderA2, erlang, set_cookie, [LeaderA2, NewCookie]),

            [ rpc:call(LeaderA2, erlang, disconnect_node, [Node]) ||
                Node <- ANodes -- [LeaderA2]],
            [ rpc:call(Node, erlang, disconnect_node, [LeaderA2]) ||
                Node <- ANodes -- [LeaderA2]],

            wait_until_new_leader(hd(ANodes -- [LeaderA2]), LeaderA2),
            InterimLeader = rpc:call(LeaderA, riak_repl_leader, leader_node, []),
            lager:info("Interim leader: ~p", [InterimLeader]),

            rpc:call(LeaderA2, erlang, set_cookie, [LeaderA2, OldCookie]),

            [ rpc:call(LeaderA2, net_adm, ping, [Node]) ||
                Node <- ANodes -- [LeaderA2]],
            [ rpc:call(Node, net_adm, ping, [LeaderA2]) ||
                Node <- ANodes -- [LeaderA2]],

            %% there's no point in writing anything until the leaders
            %% converge, as we can drop writes in the middle of an election
            wait_until_leader_converge(ANodes),

            LeaderA3 = rpc:call(ASecond, riak_repl_leader, leader_node, []),

            wait_until_connection(LeaderA3),

            lager:info("Leader: ~p", [LeaderA3]),
            lager:info("Writing 2 more keys to ~p", [LeaderA3]),
            ?assertEqual([], do_write(LeaderA3, 1301, 1302, TestBucket, 2)),

            %% verify data is replicated to B
            lager:info("Reading 2 keys written to ~p from ~p", [LeaderA3, BSecond]),
            ?assertEqual(0, wait_for_reads(BSecond, 1301, 1302, TestBucket, 2)),

            lager:info("Finished Joe's Section"),

            lager:info("Nodes restarted");
        _ ->
            lager:info("Skipping Joe's Repl Test")
    end,

    lager:info("Restarting down node ~p", [LeaderB]),
    rt:start(LeaderB),
    rt:wait_until_pingable(LeaderB),

    case nodes_all_have_version(ANodes, "1.1.0") of
        true ->

            make_bucket(ANodes, NoRepl, [{repl, false}]),

            case nodes_all_have_version(ANodes, "1.2.0") of
                true ->
                    make_bucket(ANodes, RealtimeOnly, [{repl, realtime}]),
                    make_bucket(ANodes, FullsyncOnly, [{repl, fullsync}]),

                    %% disconnect the other cluster, so realtime doesn't happen
                    lager:info("disconnect the 2 clusters"),
                    del_site(BNodes, "site1"),
                    ?assertEqual(ok, wait_until_no_connection(LeaderA)),

                    lager:info("write 100 keys to a realtime only bucket"),
                    ?assertEqual([], do_write(ASecond, 1, 100,
                            RealtimeOnly, 2)),

                    lager:info("reconnect the 2 clusters"),
                    add_site(LeaderB, {Ip, Port, "site1"}),
                    ?assertEqual(ok, wait_until_connection(LeaderA));
                _ ->
                    timer:sleep(1000)
            end,

            LeaderA4 = rpc:call(ASecond, riak_repl_leader, leader_node, []),

            lager:info("write 100 keys to a {repl, false} bucket"),
            ?assertEqual([], do_write(ASecond, 1, 100, NoRepl, 2)),

            case nodes_all_have_version(ANodes, "1.2.0") of
                true ->
                    lager:info("write 100 keys to a fullsync only bucket"),
                    ?assertEqual([], do_write(ASecond, 1, 100,
                            FullsyncOnly, 2)),

                    lager:info("Check the fullsync only bucket didn't replicate the writes"),
                    Res6 = rt:systest_read(BSecond, 1, 100, FullsyncOnly, 2),
                    ?assertEqual(100, length(Res6)),

                    lager:info("Check the realtime only bucket that was written to offline "
                        "isn't replicated"),
                    Res7 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
                    ?assertEqual(100, length(Res7));
                _ ->
                    timer:sleep(1000)
            end,

            lager:info("Check the {repl, false} bucket didn't replicate"),
            Res8 = rt:systest_read(BSecond, 1, 100, NoRepl, 2),
            ?assertEqual(100, length(Res8)),

            %% do a fullsync, make sure that fullsync_only is replicated, but
            %% realtime_only and no_repl aren't
            start_and_wait_until_fullsync_complete(LeaderA4),

            case nodes_all_have_version(ANodes, "1.2.0") of
                true ->
                    lager:info("Check fullsync only bucket is now replicated"),
                    ?assertEqual(0, wait_for_reads(BSecond, 1, 100,
                            FullsyncOnly, 2)),

                    lager:info("Check realtime only bucket didn't replicate"),
                    Res10 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
                    ?assertEqual(100, length(Res10)),


                    lager:info("Write 100 more keys into realtime only bucket"),
                    ?assertEqual([], do_write(ASecond, 101, 200,
                            RealtimeOnly, 2)),

                    timer:sleep(5000),

                    lager:info("Check the realtime keys replicated"),
                    ?assertEqual(0, wait_for_reads(BSecond, 101, 200,
                                RealtimeOnly, 2)),

                    lager:info("Check the older keys in the realtime bucket did not replicate"),
                    Res12 = rt:systest_read(BSecond, 1, 100, RealtimeOnly, 2),
                    ?assertEqual(100, length(Res12));
                _ ->
                    ok
            end,

            lager:info("Check {repl, false} bucket didn't replicate"),
            Res13 = rt:systest_read(BSecond, 1, 100, NoRepl, 2),
            ?assertEqual(100, length(Res13));
        _ ->
            ok
    end,

    lager:info("Test passed"),
    fin.

verify_sites_balanced(NumSites, BNodes0) ->
    Leader = rpc:call(hd(BNodes0), riak_repl_leader, leader_node, []),
    case node_has_version(Leader, "1.2.0") of
        true ->
            BNodes = nodes_with_version(BNodes0, "1.2.0") -- [Leader],
            NumNodes = length(BNodes),
            case NumNodes of
                0 ->
                    %% only leader is upgraded, runs clients locally
                    ?assertEqual(NumSites, client_count(Leader));
                _ ->
                    NodeCounts = [{Node, client_count(Node)} || Node <- BNodes],
                    lager:notice("nodecounts ~p", [NodeCounts]),
                    lager:notice("leader ~p", [Leader]),
                    Min = NumSites div NumNodes,
                    [?assert(Count >= Min) || {_Node, Count} <- NodeCounts]
            end;
        false ->
            ok
    end.

%% does the node meet the version requirement?
node_has_version(Node, Version) ->
    {_, NodeVersion} =  rpc:call(Node, init, script_id, []),
    case NodeVersion of
        current ->
            %% current always satisfies any version check
            true;
        _ ->
            NodeVersion >= Version
    end.

nodes_with_version(Nodes, Version) ->
    [Node || Node <- Nodes, node_has_version(Node, Version)].

nodes_all_have_version(Nodes, Version) ->
    Nodes == nodes_with_version(Nodes, Version).

client_count(Node) ->
    Clients = rpc:call(Node, supervisor, which_children, [riak_repl_client_sup]),
    length(Clients).

gen_fake_listeners(Num) ->
    Ports = gen_ports(11000, Num),
    IPs = lists:duplicate(Num, "127.0.0.1"),
    Nodes = [fake_node(N) || N <- lists:seq(1, Num)],
    lists:zip3(IPs, Ports, Nodes).

fake_node(Num) ->
    lists:flatten(io_lib:format("fake~p@127.0.0.1", [Num])).

add_fake_sites([Node|_], Listeners) ->
    [add_site(Node, {IP, Port, fake_site(Port)})
     || {IP, Port, _} <- Listeners].

add_site(Node, {IP, Port, Name}) ->
    lager:info("Add site ~p ~p:~p at node ~p", [Name, IP, Port, Node]),
    Args = [IP, integer_to_list(Port), Name],
    Res = rpc:call(Node, riak_repl_console, add_site, [Args]),
    ?assertEqual(ok, Res),
    timer:sleep(timer:seconds(5)).

del_site([Node|_]=Nodes, Name) ->
    lager:info("Del site ~p at ~p", [Name, Node]),
    Res = rpc:call(Node, riak_repl_console, del_site, [[Name]]),
    ?assertEqual(ok, Res),
    rt:wait_until_ring_converged(Nodes),
    timer:sleep(timer:seconds(5)).

fake_site(Port) ->
    lists:flatten(io_lib:format("fake_site_~p", [Port])).

verify_listeners(Listeners) ->
    Strs = [IP ++ ":" ++ integer_to_list(Port) || {IP, Port, _} <- Listeners],
    [?assertEqual(ok, verify_listener(Node, Strs)) || {_, _, Node} <- Listeners].

verify_listener(Node, Strs) ->
    lager:info("Verify listeners ~p ~p", [Node, Strs]),
    rt:wait_until(Node,
        fun(_) ->
                Status = rpc:call(Node, riak_repl_console, status, [quiet]),
                lists:all(fun(Str) ->
                            lists:keymember(Str, 2, Status)
                    end, Strs)
        end).

add_listeners(Nodes=[FirstNode|_]) ->
    Ports = gen_ports(9010, length(Nodes)),
    IPs = lists:duplicate(length(Nodes), "127.0.0.1"),
    PN = lists:zip3(IPs, Ports, Nodes),
    [add_listener(FirstNode, Node, IP, Port) || {IP, Port, Node} <- PN],
    timer:sleep(timer:seconds(5)),
    PN.

add_listener(N, Node, IP, Port) ->
    lager:info("Adding repl listener to ~p ~s:~p", [Node, IP, Port]),
    Args = [[atom_to_list(Node), IP, integer_to_list(Port)]],
    Res = rpc:call(N, riak_repl_console, add_listener, Args),
    ?assertEqual(ok, Res).

del_listeners(Node) ->
    Listeners = get_listeners(Node),
    lists:foreach(fun(Listener={IP, Port, N}) ->
                lager:info("deleting listener ~p on ~p", [Listener, Node]),
                Res = rpc:call(Node, riak_repl_console, del_listener,
                    [[atom_to_list(N), IP, integer_to_list(Port)]]),
                ?assertEqual(ok, Res)
        end, Listeners).

get_listeners(Node) ->
    Status = rpc:call(Node, riak_repl_console, status, [quiet]),
    %% *sigh*
    [
        begin
                NodeName = list_to_atom(string:substr(K, 10)),
                [IP, Port] = string:tokens(V, ":"),
                {IP, list_to_integer(Port), NodeName}
        end || {K, V} <- Status, is_list(K), string:substr(K, 1, 9) == "listener_"
    ].

gen_ports(Start, Len) ->
    lists:seq(Start, Start + Len - 1).

verify_site_ips(Leader, Site, Listeners) ->
    Status = rpc:call(Leader, riak_repl_console, status, [quiet]),
    Key = lists:flatten([Site, "_ips"]),
    IPStr = proplists:get_value(Key, Status),
    IPs = lists:sort(re:split(IPStr, ", ")),
    ExpectedIPs = lists:sort(
        [list_to_binary([IP, ":", integer_to_list(Port)]) || {IP, Port, _Node} <-
            Listeners]),
    ?assertEqual(ExpectedIPs, IPs).

make_bucket([Node|_]=Nodes, Name, Args) ->
    Res = rpc:call(Node, riak_core_bucket, set_bucket, [Name, Args]),
    rt:wait_until_ring_converged(Nodes),
    ?assertEqual(ok, Res).

start_and_wait_until_fullsync_complete(Node) ->
    Status0 = rpc:call(Node, riak_repl_console, status, [quiet]),
    Count = proplists:get_value(server_fullsyncs, Status0) + 1,
    lager:info("waiting for fullsync count to be ~p", [Count]),

    lager:info("Starting fullsync on ~p (~p)", [Node,
            rtdev:node_version(rtdev:node_id(Node))]),
    rpc:call(Node, riak_repl_console, start_fullsync, [[]]),
    %% sleep because of the old bug where stats will crash if you call it too
    %% soon after starting a fullsync
    timer:sleep(500),

    Res = rt:wait_until(Node,
        fun(_) ->
                Status = rpc:call(Node, riak_repl_console, status, [quiet]),
                case proplists:get_value(server_fullsyncs, Status) of
                    C when C >= Count ->
                        true;
                    _ ->
                        false
                end
        end),
    case node_has_version(Node, "1.2.0") of
        true ->
            ?assertEqual(ok, Res);
        _ ->
            case Res of
                ok ->
                    ok;
                _ ->
                    ?assertEqual(ok, wait_until_connection(Node)),
                    lager:warning("Pre 1.2.0 node failed to fullsync, retrying"),
                    start_and_wait_until_fullsync_complete(Node)
            end
    end,

    lager:info("Fullsync on ~p complete", [Node]).

wait_until_is_leader(Node) ->
    lager:info("wait_until_is_leader(~p)", [Node]),
    rt:wait_until(Node, fun is_leader/1).

is_leader(Node) ->
    case rpc:call(Node, riak_repl_leader, leader_node, []) of
        {badrpc, _} ->
            lager:info("Badrpc"),
            false;
        Leader ->
            lager:info("Checking: ~p =:= ~p", [Leader, Node]),
            Leader =:= Node
    end.


wait_until_is_not_leader(Node) ->
    lager:info("wait_until_is_not_leader(~p)", [Node]),
    rt:wait_until(Node, fun is_not_leader/1).

is_not_leader(Node) ->
    case rpc:call(Node, riak_repl_leader, leader_node, []) of
        {badrpc, _} ->
            lager:info("Badrpc"),
            false;
        Leader ->
            lager:info("Checking: ~p =/= ~p", [Leader, Node]),
            Leader =/= Node
    end.

wait_until_leader(Node) ->
    wait_until_new_leader(Node, undefined).

wait_until_new_leader(Node, OldLeader) ->
    Res = rt:wait_until(Node,
        fun(_) ->
                case rpc:call(Node, riak_repl_console, status, [quiet]) of
                    {badrpc, _} ->
                        false;
                    Status ->
                        case proplists:get_value(leader, Status) of
                            undefined ->
                                false;
                            OldLeader ->
                                false;
                            _Other ->
                                true
                        end
                end
        end),
    ?assertEqual(ok, Res).

wait_until_leader_converge([Node|_] = Nodes) ->
    rt:wait_until(Node,
        fun(_) ->
                length(lists:usort([begin
                        case rpc:call(N, riak_repl_console, status, [quiet]) of
                            {badrpc, _} ->
                                false;
                            Status ->
                                case proplists:get_value(leader, Status) of
                                    undefined ->
                                        false;
                                    L ->
                                        %lager:info("Leader for ~p is ~p",
                                            %[N,L]),
                                        L
                                end
                        end
                end || N <- Nodes])) == 1
        end).

wait_until_connection(Node) ->
    rt:wait_until(Node,
        fun(_) ->
                case rpc:call(Node, riak_repl_console, status, [quiet]) of
                    {badrpc, _} ->
                        false;
                    Status ->
                        case proplists:get_value(server_stats, Status) of
                            [] ->
                                false;
                            [{_, _, too_busy}] ->
                                false;
                            [_C] ->
                                true;
                            Conns ->
                                lager:warning("multiple connections detected: ~p",
                                    [Conns]),
                                true
                        end
                end
        end). %% 40 seconds is enough for repl

wait_until_no_connection(Node) ->
    rt:wait_until(Node,
        fun(_) ->
                case rpc:call(Node, riak_repl_console, status, [quiet]) of
                    {badrpc, _} ->
                        false;
                    Status ->
                        case proplists:get_value(server_stats, Status) of
                            [] ->
                                true;
                            [{_, _, too_busy}] ->
                                false;
                            [_C] ->
                                false;
                            Conns ->
                                lager:warning("multiple connections detected: ~p",
                                    [Conns]),
                                false
                        end
                end
        end). %% 40 seconds is enough for repl


wait_for_reads(Node, Start, End, Bucket, R) ->
    rt:wait_until(Node,
        fun(_) ->
                rt:systest_read(Node, Start, End, Bucket, R) == []
        end),
    Reads = rt:systest_read(Node, Start, End, Bucket, R),
    lager:info("Reads: ~p", [Reads]),
    length(Reads).

do_write(Node, Start, End, Bucket, W) ->
    case rt:systest_write(Node, Start, End, Bucket, W) of
        [] ->
            [];
        Errors ->
            lager:warning("~p errors while writing: ~p",
                [length(Errors), Errors]),
            timer:sleep(1000),
            lists:flatten([rt:systest_write(Node, S, S, Bucket, W) ||
                    {S, _Error} <- Errors])
    end.

