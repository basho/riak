-module(replication_upgrade).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    TestMetaData = riak_test_runner:metadata(),
    FromVersion = proplists:get_value(upgrade_version, TestMetaData, previous),

    lager:info("Doing rolling replication upgrade test from ~p to ~p",
        [FromVersion, "current"]),

    NumNodes = rt:config(num_nodes, 6),

    UpgradeOrder = rt:config(repl_upgrade_order, "forwards"),

    lager:info("Deploy ~p nodes", [NumNodes]),
    Conf = [
            {riak_repl,
             [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
             ]}
    ],

    NodeConfig = [{FromVersion, Conf} || _ <- lists:seq(1, NumNodes)],

    Nodes = rt:deploy_nodes(NodeConfig),

    NodeUpgrades = case UpgradeOrder of
        "forwards" ->
            Nodes;
        "backwards" ->
            lists:reverse(Nodes);
        "alternate" ->
            %% eg 1, 4, 2, 5, 3, 6
            lists:flatten(lists:foldl(fun(E, [A,B,C]) -> [B, C, A ++ [E]] end,
                    [[],[],[]], Nodes));
        "random" ->
            %% halfass randomization
            lists:sort(fun(_, _) -> random:uniform(100) < 50 end, Nodes);
        Other ->
            lager:error("Invalid upgrade ordering ~p", [Other]),
            erlang:exit()
    end,

    ClusterASize = rt:config(cluster_a_size, 3),
    {ANodes, BNodes} = lists:split(ClusterASize, Nodes),
    lager:info("ANodes: ~p", [ANodes]),
    lager:info("BNodes: ~p", [BNodes]),

    lager:info("Build cluster A"),
    repl_util:make_cluster(ANodes),

    lager:info("Build cluster B"),
    repl_util:make_cluster(BNodes),

    lager:info("Replication First pass...homogenous cluster"),

    %% initial replication run, homogeneous cluster
    replication:replication(ANodes, BNodes, false),

    lager:info("Upgrading nodes in order: ~p", [NodeUpgrades]),
    rt:log_to_nodes(Nodes, "Upgrading nodes in order: ~p", [NodeUpgrades]),
    %% upgrade the nodes, one at a time
    lists:foreach(fun(Node) ->
                lager:info("Upgrade node: ~p", [Node]),
                rt:log_to_nodes(Nodes, "Upgrade node: ~p", [Node]),
                rtdev:upgrade(Node, current),
                rt:wait_until_pingable(Node),
                timer:sleep(1000),
                lager:info("Replication with upgraded node: ~p", [Node]),
                rt:log_to_nodes(Nodes, "Replication with upgraded node: ~p", [Node]),
                replication:replication(ANodes, BNodes, true)
        end, NodeUpgrades).
