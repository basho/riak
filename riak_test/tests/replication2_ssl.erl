-module(replication2_ssl).
-behavior(riak_test).
-export([confirm/0]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    NumNodes = rt_config:get(num_nodes, 6),
    ClusterASize = rt_config:get(cluster_a_size, 3),

    lager:info("Deploy ~p nodes", [NumNodes]),
    BaseConf = [
        {riak_core,
            [
                {ssl_enabled, false}
            ]},
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]}
    ],

    PrivDir = rt:priv_dir(),
    
    SSLConfig1 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {certfile, filename:join([PrivDir,
                            "certs/selfsigned/site1-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/selfsigned/site1-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/selfsigned/ca"])}
            ]}
    ],

    SSLConfig2 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {certfile, filename:join([PrivDir,
                            "certs/selfsigned/site2-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/selfsigned/site2-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/selfsigned/ca"])}
            ]}
    ],

    SSLConfig3 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {certfile, filename:join([PrivDir,
                            "certs/selfsigned/site3-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/selfsigned/site3-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/selfsigned/ca"])}
            ]}
    ],

    %% same as above,with a depth of 0
    SSLConfig3A = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [

                {ssl_enabled, true},
                {ssl_depth, 0},
                {certfile, filename:join([PrivDir,
                            "certs/selfsigned/site3-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/selfsigned/site3-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/selfsigned/ca"])}
            ]}
    ],

    SSLConfig4 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {ssl_depth, 0},
                {certfile, filename:join([PrivDir,
                            "certs/selfsigned/site4-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/selfsigned/site4-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/selfsigned/ca"])}
            ]}
    ],

    SSLConfig5 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {peer_common_name_acl, ["*.cataclysm-software.net"]},
                {certfile, filename:join([PrivDir,
                            "certs/cacert.org/ca-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/cacert.org/ca-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/cacert.org/ca"])}
            ]}
    ],

    SSLConfig6 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {peer_common_name_acl, ["ca.cataclysm-software.net"]},
                {certfile, filename:join([PrivDir,
                            "certs/cacert.org/ny-cert.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/cacert.org/ny-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/cacert.org/ca"])}
            ]}
    ],

    SSLConfig7 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
            ]},
        {riak_core,
            [
                {ssl_enabled, true},
                {peer_common_name_acl, ["ca.cataclysm-software.net"]},
                {certfile, filename:join([PrivDir,
                            "certs/cacert.org/ny-cert-old.pem"])},
                {keyfile, filename:join([PrivDir,
                            "certs/cacert.org/ny-key.pem"])},
                {cacertdir, filename:join([PrivDir,
                            "certs/cacert.org/ca"])}
            ]}
    ],


    [Node1, Node2] = rt:deploy_nodes(2, BaseConf),

    repl_util:name_cluster(Node1, "A"),
    repl_util:name_cluster(Node2, "B"),

    %% we'll need to wait for cluster names before continuing
    rt:wait_until_ring_converged([Node1]),
    rt:wait_until_ring_converged([Node2]),

    rt:wait_for_service(Node1, riak_repl),
    rt:wait_for_service(Node2, riak_repl),

    {ok, {_IP, Port}} = rpc:call(Node2, application, get_env,
        [riak_core, cluster_mgr]),

    lager:info("connect cluster A:~p to B on port ~p", [Node1, Port]),
    repl_util:connect_cluster(Node1, "127.0.0.1", Port),
    ?assertEqual(ok, repl_util:wait_for_connection(Node1, "B")),

    lager:info("===testing basic connectivity"),
    rt:log_to_nodes([Node1, Node2], "Basic connectivity test"),
    ?assertEqual(ok, test_connection({Node1, BaseConf}, {Node2, BaseConf})),

    lager:info("===testing you can't connect to a server with a cert with the same common name"),
    rt:log_to_nodes([Node1, Node2], "Testing identical cert is disallowed"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig1, BaseConf)})),

    lager:info("===testing you can't connect when peer doesn't support SSL"),
    rt:log_to_nodes([Node1, Node2], "Testing missing ssl on peer fails"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, BaseConf})),

    lager:info("===testing you can't connect when local doesn't support SSL"),
    rt:log_to_nodes([Node1, Node2], "Testing missing ssl locally fails"),
    ?assertEqual(fail, test_connection({Node1, BaseConf},
            {Node2, merge_config(SSLConfig2, BaseConf)})),

    lager:info("===testing simple SSL connectivity"),
    rt:log_to_nodes([Node1, Node2], "Basic SSL test"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig2, BaseConf)})),

    lager:info("testing SSL connectivity with an intermediate CA"),
    rt:log_to_nodes([Node1, Node2], "Intermediate CA test"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig3, BaseConf)})),

    lager:info("===testing disallowing intermediate CAs works"),
    rt:log_to_nodes([Node1, Node2], "Disallowing intermediate CA test"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig3A, BaseConf)},
            {Node2, merge_config(SSLConfig4, BaseConf)})),

    lager:info("===testing disallowing intermediate CAs disallows connections"),
    rt:log_to_nodes([Node1, Node2], "Disallowing intermediate CA test 2"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig3A, BaseConf)},
            {Node2, merge_config(SSLConfig1, BaseConf)})),

    lager:info("===testing wildcard and strict ACLs with cacert.org certs"),
    rt:log_to_nodes([Node1, Node2], "wildcard and strict ACL test"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig5, BaseConf)},
            {Node2, merge_config(SSLConfig6, BaseConf)})),

    lager:info("===testing expired certificates fail"),
    rt:log_to_nodes([Node1, Node2], "expired certificates test"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig5, BaseConf)},
            {Node2, merge_config(SSLConfig7, BaseConf)})),

    lager:info("Connectivity tests passed"),

    repl_util:disconnect_cluster(Node1, "B"),

    lager:info("Re-deploying 6 nodes"),

    Nodes = rt:deploy_nodes(6, BaseConf),

    [rt:wait_until_pingable(N) || N <- Nodes],

    {ANodes, BNodes} = lists:split(ClusterASize, Nodes),

    lager:info("Reconfiguring nodes with SSL options"),
    [rt:update_app_config(N, merge_config(SSLConfig5, BaseConf)) || N <-
        ANodes],

    [rt:update_app_config(N, merge_config(SSLConfig6, BaseConf)) || N <-
        BNodes],

    [rt:wait_until_pingable(N) || N <- Nodes],

    lager:info("Build cluster A"),
    repl_util:make_cluster(ANodes),

    lager:info("Build cluster B"),
    repl_util:make_cluster(BNodes),

    repl_util:disconnect_cluster(Node1, "B"),

    replication2:replication(ANodes, BNodes, false),

    pass.

merge_config(Mixin, Base) ->
    lists:ukeymerge(1, lists:keysort(1, Mixin), lists:keysort(1, Base)).

test_connection({Node1, Config1}, {Node2, Config2}) ->
    rt:update_app_config(Node1, Config1),
    rt:wait_until_pingable(Node1),
    rt:update_app_config(Node2, Config2),
    rt:wait_until_pingable(Node2),
    timer:sleep(5000),
    repl_util:wait_for_connection(Node1, "B").




