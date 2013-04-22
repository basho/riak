-module(replication_ssl).
-behavior(riak_test).
-export([confirm/0]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    NumNodes = rt:config(num_nodes, 6),
    ClusterASize = rt:config(cluster_a_size, 3),

    lager:info("Deploy ~p nodes", [NumNodes]),
    BaseConf = [
            {riak_repl,
             [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled}
             ]}
    ],

    %% XXX for some reason, codew:priv_dir returns riak_test/riak_test/priv,
    %% which is wrong, so fix it.
    PrivDir = re:replace(code:priv_dir(riak_test), "riak_test(/riak_test)*",
        "riak_test", [{return, list}]),

    ?assert(filelib:is_dir(PrivDir)),


    lager:info("priv dir: ~p -> ~p", [code:priv_dir(riak_test), PrivDir]),

    SSLConfig1 = [
        {riak_repl,
            [
                {fullsync_on_connect, false},
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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
                {fullsync_interval, disabled},
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

    Listeners = replication:add_listeners([Node1]),
    replication:verify_listeners(Listeners),

    {Ip, Port, _} = hd(Listeners),
    replication:add_site(Node2, {Ip, Port, "site1"}),

    replication:verify_site_ips(Node2, "site1", Listeners),

    lager:info("testing basic connectivity"),
    ?assertEqual(ok, test_connection({Node1, BaseConf}, {Node2, BaseConf})),

    lager:info("testing you can't connect to a server with a cert with the same common name"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig1, BaseConf)})),

    lager:info("testing simple SSL connectivity"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig2, BaseConf)})),

    lager:info("testing SSL connectivity with an intermediate CA"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig1, BaseConf)},
            {Node2, merge_config(SSLConfig3, BaseConf)})),

    lager:info("testing disallowing intermediate CAs works"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig3A, BaseConf)},
            {Node2, merge_config(SSLConfig4, BaseConf)})),

    lager:info("testing disallowing intermediate CAs disallows connections"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig3A, BaseConf)},
            {Node2, merge_config(SSLConfig1, BaseConf)})),

    lager:info("testing wildcard and strict ACLs with cacert.org certs"),
    ?assertEqual(ok, test_connection({Node1, merge_config(SSLConfig5, BaseConf)},
            {Node2, merge_config(SSLConfig6, BaseConf)})),

    lager:info("testing expired certificates fail"),
    ?assertEqual(fail, test_connection({Node1, merge_config(SSLConfig5, BaseConf)},
            {Node2, merge_config(SSLConfig7, BaseConf)})),

    lager:info("Connectivity tests passed"),

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

    replication:replication(ANodes, BNodes, false),

    pass.

merge_config(Mixin, Base) ->
    lists:ukeymerge(1, lists:keysort(1, Mixin), lists:keysort(1, Base)).

test_connection({Node1, Config1}, {Node2, Config2}) ->
    rt:update_app_config(Node1, Config1),
    rt:wait_until_pingable(Node1),
    rt:update_app_config(Node2, Config2),
    rt:wait_until_pingable(Node2),
    timer:sleep(5000),
    replication:wait_until_connection(Node1).




