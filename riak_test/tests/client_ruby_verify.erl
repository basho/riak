-module(client_ruby_verify).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-prereq("ruby").
-prereq("gem").

%% @todo Only Memory backend is supported

confirm() ->
    prereqs(),
    GemDir = dat_gem(),
    Nodes = rt:deploy_nodes([{current,
                              [{riak_kv, [{test, true},
                                          {add_paths, [filename:join([GemDir, "erl_src"])]}]},
                               {riak_search, [{enabled, true},
                                              {search_backend, riak_search_test_backend}]}]}],
                            [riak_search]),
    [Node1] = Nodes,

    [{Node1, ConnectionInfo}] = rt:connection_info([Node1]),
    {_HTTP_Host, HTTP_Port} = orddict:fetch(http, ConnectionInfo),
    {_PB_Host, PB_Port} = orddict:fetch(pb, ConnectionInfo),

    %% Ruby Client Tests require a path to riak, to grab the PIPE_DIR
    %% from riak && riak-admin. Work will need to be done on the
    %% ruby-client side to enable this test to work with remote nodes.
    %% Fortunately for us, riak_test does not support remote nodes, so
    %% all is fine in ruby land... for now.
    %% That's why I'm calling rtdev directly, violating a cardinal rule
    %% of riak_test.
    RiakRootDir = rtdev:node_path(Node1),

    Cmd = "bin/rspec --profile --tag integration --tag \~nodegen --no-color -fd",

    lager:info("Cmd: ~s", [Cmd]),

    {Code, RubyLog} = rt:stream_cmd(Cmd, [{cd, GemDir}, {env, [
            {"RIAK_NODE_NAME", atom_to_list(Node1)},
            {"RIAK_ROOT_DIR", RiakRootDir},
            {"HTTP_PORT", integer_to_list(HTTP_Port)},
            {"PB_PORT", integer_to_list(PB_Port)},
            {"RIAK_VERSION", binary_to_list(rt:get_version())}
        ]}]),
    ?assert(rt:str(RubyLog, " 0 failures")),
    ?assert(Code =:= 0),
    pass.

prereqs() ->
    lager:info("Checking ruby version is 1.8.7 or 1.9.*"),
    RubyVersion = os:cmd("ruby -v"),
    ?assert(rt:str(RubyVersion, "1.9.") orelse rt:str(RubyVersion, "1.8.7")),

    rt:install_on_absence("bundle", "gem install bundler --no-rdoc --no-ri"),
    ok.


% Download the ruby-client gem, unpack it and build it locally
dat_gem() ->
    lager:info("Fetching riak-client gem"),
    GemInstalled = os:cmd("cd " ++ rt:config(rt_scratch_dir) ++ " ; gem fetch riak-client"),
    GemFile = string:substr(GemInstalled, 12, length(GemInstalled) - 12),
    %GemFile = "riak-client",
    lager:info("Downloaded gem: ~s", [GemFile]),

    rt:stream_cmd(io_lib:format("gem unpack ~s.gem", [GemFile]), [{cd, rt:config(rt_scratch_dir)}]),

    Cmd = "bundle install --without=guard --binstubs --no-color --path=vendor/bundle",
    lager:info(Cmd),

    GemDir = filename:join([rt:config(rt_scratch_dir), GemFile]),

    {_Exit, _Log} = rt:stream_cmd(Cmd, [{cd, GemDir}, {env, [{"BUNDLE_PATH", "vendor/bundle"}]}]),
    GemDir.
