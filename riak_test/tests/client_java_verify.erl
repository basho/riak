-module(client_java_verify).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

%% Change when a new release comes out.
-define(JAVA_FAT_BE_URL, rt:config(java.fat_be_url)).
-define(JAVA_TESTS_URL, rt:config(java.tests_url)).

-prereq("java").
-prereq("curl").

confirm() ->
    prereqs(),
    Nodes = rt:deploy_nodes(1),
    [Node1] = Nodes,
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node1])),
    
    rpc:call(Node1, application, set_env, [erlang_js, script_timeout, 10000]), 
    [{Node1, ConnectionInfo}] = rt:connection_info([Node1]),
    {HTTP_Host, HTTP_Port} = orddict:fetch(http, ConnectionInfo),
    {PB_Host, PB_Port} = orddict:fetch(pb, ConnectionInfo),

    lager:info("Connection Info: http: ~p:~p pb: ~p:~p", [HTTP_Host, HTTP_Port, PB_Host, PB_Port]),

    java_unit_tests(HTTP_Host, HTTP_Port, PB_Host, PB_Port),
    pass.

prereqs() ->
    %% Does you have the java client available?
    rt:download(?JAVA_FAT_BE_URL),
    rt:download(?JAVA_TESTS_URL),
    ok.

java_unit_tests(HTTP_Host, HTTP_Port, _PB_Host, PB_Port) ->
    lager:info("Run the Java unit tests from somewhere on the local machine."),

    %% run the following:
    Cmd = io_lib:format(
        "java -Dcom.basho.riak.host=~s -Dcom.basho.riak.http.port=~p -Dcom.basho.riak.pbc.port=~p -cp ~s:~s org.junit.runner.JUnitCore com.basho.riak.client.AllTests",
        [HTTP_Host, HTTP_Port, PB_Port, 
        rt:config(rt_scratch_dir) ++ "/" ++ rt:url_to_filename(?JAVA_FAT_BE_URL), 
        rt:config(rt_scratch_dir) ++ "/" ++ rt:url_to_filename(?JAVA_TESTS_URL)]),
    lager:info("Cmd: ~s", [Cmd]),

    {ExitCode, JavaLog} = rt:stream_cmd(Cmd, [{cd, rt:config(rt_scratch_dir)}]),
    ?assertEqual(0, ExitCode),
    lager:info(JavaLog),
    ?assertNot(rt:str(JavaLog, "FAILURES!!!")),
    ok.
