%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(jmx_verify).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-prereq("java").

%% You should have curl installed locally to do this.
confirm() ->
    JMXPort = 41111,
    Config = [{riak_jmx, [{enabled, true}, {port, JMXPort}]}],
    Nodes = rt:deploy_nodes(1, Config),
    [Node1] = Nodes,
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node1])),

    {ok, [{IP, _Port}|_]} = rpc:call(Node1, application, get_env, [riak_core, http]),

    JMXDumpCmd = jmx_dump_cmd(IP, JMXPort),

    JMX1 = jmx_dump(JMXDumpCmd),

    %% make sure a set of stats have valid values
    verify_nz(JMX1, [<<"cpu_nprocs">>,
                     <<"mem_total">>,
                     <<"mem_allocated">>,
                     <<"ring_creation_size">>]),

    lager:info("perform 5 x  PUT and a GET to increment the stats"),
    lager:info("as the stat system only does calcs for > 5 readings"),
    
    C = rt:httpc(Node1),
    [rt:httpc_write(C, <<"systest">>, <<X>>, <<"12345">>) || X <- lists:seq(1, 5)],
    [rt:httpc_read(C, <<"systest">>, <<X>>) || X <- lists:seq(1, 5)],

    JMX2 = jmx_dump(JMXDumpCmd),
    %% make sure the stats that were supposed to increment did
    verify_inc(JMX1, JMX2, [{<<"node_gets">>, 10},
                            {<<"node_puts">>, 5},
                            {<<"node_gets_total">>, 10},
                            {<<"node_puts_total">>, 5},
                            {<<"vnode_gets">>, 30},
                            {<<"vnode_puts">>, 15},
                            {<<"vnode_gets_total">>, 30},
                            {<<"vnode_puts_total">>, 15}]),

    %% verify that fsm times were tallied
    verify_nz(JMX2, [<<"node_get_fsm_time_mean">>,
                     <<"node_get_fsm_time_median">>,
                     <<"node_get_fsm_time_95">>,
                     <<"node_get_fsm_time_99">>,
                     <<"node_get_fsm_time_100">>,
                     <<"node_put_fsm_time_mean">>,
                     <<"node_put_fsm_time_median">>,
                     <<"node_put_fsm_time_95">>,
                     <<"node_put_fsm_time_99">>,
                     <<"node_put_fsm_time_100">>]),

    lager:info("Make PBC Connection"),
    Pid = rt:pbc(Node1),

    JMX3 = jmx_dump(JMXDumpCmd),
    rt:systest_write(Node1, 1),
    %% make sure the stats that were supposed to increment did
    verify_inc(JMX2, JMX3, [{<<"pbc_connects_total">>, 1},
                            {<<"pbc_connects">>, 1},
                            {<<"pbc_active">>, 1}]),

    lager:info("Force Read Repair"),
    rt:pbc_write(Pid, <<"testbucket">>, <<"1">>, <<"blah!">>),
    rt:pbc_set_bucket_prop(Pid, <<"testbucket">>, [{n_val, 4}]),

    JMX4 = jmx_dump(JMXDumpCmd),

    verify_inc(JMX3, JMX4, [{<<"read_repairs_total">>, 0},
                            {<<"read_repairs">>, 0}]),

    _Value = rt:pbc_read(Pid, <<"testbucket">>, <<"1">>),

    %%Stats5 = get_stats(Node1),
    JMX5 = jmx_dump(JMXDumpCmd),
    verify_inc(JMX3, JMX5, [{<<"read_repairs_total">>, 1},
                            {<<"read_repairs">>, 1}]),
    pass.

verify_inc(Prev, Props, Keys) ->
    [begin
         Old = proplists:get_value(Key, Prev, 0),
         New = proplists:get_value(Key, Props, 0),
         lager:info("~s: ~p -> ~p (expected ~p)", [Key, Old, New, Old + Inc]),
         ?assertEqual({Key, New}, {Key, (Old + Inc)})
     end || {Key, Inc} <- Keys].

verify_nz(Props, Keys) ->
    [?assertNotEqual({Key, proplists:get_value(Key,Props,0)}, {Key, 0}) || Key <- Keys].

jmx_jar_path() ->
    %% Find riak_jmx.jar
    DepsPath = rt:get_deps(),
    Deps = string:tokens(os:cmd("ls " ++ DepsPath), "\n"),
    [RiakJMX] = lists:filter(fun(X) -> string:str(X, "riak_jmx") == 1 end, Deps),
    filename:join([DepsPath, RiakJMX, "priv", "riak_jmx.jar"]).

jmx_dump_cmd(IP, Port) ->
    io_lib:format("java -cp ~s com.basho.riak.jmx.Dump ~s ~p", 
        [jmx_jar_path(), IP, Port]).

jmx_dump(Cmd) ->
    timer:sleep(40000), %% JMX only updates every 30seconds
    Output = string:strip(os:cmd(Cmd), both, $\n),
    JSONOutput = mochijson2:decode(Output),
    [ {process_key(Key), Value} || {struct, [{Key, Value}]} <- JSONOutput].

process_key(<<"CPUNProcs">>) -> <<"cpu_nprocs">>;
process_key(<<"MemAllocated">>) -> <<"mem_allocated">>;
process_key(<<"MemTotal">>) -> <<"mem_total">>;
process_key(<<"NodeGets">>) -> <<"node_gets">>;
process_key(<<"NodeGetsTotal">>) -> <<"node_gets_total">>;
process_key(<<"NodePuts">>) -> <<"node_puts">>;
process_key(<<"NodePutsTotal">>) -> <<"node_puts_total">>;
process_key(<<"VnodeGets">>) -> <<"vnode_gets">>;
process_key(<<"VnodeGetsTotal">>) -> <<"vnode_gets_total">>;
process_key(<<"VnodePuts">>) -> <<"vnode_puts">>;
process_key(<<"VnodePutsTotal">>) -> <<"vnode_puts_total">>;
process_key(<<"PbcActive">>) -> <<"pbc_active">>;
process_key(<<"PbcConnects">>) -> <<"pbc_connects">>;
process_key(<<"PbcConnectsTotal">>) -> <<"pbc_connects_total">>;
process_key(<<"NodeName">>) -> <<"nodename">>;
process_key(<<"RingCreationSize">>) -> <<"ring_creation_size">>;
process_key(<<"CpuAvg1">>) -> <<"cpu_avg1">>;
process_key(<<"CpuAvg5">>) -> <<"cpu_avg5">>;
process_key(<<"CpuAvg15">>) -> <<"cpu_avg15">>;
process_key(<<"NodeGetFsmTime95">>) -> <<"node_get_fsm_time_95">>;
process_key(<<"NodeGetFsmTime99">>) -> <<"node_get_fsm_time_99">>;
process_key(<<"NodeGetFsmTimeMax">>) -> <<"node_get_fsm_time_100">>;
process_key(<<"NodeGetFsmTimeMean">>) -> <<"node_get_fsm_time_mean">>;
process_key(<<"NodeGetFsmTimeMedian">>) -> <<"node_get_fsm_time_median">>;
process_key(<<"NodePutFsmTime95">>) -> <<"node_put_fsm_time_95">>;
process_key(<<"NodePutFsmTime99">>) -> <<"node_put_fsm_time_99">>;
process_key(<<"NodePutFsmTimeMax">>) -> <<"node_put_fsm_time_100">>;
process_key(<<"NodePutFsmTimeMean">>) -> <<"node_put_fsm_time_mean">>;
process_key(<<"NodePutFsmTimeMedian">>) -> <<"node_put_fsm_time_median">>;
process_key(<<"ReadRepairs">>) -> <<"read_repairs">>;
process_key(<<"ReadRepairsTotal">>) -> <<"read_repairs_total">>.
