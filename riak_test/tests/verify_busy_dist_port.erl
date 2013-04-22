%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% Test for regression in riak_sysmon where busy_port/busy_dist_port were not set to 
%% true in app.config by default. Originally reported in az1018 (AgileZen 1018). 
%%
%% This test starts two riak nodes and pauses the process of one of the node's vms
%% using "kill -STOP". The other node (not paused) is then directed to send thousands
%% of messages to the paused node, which should cause busy_dist_port. We then check
%% for busy_dist_port messages in the logs. 
%% 
%% see: https://issues.basho.com/show_bug.cgi?id=1305 
%% see: https://github.com/basho/basho_expect/blob/master/basho_expect/regression_az1018.py
%% 
%% -- ORIGINAL TICKET TEXT FROM AGILE ZEN (AZ1018) --
%% As we discovered in a customer's production network, riak_sysmon has been
%% mis-configured and buggy and therefore was not logging 'busy_dist_port' events
%% when they were happening. While triaging the customer's cluster, we made
%% several mistakes while assuming that those events weren't happening.
%%
%% Two fixes are required:
%%
%% Fix the riak_sysmon_filter:init() code.
%% Tune the app.config settings to correct values.
%% 
%% -- END ORIGINAL TICKET --
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
-module(verify_busy_dist_port).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    [Node1, Node2] = rt:build_cluster(2),
    lager:info("deployed 2 nodes"),

    rt:load_modules_on_nodes([cause_bdp, verify_bdp_event_handler,
                             riak_test_lager_backend], [Node1]),
    Res = rpc:call(Node1, verify_bdp_event_handler, add_handler, [self()]),    
    ok = rpc:call(Node1, gen_event, add_handler, [lager_event, riak_test_lager_backend, [info, false]]),
    ok = rpc:call(Node1, lager, set_loglevel, [riak_test_lager_backend, info]),
    lager:info("RES: ~p", [Res]),

    OsPid = rpc:call(Node2, os, getpid, []),
    lager:info("pausing node 2 (~p) pid ~s", [Node2, OsPid]),
    %% must use cast here, call will never return
    rpc:cast(Node2, os, cmd, [lists:flatten(io_lib:format("kill -STOP ~s", [OsPid]))]),

    lager:info("flooding node 2 (paused) with messages from node 1"),
    rpc:call(Node1, cause_bdp, spam_nodes, [[Node2]]), 


    receive
        go ->
            lager:info("busy_dist_port event fired on node 1 (~p), checking logs", [Node1])
    after
        60000 ->
            lager:error("no busy_dist_port event fired on node 1 in 60s. test is borked",
                        [])
    end,

    Logs = rpc:call(Node1, riak_test_lager_backend, get_logs, []),
    Success = case re:run(Logs, "monitor busy_dist_port .*#Port", []) of
                  {match, _} ->
                      lager:info("found busy_dist_port message in log", []),
                      true;
                  nomatch -> 
                      lager:error("busy_dist_port message not found in log", []),
                      false
              end,                                    

    lager:info("continuing node 2 (~p) pid ~s", [Node2, OsPid]),
    %% NOTE: this call must be executed on the OS running Node2 in order to unpause it
    %%       and not break future test runs. The command cannot be executed via 
    %%       rpc:cast(Node2, os, cmd, ...) because Node2 is paused and will never process the 
    %%       message!
    rt:cmd(lists:flatten(io_lib:format("kill -CONT ~p", [OsPid]))),

    ?assert(Success).

