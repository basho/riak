%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(results_tests).

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").

all_test_() ->
    [fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?TWO_PHASE_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, ["hello"]),
             test_util:verify_results(FlowId, none),
             exit(Pid, shutdown),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?TWO_PHASE_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, ["hello"]),
             luke_flow:finish_inputs(Pid),
             {ok, Results} = test_util:verify_results(FlowId, results),
             test_util:verify_results(FlowId, done),
             ?assertMatch(["hello"], Results),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?TWO_ASYNC_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, "testing"),
             {ok, "testing"} = test_util:verify_results(FlowId, results),
             luke_flow:add_inputs(Pid, [100, 200]),
             {ok, [100, 200]} = test_util:verify_results(FlowId, results),
             luke_flow:finish_inputs(Pid),
             test_util:verify_results(FlowId, done),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?MAP_FLOW),
             Phases = test_util:verify_phases(Pid, 1),
             luke_flow:add_inputs(Pid, [a,b]),
             {ok, [5,5]} = test_util:verify_results(FlowId, results),
             test_util:verify_results(FlowId, none),
             luke_flow:add_inputs(Pid, [a,b]),
             {ok, [5,5]} = test_util:verify_results(FlowId, results),
             luke_flow:finish_inputs(Pid),
             test_util:verify_results(FlowId, done),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?MAP_FLOW),
             Phases = test_util:verify_phases(Pid, 1),
             luke_flow:add_inputs(Pid, [a,b]),
             luke_flow:add_inputs(Pid, [a,b]),
             ?assertMatch({ok, [5,5,5,5]}, luke_flow:collect_output(FlowId, 100)),
             test_util:verify_results(FlowId, none),
             luke_flow:finish_inputs(Pid),
             test_util:verify_results(FlowId, done),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             FlowId = make_ref(),
             {ok, Pid} = luke:new_flow(FlowId, ?MAP_DBL_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, [a,b]),
             luke_flow:add_inputs(Pid, [a,b]),
             ?assertMatch({ok, [5,5,5,5,5,5,5,5]}, luke_flow:collect_output(FlowId, 100)),
             test_util:verify_results(FlowId, none),
             luke_flow:finish_inputs(Pid),
             test_util:verify_results(FlowId, done),
             test_util:assertDead([Pid|Phases]) end].
