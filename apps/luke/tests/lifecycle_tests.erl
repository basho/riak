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

-module(lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").

setup_teardown_test_() ->
    [fun() ->
             %% Startup/teardown, no input
             {ok, Pid} = luke:new_flow(make_ref(), ?TWO_PHASE_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             exit(Pid, shutdown),
             timer:sleep(10),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             %% Startup/teardown, input w/no end
             {ok, Pid} = luke:new_flow(make_ref(), ?TWO_PHASE_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, [100]),
             exit(Pid, shutdown),
             timer:sleep(10),
             test_util:assertDead([Pid|Phases]) end,
     fun() ->
             %% Startup/teardown, input w/finish
             {ok, Pid} = luke:new_flow(make_ref(), ?TWO_PHASE_FLOW),
             Phases = test_util:verify_phases(Pid, 2),
             luke_flow:add_inputs(Pid, [100]),
             luke_flow:add_inputs(Pid, [200]),
             luke_flow:finish_inputs(Pid),
             exit(Pid, shutdown),
             timer:sleep(10),
             test_util:assertDead([Pid|Phases]) end].
