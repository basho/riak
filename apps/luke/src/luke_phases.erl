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

-module(luke_phases).

-export([send_inputs/2, send_inputs_done/1, send_flow_complete/1]).
-export([send_phase_complete/1, send_flow_results/2]).

send_inputs(PhasePid, Inputs) ->
    gen_fsm:send_event(PhasePid, {inputs, Inputs}).

send_inputs_done(PhasePid) ->
    gen_fsm:send_event(PhasePid, inputs_done).

send_phase_complete(PhasePid) ->
    gen_fsm:send_event(PhasePid, phase_complete).

send_flow_complete(FlowPid) ->
    gen_fsm:send_event(FlowPid, {results, done}).

send_flow_results(FlowPid, Results) ->
    gen_fsm:send_event(FlowPid, {results, Results}).
