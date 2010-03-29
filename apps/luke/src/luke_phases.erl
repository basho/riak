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

%% @doc Encapsulates the messaging protocol used during flow processing.
%%      Some of these functions operate on either a single
%%      phase pid or a list of phase pids.
%%
%%      Multiple pids are encountered when a phase has parallel
%%      instances running. This is triggerd by the 'converge'
%%      phase behavior.
-module(luke_phases).

-export([send_inputs/2,
         send_inputs_done/1,
         send_flow_complete/1]).
-export([send_flow_results/3]).

%% @doc Sends inputs to a phase process
%%      If a phase has multiple processes, inputs
%%      will be distributed in a round robin fashion.
%% @spec send_inputs(pid() | [pid()], any()) -> ok
send_inputs(PhasePids, Inputs) when is_list(PhasePids) ->
    [H|T] = PhasePids,
    send_inputs(H, Inputs),
    T ++ [H];
send_inputs(PhasePid, Inputs) when is_pid(PhasePid) ->
    gen_fsm:send_event(PhasePid, {inputs, Inputs}).

%% @doc Signals completion of inputs to a phase
%%      or a list of phases.
send_inputs_done(PhasePids) when is_list(PhasePids) ->
    lists:foreach(fun(Pid) -> send_inputs_done(Pid) end, PhasePids);
send_inputs_done(PhasePid) when is_pid(PhasePid) ->
    gen_fsm:send_event(PhasePid, inputs_done).

%% @doc Signal completion of flow to the flow pid
%%      This is sent by the last process of the last
%%      phase in the flow
%% @spec send_flow_complete(pid()) -> ok
send_flow_complete(FlowPid) ->
    gen_fsm:send_event(FlowPid, {results, done}).

%% @doc Sends flow results to the flow pid
%%      This is sent by phases which are configured
%%      to accumulate their results
%% @spec send_flow_results(pid(), any()) -> ok
send_flow_results(FlowPid, Id, Results) ->
    gen_fsm:send_event(FlowPid, {results, Id, Results}).
