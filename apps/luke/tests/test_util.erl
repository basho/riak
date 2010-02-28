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

-module(test_util).

-include_lib("eunit/include/eunit.hrl").

-export([start_flow/1, verify_phases/2, verify_results/2, assertDead/1]).

start_flow(FlowDesc) ->
    FlowId = make_ref(),
    {ok, Pid} = luke:new_flow(FlowId, FlowDesc),
    Phases = test_util:verify_phases(Pid, length(FlowDesc)),
    {FlowId, Pid, Phases}.


verify_phases(Pid, Size) ->
    Phases = luke_flow:get_phases(Pid),
    ?assertEqual(Size, length(Phases)),
    Phases.

verify_results(FlowId, none) ->
    receive
        {flow_results, FlowId, done} ->
            throw({error, unexpected_done});
        {flow_results, FlowId, Results} ->
            throw({error, unexpected_results, Results})
    after 100 ->
            ok
    end;
verify_results(FlowId, results) ->
    receive
        {flow_results, FlowId, done} ->
            throw({error, unexpected_done});
        {flow_results, FlowId, Results} ->
            {ok, Results}
    after 100 ->
            throw({error, no_results})
    end;
verify_results(FlowId, done) ->
    receive
        {flow_results, FlowId, done} ->
            ok;
        {flow_results, FlowId, Results} ->
            throw({error, unexpected_results, Results})
    after 100 ->
            throw({error, no_results})
    end.

assertDead(Pids)->
    timer:sleep(25),
    assertDead0(Pids).

assertDead0([]) ->
    ok;
assertDead0([H|T]) when is_list(H) ->
    ok = assertDead0(H),
    assertDead0(T);
assertDead0([H|T]) when is_pid(H) ->
    ?assertMatch(false, erlang:is_process_alive(H)),
    assertDead0(T).
