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

-module(luke_flow).

-behaviour(gen_fsm).

%% API
-export([start_link/4, add_inputs/2, finish_inputs/1, collect_output/2]).

%% FSM states
-export([get_phases/1, executing/2, executing/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {flow_id, fsms, client, timeout, results=[]}).

add_inputs(FlowPid, Inputs) ->
    gen_fsm:send_event(FlowPid, {inputs, Inputs}).

finish_inputs(FlowPid) ->
    gen_fsm:send_event(FlowPid, inputs_done).

collect_output(FlowId, Timeout) ->
    collect_output(FlowId, Timeout, []).

get_phases(FlowPid) ->
    gen_fsm:sync_send_event(FlowPid, get_phases).

start_link(Client, FlowId, FlowDesc, Timeout) when is_list(FlowDesc),
                                                   is_pid(Client) ->
    gen_fsm:start_link(?MODULE, [Client, FlowId, FlowDesc, Timeout], []).

init([Client, FlowId, FlowDesc, Timeout0]) ->
    Timeout = erlang:trunc(Timeout0 * 1.1),
    case start_phases(FlowDesc, Timeout) of
        {ok, FSMs} ->
            {ok, executing, #state{fsms=FSMs, flow_id=FlowId, timeout=Timeout, client=Client}, Timeout};
        Error ->
            {stop, Error}
    end.

executing({inputs, Inputs}, #state{fsms=[H|_], timeout=Timeout}=State) ->
    luke_phases:send_inputs(H, Inputs),
    {next_state, executing, State, Timeout};
executing(inputs_done, #state{fsms=[H|_], timeout=Timeout}=State) ->
    luke_phases:send_inputs_done(H),
    {next_state, executing, State, Timeout};
executing(timeout, #state{client=Client, flow_id=FlowId}=State) ->
    Client ! {flow_results, FlowId, done},
    {stop, normal, State};
executing({results, done}, #state{client=Client, flow_id=FlowId}=State) ->
    Client ! {flow_results, FlowId, done},
    {stop, normal, State};
executing({results, Result}, #state{client=Client, flow_id=FlowId, timeout=Timeout}=State) ->
    Client ! {flow_results, FlowId, Result},
    {next_state, executing, State, Timeout}.

executing(get_phases, _From, #state{fsms=FSMs}=State) ->
    {reply, FSMs, executing, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.

handle_info({'DOWN', _MRef, _Type, Pid, Reason}, StateName, #state{client=Client, fsms=FSMs, timeout=Timeout}=State) ->
    case lists:member(Pid, FSMs) of
        false ->
            {next_state, StateName, State, Timeout};
        true ->
            if
                Reason =:= normal ->
                    {next_state, StateName, State#state{fsms=lists:delete(Pid, FSMs)}, Timeout};
                true ->
                    Client ! {flow_error, Reason},
                    {stop, normal, State}
            end
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
start_phases(FlowDesc, Timeout) ->
    PerPhaseTimeout = erlang:trunc(Timeout / length(FlowDesc)),
    start_phases(lists:reverse(FlowDesc), PerPhaseTimeout, []).

start_phases([], _Timeout, Accum) ->
    {ok, Accum};
start_phases([{PhaseMod, Accumulates, Args}|T], Timeout, Accum) ->
    NextFSM = if
                  length(Accum) == 0 ->
                      undefined;
                  true ->
                      hd(Accum)
              end,
    case luke_phase_sup:new_phase(PhaseMod, Accumulates, NextFSM, self(), Timeout, Args) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            start_phases(T, Timeout, [Pid|Accum]);
        Error ->
            Error
    end.

collect_output(FlowId, Timeout, Accum) ->
    receive
        {flow_results, FlowId, done} ->
            {ok, lists:flatten(lists:reverse(Accum))};
        {flow_results, FlowId, Results} ->
            collect_output(FlowId, Timeout, [Results|Accum])
    after Timeout ->
            if
                length(Accum) == 0 ->
                    {error, timeout};
                true ->
                    {ok, lists:flatten(lists:reverse(Accum))}
            end
    end.
