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

%% @doc Manages the execution of a flow
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

%% @doc Add inputs to the flow. Inputs will be sent to the
%%      first phase
%% @spec add_inputs(pid(), any()) -> ok
add_inputs(FlowPid, Inputs) ->
    gen_fsm:send_event(FlowPid, {inputs, Inputs}).

%% @doc Informs the phases all inputs are complete.
%% @spec finish_inputs(pid()) -> ok
finish_inputs(FlowPid) ->
    gen_fsm:send_event(FlowPid, inputs_done).

%% @doc Collects flow output. This function will block
%%      until the flow completes or exceeds the timeout.
%% @spec collect_output(any(), integer()) -> [any()] | {error, any()}
collect_output(FlowId, Timeout) ->
    collect_output(FlowId, Timeout, []).

%% @doc Returns the pids for each phase. Intended for
%%      testing only
%% @spec get_phases(pid()) -> [pid()]
get_phases(FlowPid) ->
    gen_fsm:sync_send_event(FlowPid, get_phases).

start_link(Client, FlowId, FlowDesc, Timeout) when is_list(FlowDesc),
                                                   is_pid(Client) ->
    gen_fsm:start_link(?MODULE, [Client, FlowId, FlowDesc, Timeout], []).

init([Client, FlowId, FlowDesc, Timeout]) ->
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

handle_info({'DOWN', _MRef, _Type, Pid, Reason}, StateName, #state{flow_id=FlowId, client=Client,
                                                                   fsms=FSMs, timeout=Timeout}=State) ->
    if
        Reason =:= normal ->
            {next_state, StateName, State#state{fsms=lists:delete(Pid, FSMs)}, Timeout};
        true ->
            Client ! {flow_error, FlowId, Reason},
            {stop, normal, State}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
start_phases(FlowDesc, Timeout) ->
    start_phases(lists:reverse(FlowDesc), Timeout, []).

start_phases([], _Timeout, Accum) ->
    {ok, Accum};
start_phases([{PhaseMod, Behaviors, Args}|T], Timeout, Accum) ->
    NextFSM = next_fsm(Accum),
    case proplists:get_value(converge, Behaviors) of
        undefined ->
            case luke_phase_sup:new_phase(PhaseMod, Behaviors, NextFSM, self(), Timeout, Args) of
                {ok, Pid} ->
                    erlang:monitor(process, Pid),
                    start_phases(T, Timeout, [Pid|Accum]);
                Error ->
                    Error
            end;
        InstanceCount ->
            Pids = start_converging_phases(PhaseMod, Behaviors, NextFSM, self(), Timeout, Args, InstanceCount),
            erlang:monitor(process, hd(Pids)),
            start_phases(T, Timeout, [Pids|Accum])
    end.

collect_output(FlowId, Timeout, Accum) ->
    receive
        {flow_results, FlowId, done} ->
            {ok, lists:append(lists:reverse(Accum))};
        {flow_results, FlowId, Results} ->
            collect_output(FlowId, Timeout, [Results|Accum]);
        {flow_error, FlowId, Error} ->
            Error
    after Timeout ->
            if
                length(Accum) == 0 ->
                    {error, timeout};
                true ->
                    {ok, lists:append(lists:reverse(Accum))}
            end
    end.

next_fsm(Accum) ->
 if
     length(Accum) == 0 ->
         undefined;
     true ->
         case hd(Accum) of
             P when is_pid(P) ->
                 [P];
             P ->
                 P
         end
 end.

start_converging_phases(PhaseMod, Behaviors0, NextFSM, Flow, Timeout, Args, Count) ->
    Behaviors = [normalize_behavior(B) || B <- Behaviors0],
    Pids = start_converging_phases(PhaseMod, Behaviors, NextFSM, Flow, Timeout, Args, Count, []),
    [Leader|_] = Pids,
    lists:foreach(fun(P) -> luke_phase:partners(P, Leader, Pids) end, Pids),
    Pids.

start_converging_phases(_PhaseMod, _Behaviors, _NextFSM, _Flow, _Timeout, _Args, 0, Accum) ->
    Accum;
start_converging_phases(PhaseMod, Behaviors, NextFSM, Flow, Timeout, Args, Count, Accum) ->
    case luke_phase_sup:new_phase(PhaseMod, Behaviors, NextFSM, Flow, Timeout, Args) of
        {ok, Pid} ->
            start_converging_phases(PhaseMod, Behaviors, NextFSM, Flow, Timeout, Args, Count - 1, [Pid|Accum]);
        Error ->
            throw(Error)
    end.

normalize_behavior({converge, _}) ->
    converge;
normalize_behavior(Behavior) ->
    Behavior.
