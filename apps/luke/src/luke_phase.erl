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

-module(luke_phase).

-behaviour(gen_fsm).

%% API
-export([start_link/6, complete/0]).

%% Behaviour
-export([behaviour_info/1]).

%% States
-export([executing/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {mod, modstate, accumulates, next_phase, flow, timeout, cb_timeout=false}).

behaviour_info(callbacks) ->
  [{init, 1},
   {handle_timeout, 1},
   {handle_input, 3},
   {handle_input_done, 1},
   {handle_event, 2},
   {handle_info, 2},
   {terminate, 2}];
behaviour_info(_) ->
    undefined.

start_link(PhaseMod, Accumulates, NextPhase, Flow, Timeout, PhaseArgs) ->
    gen_fsm:start_link(?MODULE, [PhaseMod, Accumulates, NextPhase, Flow, Timeout, PhaseArgs], []).

complete() ->
    gen_fsm:send_event(self(), complete).

init([PhaseMod, Accumulates, NextPhase, Flow, Timeout, PhaseArgs]) ->
    case PhaseMod:init(PhaseArgs) of
        {ok, ModState} ->
            erlang:monitor(process, Flow),
            {ok, executing, #state{mod=PhaseMod, modstate=ModState, next_phase=NextPhase,
                                   flow=Flow, accumulates=Accumulates, timeout=Timeout}, Timeout};
        {stop, Reason} ->
            {stop, Reason}
    end.


executing({inputs, Input}, #state{mod=PhaseMod, modstate=ModState, timeout=Timeout}=State) ->
    handle_callback(PhaseMod:handle_input(Input, ModState, Timeout), State);
executing(inputs_done, #state{mod=PhaseMod, modstate=ModState}=State) ->
    handle_callback(PhaseMod:handle_input_done(ModState), State);
executing(complete, #state{flow=Flow, next_phase=Next}=State) ->
    case Next of
        undefined ->
            luke_phases:send_flow_complete(Flow);
        _ ->
            luke_phases:send_inputs_done(Next)
    end,
    {stop, normal, State};
executing(timeout, #state{cb_timeout=true, mod=Mod, modstate=ModState}=State) ->
    handle_callback(Mod:handle_timeout(ModState), State#state{cb_timeout=false});
executing(timeout, #state{cb_timeout=false}=State) ->
    {stop, normal, State};
executing(Event, #state{mod=PhaseMod, modstate=ModState}=State) ->
    handle_callback(PhaseMod:handle_event(Event, ModState), State).

handle_event(_Event, StateName, #state{timeout=Timeout}=State) ->
    {next_state, StateName, State, Timeout}.

handle_sync_event(_Event, _From, StateName, #state{timeout=Timeout}=State) ->
    {reply, ignored, StateName, State, Timeout}.

handle_info({'DOWN', _MRef, _Type, Flow, _Info}, _StateName, #state{flow=Flow}=State) ->
    {stop, normal, State};
handle_info(timeout, executing, #state{cb_timeout=true, mod=Mod, modstate=ModState}=State) ->
    handle_callback(Mod:handle_timeout(ModState), State#state{cb_timeout=false});
handle_info(timeout, executing, #state{flow=Flow}=State) ->
    luke_phases:send_flow_results(Flow, {error, timeout}),
    luke_phases:send_flow_complete(Flow),
    {stop, normal, State};
handle_info(Info, _StateName, #state{mod=PhaseMod, modstate=ModState}=State) ->
    handle_callback(PhaseMod:handle_info(Info, ModState), State).

terminate(Reason, _StateName, #state{mod=PhaseMod, modstate=ModState}) ->
    PhaseMod:terminate(Reason, ModState),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
handle_callback({no_output, NewModState}, #state{timeout=Timeout}=State) ->
    {next_state, executing, State#state{modstate=NewModState}, Timeout};
handle_callback({no_output, NewModState, TempTimeout}, #state{timeout=Timeout}=State) when TempTimeout < Timeout ->
    io:format("TempTimeout: ~p~n", [TempTimeout]),
    {next_state, executing, State#state{modstate=NewModState, cb_timeout=true}, TempTimeout};
handle_callback({output, Output, NewModState}, #state{timeout=Timeout}=State) ->
    route_output(Output, State),
    {next_state, executing, State#state{modstate=NewModState}, Timeout};
handle_callback({output, Output, NewModState, TempTimeout}, #state{timeout=Timeout}=State) when TempTimeout < Timeout ->
    io:format("TempTimeout: ~p~n", [TempTimeout]),
    route_output(Output, State),
    {next_state, executing, State#state{modstate=NewModState, cb_timeout=true}, TempTimeout};
handle_callback({stop, Reason, NewModState}, State) ->
    {stop, Reason, State#state{modstate=NewModState}}.

route_output(Output, #state{next_phase=Next, flow=Flow, accumulates=Accumulates}) ->
    propagate_inputs(Next, Output),
    if
        Accumulates =:= true ->
            luke_phases:send_flow_results(Flow, Output);
        true ->
            ok
    end.

propagate_inputs(undefined, _Results) ->
    ok;
propagate_inputs(Next, Results) ->
    luke_phases:send_inputs(Next, Results).
