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

-module(map_phase).

-behaviour(luke_phase).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_input/3, handle_input_done/1, handle_event/2,
         handle_timeout/1, handle_info/2, terminate/2]).

-record(state, {workers=[], done=false}).

init([]) ->
    {ok, #state{}}.

handle_input(Inputs, #state{workers=Workers}=State, _Timeout) ->
    Worker = worker(self(), length(Inputs)),
    {no_output, State#state{workers=[Worker|Workers]}}.

handle_input_done(#state{workers=[]}=State) ->
    luke_phase:complete(),
    {no_output, State};
handle_input_done(State) ->
    {no_output, State#state{done=true}}.

handle_event({mapexec_results, Worker, Data}, #state{done=Done, workers=Workers0}=State) ->
    Workers = lists:delete(Worker, Workers0),
    case Done =:= true andalso length(Workers) == 0 of
        true ->
            luke_phase:complete();
        false ->
            ok
    end,
    {output, Data, State#state{workers=Workers}};

handle_event(_Event, State) ->
    {no_output, State}.

handle_timeout(State) ->
    {no_output, State}.

handle_info(_Info, State) ->
    {no_output, State}.

terminate(_Reason, _State) ->
    ok.

worker(Phase, Size) ->
    spawn(fun() ->
                  Data = generate_data(Size),
                  gen_fsm:send_event(Phase, {mapexec_results, self(), Data}) end).

generate_data(Size) ->
    generate_data(Size, []).

generate_data(0, Accum) ->
    Accum;
generate_data(Size, Accum) ->
    generate_data(Size - 1, [5|Accum]).
