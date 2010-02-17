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

-module(async_phase).

-behaviour(luke_phase).

-export([init/1, handle_input/3, handle_input_done/1, handle_event/2,
         handle_timeout/1, handle_info/2, terminate/2]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_input(Inputs, State, _Timeout) ->
    {output, Inputs, State}.

handle_input_done(State) ->
    luke_phase:complete(),
    {no_output, State}.

handle_event(_Event, State) ->
    {no_output, State}.

handle_timeout(State) ->
    {no_output, State}.

handle_info(_Info, State) ->
    {no_output, State}.

terminate(_Reason, _State) ->
    ok.
