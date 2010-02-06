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

-module(luke).

-behaviour(application).

-define(DEFAULT_TIMEOUT, 60000).

%% Application callbacks
-export([start/0, start/2, stop/1, new_flow/2, new_flow/3, new_flow/4]).

new_flow(FlowId, FlowDesc) ->
    new_flow(self(), FlowId, FlowDesc, ?DEFAULT_TIMEOUT).

new_flow(Client, FlowId, FlowDesc) ->
    new_flow(Client, FlowId, FlowDesc, ?DEFAULT_TIMEOUT).

new_flow(Client, FlowId, FlowDesc, Timeout) ->
    luke_flow_sup:new_flow(Client, FlowId, FlowDesc, Timeout).

start() ->
    application:start(luke).

start(_StartType, _StartArgs) ->
    luke_sup:start_link().

stop(_State) ->
    ok.
