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

-module(luke_flow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_flow/4,
         new_flow/5]).

%% Supervisor callbacks
-export([init/1]).

new_flow(Client, FlowId, FlowDesc, Timeout) ->
    start_child(Client, FlowId, FlowDesc, Timeout).

new_flow(Node, Client, FlowId, FlowDesc, Timeout) ->
    start_child(Node, Client, FlowId, FlowDesc, Timeout).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    Process = {undefined,
               {luke_flow, start_link, []},
               temporary, brutal_kill, worker, dynamic},
    {ok, {SupFlags, [Process]}}.

%% Internal functions
start_child(Client, FlowId, FlowDesc, Timeout) ->
    supervisor:start_child(?MODULE, [Client, FlowId, FlowDesc, Timeout]).
start_child(Node, Client, FlowId, FlowDesc, Timeout) ->
    supervisor:start_child({?MODULE, Node}, [Client, FlowId, FlowDesc, Timeout]).
