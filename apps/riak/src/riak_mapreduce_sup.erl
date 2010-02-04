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
-module(riak_mapreduce_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_mapreduce_fsm/5]).

%% Supervisor callbacks
-export([init/1]).

new_mapreduce_fsm(Node, ReqId, Query, Timeout, Requestor) ->
    case supervisor:start_child({?MODULE, Node},  [riak_mapreduce_fsm, [ReqId, Query,
                                                                        Timeout, Requestor], []]) of
        {ok, Pid} ->
            {ok, {ReqId, Pid}};
        Error ->
            Error
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    Process = {undefined,
               {gen_fsm, start_link, []},
               temporary, brutal_kill, worker, dynamic},
    {ok, {SupFlags, [Process]}}.
