%% -------------------------------------------------------------------
%%
%% riak_core: Core Riak Application
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(riak_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RiakWeb = {webmachine_mochiweb,
                 {webmachine_mochiweb, start, [riak_core_web:config()]},
                  permanent, 5000, worker, dynamic},
    IsWebConfigured = (app_helper:get_env(riak_core, web_ip) /= undefined)
        andalso (app_helper:get_env(riak_core, web_port) /= undefined),

    Children = lists:flatten(
                 [?CHILD(riak_core_ring_events, worker),
                  ?CHILD(riak_core_ring_manager, worker),
                  ?CHILD(riak_core_gossip, worker),
                  ?IF(IsWebConfigured, RiakWeb, [])
                 ]),

    {ok, {{one_for_one, 10, 10}, Children}}.


