%% -------------------------------------------------------------------
%%
%% riak_sup: supervise the core Riak services
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

%% @doc supervise the core Riak services

-module(riak_kv_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define (IF (Bool, A, B), if Bool -> A; true -> B end).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    [ webmachine_router:add_route(R)
      || R <- lists:reverse(riak_kv_web:dispatch_table()) ],

    VSup = {riak_kv_vnode_sup,
            {riak_kv_vnode_sup, start_link, []},
            permanent, infinity, supervisor, [riak_kv_vnode_sup]},
    VMaster = {riak_kv_vnode_master,
               {riak_kv_vnode_master, start_link, []},
               permanent, 5000, worker, [riak_kv_vnode_master]},
    HandoffListen = {riak_kv_handoff_listener,
               {riak_kv_handoff_listener, start_link, []},
               permanent, 5000, worker, [riak_kv_handoff_listener]},
    RiakPb = [{riak_kv_pb_listener,
               {riak_kv_pb_listener, start_link, []},
               permanent, 5000, worker, [riak_kv_pb_listener]},
              {riak_kv_pb_socket_sup,
               {riak_kv_pb_socket_sup, start_link, []},
               permanent, infinity, supervisor, [riak_kv_pb_socket_sup]}
              ],
    RiakStat = {riak_kv_stat,
                {riak_kv_stat, start_link, []},
                permanent, 5000, worker, [riak_kv_stat]},
    RiakJsMgr = {riak_kv_js_manager,
                 {riak_kv_js_manager, start_link,
                  [app_helper:get_env(riak_kv, js_vm_count, 0)]},
                 permanent, 30000, worker, [riak_kv_js_manager]},
    RiakJsSup = {riak_kv_js_sup,
                 {riak_kv_js_sup, start_link, []},
                 permanent, infinity, supervisor, [riak_kv_js_sup]},
    % Figure out which processes we should run...
    IsPbConfigured = (app_helper:get_env(riak_kv, pb_ip) /= undefined)
        andalso (app_helper:get_env(riak_kv, pb_port) /= undefined),
    HasStorageBackend = (app_helper:get_env(riak_kv, storage_backend) /= undefined),
    IsStatEnabled = (app_helper:get_env(riak_kv, riak_kv_stat) == true),

    % Build the process list...
    Processes = lists:flatten([
        VSup,
        ?IF(HasStorageBackend, VMaster, []),
        HandoffListen,
        ?IF(IsPbConfigured, RiakPb, []),
        ?IF(IsStatEnabled, RiakStat, []),
        RiakJsSup,
        RiakJsMgr
    ]),

    % Run the proesses...
    {ok, {{one_for_one, 10, 10}, Processes}}.
