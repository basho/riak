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

-module(riak_sup).

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
    Eventer = {riak_eventer,
               {riak_eventer, start_link, []},
               permanent, 5000, worker, [riak_eventer]},
    VSup = {riak_vnode_sup,
            {riak_vnode_sup, start_link, []},
            permanent, infinity, supervisor, [riak_vnode_sup]},
    VMaster = {riak_vnode_master,
               {riak_vnode_master, start_link, []},
               permanent, 5000, worker, [riak_vnode_master]},
    HandoffListen = {riak_handoff_listener,
               {riak_handoff_listener, start_link, []},
               permanent, 5000, worker, [riak_handoff_listener]},
    RingMgr = {riak_ring_manager,
             {riak_ring_manager, start_link, []},
             permanent, 5000, worker, [riak_ring_manager]},
    Connect = {riak_connect,
             {riak_connect, start_link, []},
             permanent, 5000, worker, [riak_connect]},
    LocalLogger = {riak_local_logger,
                   {riak_local_logger, start_link, []},
                   permanent, 5000, worker, [riak_local_logger]},
    RiakWeb = {webmachine_mochiweb,
                 {webmachine_mochiweb, start, [riak_web:config()]},
                  permanent, 5000, worker, dynamic},
    RiakStat = {riak_stat,
                {riak_stat, start_link, []},
                permanent, 5000, worker, [riak_stat]},
    RiakJsMgr = {riak_js_manager,
                 {riak_js_manager, start_link, [riak:get_app_env(js_vm_count, 0)]},
                 permanent, 30000, worker, [riak_js_manager]},
    RiakJsSup = {riak_js_sup,
                 {riak_js_sup, start_link, []},
                 permanent, infinity, supervisor, [riak_js_sup]},
    MapReduceFSMSup = {riak_mapreduce_sup,
                       {riak_mapreduce_sup, start_link, []},
                       permanent, infinity, supervisor, [riak_mapreduce_sup]},
    PhaseFSMSup = {riak_phase_sup,
                       {riak_phase_sup, start_link, []},
                       permanent, infinity, supervisor, [riak_phase_sup]},
    % Figure out which processes we should run...
    IsWebConfigured = (riak:get_app_env(riak_web_ip) /= undefined) andalso (riak:get_app_env(riak_web_ip) /= "undefined"),
    HasStorageBackend = (riak:get_app_env(storage_backend) /= undefined) andalso (riak:get_app_env(storage_backend) /= "undefined"),
    IsStatEnabled = (riak:get_app_env(riak_stat) == true),

    % Build the process list...
    Processes = lists:flatten([
        Eventer,
        VSup,
        ?IF(HasStorageBackend, VMaster, []),
        HandoffListen,
        RingMgr,
        Connect,
        LocalLogger,
        ?IF(IsWebConfigured, RiakWeb, []),
        ?IF(IsStatEnabled, RiakStat, []),
        MapReduceFSMSup,
        PhaseFSMSup,
        RiakJsSup,
        RiakJsMgr
    ]),

    % Run the proesses...
    {ok, {{one_for_one, 10, 10}, Processes}}.
