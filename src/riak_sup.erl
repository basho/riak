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

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Eventer = {riak_eventer,
               {riak_eventer, start_link, []},
               permanent, 5000, worker, dynamic},
    Doorbell = {riak_doorbell,
                {riak_doorbell, start_link, []},
                permanent, 5000, worker, dynamic},
    RingMgr = {riak_ring_manager,
             {riak_ring_manager, start_link, []},
             permanent, 5000, worker, dynamic},
    RingGossip = {riak_ring_gossiper,
             {riak_ring_gossiper, start_link, []},
             permanent, 5000, worker, dynamic},
    Connect = {riak_connect,
             {riak_connect, start_link, []},
             permanent, 5000, worker, dynamic},
    API = {riak_api,
           {riak_api, start_link, []},
           permanent, 5000, worker, dynamic},
    VMaster = {riak_vnode_master,
               {riak_vnode_master, start_link, []},
               permanent, 5000, worker, dynamic},
    EventGuard = {riak_event_guard,
                 {riak_event_guard, start_link, []},
                 permanent, 5000, worker, dynamic},
    BucketKeys = {riak_bucketkeys,
                 {riak_bucketkeys, start_link, []},
                  permanent, 5000, worker, dynamic},
    LocalLogger = {riak_local_logger,
                   {riak_local_logger, start_link, []},
                   permanent, 5000, worker, dynamic},
    RiakWeb = {webmachine_mochiweb,
                 {webmachine_mochiweb, start, [riak_web:config()]},
                  permanent, 5000, worker, dynamic},
    Processes0 = 
    case riak:get_app_env(riak_web_ip) of
        "undefined" ->
            [RingMgr,RingGossip,Connect,API,EventGuard,LocalLogger,BucketKeys];
        undefined ->
            [RingMgr,RingGossip,Connect,API,EventGuard,LocalLogger,BucketKeys];
        _ ->
            [RingMgr,RingGossip,Connect,API,EventGuard,LocalLogger,BucketKeys,
             RiakWeb]
    end,
    Processes1 = 
    case riak:get_app_env(doorbell_port) of
        "undefined" -> Processes0;
        undefined -> Processes0;
        _ -> [Doorbell|Processes0]
    end,
    Processes2 = 
    case riak:get_app_env(storage_backend) of
        "undefined" -> Processes1;
        undefined -> Processes1;
        _ -> [VMaster|Processes1]
    end,
    Processes = [Eventer|Processes2],
    {ok, {{one_for_one, 10, 10}, Processes}}.
