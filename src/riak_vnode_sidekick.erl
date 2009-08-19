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

-module(riak_vnode_sidekick).
-behaviour(gen_fsm).

-export([start/2]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([home/2,not_home/2]).

-define(TIMEOUT, 60000).     % poll time for normal operation
-define(LONGTIMEOUT, 120000). % if a vnode-merkle exchange in progress

-record(state, {vnode,idx,counter}).

% The vnode sidekick notices when a vnode is not "home"
% (when it is serving as an imperfect recipient of puts)
% and if it is not home and the home node is reachable,
% the sidekick initiates handoff.

start(VNode,Idx) ->
    gen_fsm:start(?MODULE, [VNode,Idx], []).
init([VNode,Idx]) ->
    StateData = #state{vnode=VNode,idx=Idx,counter=0},
    {ok,home,StateData,?TIMEOUT}.

home(timeout, StateData=#state{vnode=VNode,idx=Idx,counter=Count}) ->
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    Me = node(),
    case riak_ring:index_owner(MyRing, Idx) of
        Me ->
            gen_server2:cast(VNode, activate),
            case Count > 9 of
                true ->
                    gen_server2:cast(VNode, cache_purge),
                    {next_state,home,StateData#state{counter=0},?TIMEOUT};
                false ->
                    {next_state,home,StateData#state{counter=Count+1},?TIMEOUT}
            end;
        _ ->
            {next_state,not_home,StateData,1}
    end.

not_home(timeout, StateData=#state{vnode=VNode,idx=Idx}) ->
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    TargetNode = riak_ring:index_owner(MyRing, Idx),
    Me = node(),
    case TargetNode of % just in case we took ownership
        Me -> {next_state,home,StateData,?TIMEOUT}; 
        _ -> 
            case net_adm:ping(TargetNode) of
                pang -> {next_state,not_home,StateData,?TIMEOUT};
                pong ->
                    ObjList = gen_server2:call(VNode, list, 60000),
                    case ObjList of
                        [] ->
                            gen_server2:cast(VNode, {rexit, "sidekick"}),
                            {next_state,not_home,StateData,?LONGTIMEOUT};
                        _ -> 
                            gen_server2:cast(VNode, deactivate),
                            Merk = make_merk(VNode, Idx, ObjList),
                            gen_server:cast({riak_vnode_master, TargetNode},
                                            {vnode_merkle, {VNode,Idx,Merk}}),
                            {next_state,not_home,StateData,?LONGTIMEOUT}
                    end
            end
    end.

make_merk(VNode, Idx, ObjList) ->
    riak_eventer:notify(riak_vnode_sidekick, merkle_prep, Idx),
    merkerl:build_tree([{K,crypto:sha(V)} || {K,{ok,V}} <- 
                        [{K,gen_server2:call(VNode, {get_binary, K})} ||
                           K <- ObjList]]).

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(vnode_shutdown, _StateName, StateData) ->
    {stop,normal,StateData}.

%% @private
terminate(_Reason, _StateName, _State) -> ok.

