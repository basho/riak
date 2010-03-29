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

%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.

-module(riak_kv_ring_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([]) ->
    %% Pull the initial ring and make sure all vnodes are started
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    ensure_vnodes_started(Ring),
    {ok, #state{}}.

handle_event({ring_update, Ring}, State) ->
    %% Make sure all vnodes are started...
    ensure_vnodes_started(Ring),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.


handle_call(_Request, State) ->
    {ok, ok, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_vnodes_started(Ring) ->
    AllMembers = riak_core_ring:all_members(Ring),
    VNodes2Start =
        case {length(AllMembers), hd(AllMembers) =:= node()} of
            {1, true} ->
                riak_core_ring:my_indices(Ring);
            _ ->
                {ok, Excl} = gen_server:call(riak_kv_vnode_master, get_exclusions, 15000),
                case riak_core_ring:random_other_index(Ring, Excl) of
                    no_indices ->
                        case length(Excl) =:= riak_core_ring:num_partitions(Ring) of
                            true ->
                                exit;
                            false ->
                                riak_core_ring:my_indices(Ring)
                        end;
                    RO ->
                        [RO | riak_core_ring:my_indices(Ring)]
                end
        end,
    case VNodes2Start of
        exit ->
            riak:stop("node removal completed, exiting.");
        _ ->
            [begin
                 gen_server:cast({riak_kv_vnode_master, node()}, {start_vnode, I}) 
             end|| I <- VNodes2Start]
    end.
