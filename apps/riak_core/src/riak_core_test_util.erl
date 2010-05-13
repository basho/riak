%% -------------------------------------------------------------------
%%
%% riak_test_util: utilities for test scripts
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

%% @doc utilities for test scripts

-module(riak_core_test_util).
-export([setup_mockring1/0]).
-include_lib("eunit/include/eunit.hrl").

setup_mockring1() ->
    % requires a running riak_core_ring_manager, in test-mode is ok
    Ring0 = lists:foldl(fun(_,R) ->
                               riak_core_ring:transfer_node(
                                 hd(riak_core_ring:my_indices(R)),
                                 othernode@otherhost, R) end,
                       riak_core_ring:fresh(16,node()),[1,2,3,4,5,6]),
    Ring = lists:foldl(fun(_,R) ->
                               riak_core_ring:transfer_node(
                                 hd(riak_core_ring:my_indices(R)),
                                 othernode2@otherhost2, R) end,
                       Ring0,[1,2,3,4,5,6]),
    riak_core_ring_manager:set_my_ring(Ring).
