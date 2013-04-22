%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
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
-module(verify_reset_bucket_props).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-define(BUCKET, <<"test_bucket">>).

confirm() ->
    %% Bring up a 3-node cluster for the test
    %% we will be using two of the nodes to perform an
    %% update and then a reset (one on each node) of a bucket's properties.
    %% All nodes are checked to make sure the reset is affected on them
    [Node1, Node2, _Node3] = Nodes = rt:build_cluster(3),

    DefaultProps = get_current_bucket_props(Nodes, ?BUCKET),

    Updates = [{n_val, 1}],
    lager:info("Setting bucket properties ~p for bucket ~p on node ~p", 
               [?BUCKET, Updates, Node1]),
    rpc:call(Node1, riak_core_bucket, set_bucket, [?BUCKET, Updates]),    
    rt:wait_until_ring_converged(Nodes),

    UpdatedProps = get_current_bucket_props(Nodes, ?BUCKET),
    ?assertNotEqual(DefaultProps, UpdatedProps),
    
    lager:info("Resetting bucket properties for bucket ~p on node ~p", 
               [?BUCKET, Node2]),
    rpc:call(Node2, riak_core_bucket, reset_bucket, [?BUCKET]),
    rt:wait_until_ring_converged(Nodes),
    
    [check_props_reset(Node, ?BUCKET, DefaultProps) || Node <- Nodes].

%% fetch bucket properties via rpc 
%% from a node or a list of nodes (one node is chosen at random)
get_current_bucket_props(Nodes, Bucket) when is_list(Nodes) ->    
    Node = lists:nth(length(Nodes), Nodes),
    get_current_bucket_props(Node, Bucket);
get_current_bucket_props(Node, Bucket) when is_atom(Node) ->
    rpc:call(Node, 
             riak_core_bucket,
             get_bucket,
             [Bucket]).

check_props_reset(Node, Bucket, DefaultProps) ->
    Current = get_current_bucket_props(Node, Bucket),
    ?assertEqual(lists:usort(DefaultProps), lists:usort(Current)).



