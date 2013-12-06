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
-module(gh_riak_core_176).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    Nodes = rt:deploy_nodes(3),
    [Node1, Node2, Node3] = Nodes,
    Nodes12 = [Node1, Node2],
    Nodes123 = Nodes,

    %% Stolen from riak_core_handoff_sender.erl
    [_Name,Host] = string:tokens(atom_to_list(Node2), "@"),

    %% Get IP associated with node name
    {ok, NodeIP} = inet:getaddr(Host, inet),

    %% Find alternative IP
    {ok, IfAddrs} = inet:getifaddrs(),
    Addrs =
        lists:flatmap(
          fun({_If, Props}) ->
                  [Addr || {addr, Addr} <- Props,
                           size(Addr) == 4]
          end, IfAddrs),
    AlternateIP = ip_tuple_to_string(hd(Addrs -- [NodeIP])),

    lager:info("Change ~p handoff_ip from ~p to ~p",
               [Node2, NodeIP, AlternateIP]),
    NewConfig = [{riak_core, [{handoff_ip, AlternateIP}]}],
    rt:update_app_config(Node2, NewConfig),
    rt:wait_for_service(Node2, riak_kv),

    lager:info("Write data to the cluster"),
    rt:systest_write(Node1, 100),

    lager:info("Join ~p to the cluster and wait for handoff to finish",
               [Node2]),
    rt:join(Node2, Node1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes12)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes12)),
    rt:wait_until_nodes_agree_about_ownership(Nodes12),
    
    %% Check 0.0.0.0 address works
    lager:info("Change ~p handoff_ip to \"0.0.0.0\"", [Node3]),
    rt:update_app_config(Node3,
                         [{riak_core, [{handoff_ip, "0.0.0.0"}]}]),

    lager:info("Join ~p to the cluster and wait for handoff to finish",
               [Node3]),
    rt:wait_for_service(Node3, riak_kv),
    rt:join(Node3, Node1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes123)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes123)),
    rt:wait_until_nodes_agree_about_ownership(Nodes123),

    lager:info("Test gh_riak_core_176 passed"),
    pass.
    
ip_tuple_to_string(T) ->
    L = tuple_to_list(T),
    string:join([integer_to_list(X) || X <- L], ".").

