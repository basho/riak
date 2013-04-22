%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012-2013 Basho Technologies, Inc.
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
-module(verify_capabilities).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

%% 1.4 {riak_kv, handoff_data_encoding} -> [encode_raw, encode_zlib]
%% 1.3 {riak_kv, anti_entropy} -> [disabled, enabled_v1]
confirm() ->
    lager:info("Deploying mixed set of nodes"),
    Legacy = case lists:member(legacy, rt:versions()) of
        true -> legacy;
        _ -> current
    end,

    Nodes = rt:deploy_nodes([current, previous, Legacy]),
    [CNode, PNode, LNode] = Nodes,

    lager:info("Verifying known capabilities on a Current 1-node cluster"),
    lager:info("Verify staged_joins == true"),
    ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_core, staged_joins}, true)),

    %% This test is written with the intent that 1.4 is 'current'
    CCapabilities = rt:capability(CNode, all),
    assert_capability(CCapabilities, {riak_kv, legacy_keylisting}, false),
    assert_capability(CCapabilities, {riak_kv, listkeys_backpressure}, true),
    assert_capability(CCapabilities, {riak_core, staged_joins}, true),
    assert_capability(CCapabilities, {riak_kv, index_backpressure}, true),
    assert_capability(CCapabilities, {riak_pipe, trace_format}, ordsets),
    assert_capability(CCapabilities, {riak_kv, mapred_2i_pipe}, true),
    assert_capability(CCapabilities, {riak_kv, mapred_system}, pipe),
    assert_capability(CCapabilities, {riak_kv, vnode_vclocks}, true),
    assert_capability(CCapabilities, {riak_core, vnode_routing}, proxy),
    assert_supported(CCapabilities, {riak_core, staged_joins}, [true,false]),
    assert_supported(CCapabilities, {riak_core, vnode_routing}, [proxy,legacy]),
    assert_supported(CCapabilities, {riak_kv, index_backpressure}, [true,false]),
    assert_supported(CCapabilities, {riak_kv, legacy_keylisting}, [false]),
    assert_supported(CCapabilities, {riak_kv, listkeys_backpressure}, [true,false]),
    assert_supported(CCapabilities, {riak_kv, mapred_2i_pipe}, [true,false]),
    assert_supported(CCapabilities, {riak_kv, mapred_system}, [pipe]),
    assert_supported(CCapabilities, {riak_kv, vnode_vclocks}, [true,false]),
    assert_supported(CCapabilities, {riak_pipe, trace_format}, [ordsets,sets]),
    assert_supported(CCapabilities, {riak_kv, handoff_data_encoding}, [encode_raw, encode_zlib]),

    %% We've got a current-version node only, we should see raw selected as default:
    assert_using(CNode, {riak_kv, handoff_data_encoding}, encode_raw),

    lager:info("Crash riak_core_capability server"),
    restart_capability_server(CNode),

    lager:info("Verify staged_joins == true after crash"),
    ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_core, staged_joins}, true)),

    lager:info("Building current + ~s cluster", [Legacy]),
    rt:join(LNode, CNode),
    ?assertEqual(ok, rt:wait_until_all_members([CNode], [CNode, LNode])),
    ?assertEqual(ok, rt:wait_until_legacy_ringready(CNode)),

    case Legacy of
        legacy ->
            LCapabilities = rt:capability(CNode, all),
            assert_capability(LCapabilities, {riak_kv, legacy_keylisting}, false),
            assert_capability(LCapabilities, {riak_kv, listkeys_backpressure}, true),
            assert_capability(LCapabilities, {riak_core, staged_joins}, false),
            assert_capability(LCapabilities, {riak_kv, index_backpressure}, false),
            assert_capability(LCapabilities, {riak_pipe, trace_format}, sets),
            assert_capability(LCapabilities, {riak_kv, mapred_2i_pipe}, true),
            assert_capability(LCapabilities, {riak_kv, mapred_system}, pipe),
            assert_capability(LCapabilities, {riak_kv, vnode_vclocks}, true),
            assert_capability(LCapabilities, {riak_core, vnode_routing}, proxy),
            assert_supported(LCapabilities, {riak_core, staged_joins}, [true,false]),
            assert_supported(LCapabilities, {riak_core, vnode_routing}, [proxy,legacy]),
            assert_supported(LCapabilities, {riak_kv, index_backpressure}, [true,false]),
            assert_supported(LCapabilities, {riak_kv, legacy_keylisting}, [false]),
            assert_supported(LCapabilities, {riak_kv, listkeys_backpressure}, [true,false]),
            assert_supported(LCapabilities, {riak_kv, mapred_2i_pipe}, [true,false]),
            assert_supported(LCapabilities, {riak_kv, mapred_system}, [pipe]),
            assert_supported(LCapabilities, {riak_kv, vnode_vclocks}, [true,false]),
            assert_supported(LCapabilities, {riak_pipe, trace_format}, [ordsets,sets]),

            %% We've added a legacy server: we should see zlib selected by the current-version node:
            assert_using(CNode, {riak_kv, handoff_data_encoding}, encode_zlib),

            lager:info("Crash riak_core_capability server"),
            restart_capability_server(CNode),

            lager:info("Verify staged_joins == false after crash"),
            ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_core, staged_joins}, false)),

            lager:info("Adding previous node to cluster"),
            rt:join(PNode, LNode),
            ?assertEqual(ok, rt:wait_until_all_members([CNode], [CNode, LNode, PNode])),
            ?assertEqual(ok, rt:wait_until_legacy_ringready(CNode)),

            lager:info("Verify staged_joins == false after crash"),
            ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_core, staged_joins}, false)),
            
            PCapabilities = rt:capability(CNode, all),
            assert_capability(PCapabilities, {riak_kv, legacy_keylisting}, false),
            assert_capability(PCapabilities, {riak_kv, listkeys_backpressure}, true),
            assert_capability(PCapabilities, {riak_core, staged_joins}, false),
            assert_capability(PCapabilities, {riak_kv, index_backpressure}, false),
            assert_capability(PCapabilities, {riak_pipe, trace_format}, sets),
            assert_capability(PCapabilities, {riak_kv, mapred_2i_pipe}, true),
            assert_capability(PCapabilities, {riak_kv, mapred_system}, pipe),
            assert_capability(PCapabilities, {riak_kv, vnode_vclocks}, true),
            assert_capability(PCapabilities, {riak_core, vnode_routing}, proxy),
            assert_supported(PCapabilities, {riak_core, staged_joins}, [true,false]),
            assert_supported(PCapabilities, {riak_core, vnode_routing}, [proxy,legacy]),
            assert_supported(PCapabilities, {riak_kv, index_backpressure}, [true,false]),
            assert_supported(PCapabilities, {riak_kv, legacy_keylisting}, [false]),
            assert_supported(PCapabilities, {riak_kv, listkeys_backpressure}, [true,false]),
            assert_supported(PCapabilities, {riak_kv, mapred_2i_pipe}, [true,false]),
            assert_supported(PCapabilities, {riak_kv, mapred_system}, [pipe]),
            assert_supported(PCapabilities, {riak_kv, vnode_vclocks}, [true,false]),
            assert_supported(PCapabilities, {riak_pipe, trace_format}, [ordsets,sets]),

            %% We've added a previous version (1.2) we should (still) see zlib selected:
            assert_using(CNode, {riak_kv, handoff_data_encoding}, encode_zlib),

            lager:info("Upgrade Legacy node"),
            rt:upgrade(LNode, current),
            ?assertEqual(ok, rt:wait_until_all_members([CNode], [CNode, LNode, PNode])),
            ?assertEqual(ok, rt:wait_until_legacy_ringready(CNode)),
            lager:info("Verify staged_joins == true after upgrade of legacy -> current"),

            %% We have upgraded the legacy node, but we should see zlib selected (previous node still not upgraded):
            assert_using(CNode, {riak_kv, handoff_data_encoding}, encode_zlib);

        _ ->
            lager:info("Legacy Riak not available, skipping legacy tests"),
            lager:info("Adding previous node to cluster"),
            rt:join(PNode, LNode),
            ?assertEqual(ok, rt:wait_until_all_members([CNode], [CNode, LNode, PNode])),
            ?assertEqual(ok, rt:wait_until_legacy_ringready(CNode)),
            lager:info("Verify staged_joins == true after adding previous")
    end,

    ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_core, staged_joins}, true)),

    PCap2 = rt:capability(CNode, all),
    assert_capability(PCap2, {riak_kv, legacy_keylisting}, false),
    assert_capability(PCap2, {riak_kv, listkeys_backpressure}, true),
    assert_capability(PCap2, {riak_core, staged_joins}, true),
    assert_capability(PCap2, {riak_kv, index_backpressure}, false),
    assert_capability(PCap2, {riak_pipe, trace_format}, sets),
    assert_capability(PCap2, {riak_kv, mapred_2i_pipe}, true),
    assert_capability(PCap2, {riak_kv, mapred_system}, pipe),
    assert_capability(PCap2, {riak_kv, vnode_vclocks}, true),
    assert_capability(PCap2, {riak_core, vnode_routing}, proxy),
    assert_supported(PCap2, {riak_core, staged_joins}, [true,false]),
    assert_supported(PCap2, {riak_core, vnode_routing}, [proxy,legacy]),
    assert_supported(PCap2, {riak_kv, index_backpressure}, [true,false]),
    assert_supported(PCap2, {riak_kv, legacy_keylisting}, [false]),
    assert_supported(PCap2, {riak_kv, listkeys_backpressure}, [true,false]),
    assert_supported(PCap2, {riak_kv, mapred_2i_pipe}, [true,false]),
    assert_supported(PCap2, {riak_kv, mapred_system}, [pipe]),
    assert_supported(PCap2, {riak_kv, vnode_vclocks}, [true,false]),
    assert_supported(PCap2, {riak_pipe, trace_format}, [ordsets,sets]),
    
    lager:info("Upgrading Previous node"),
    rt:upgrade(PNode, current),

    lager:info("Verifying index_backpressue changes to true"),
    ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_kv, index_backpressure}, true)),

    lager:info("Verifying riak_pipe,trace_format changes to ordsets"),
    ?assertEqual(ok, rt:wait_until_capability(CNode, {riak_pipe, trace_format}, ordsets)),

    CCap2 = rt:capability(CNode, all),
    assert_capability(CCap2, {riak_kv, legacy_keylisting}, false),
    assert_capability(CCap2, {riak_kv, listkeys_backpressure}, true),
    assert_capability(CCap2, {riak_core, staged_joins}, true),
    assert_capability(CCap2, {riak_kv, index_backpressure}, true),
    assert_capability(CCap2, {riak_pipe, trace_format}, ordsets),
    assert_capability(CCap2, {riak_kv, mapred_2i_pipe}, true),
    assert_capability(CCap2, {riak_kv, mapred_system}, pipe),
    assert_capability(CCap2, {riak_kv, vnode_vclocks}, true),
    assert_capability(CCap2, {riak_core, vnode_routing}, proxy),
    assert_supported(CCap2, {riak_core, staged_joins}, [true,false]),
    assert_supported(CCap2, {riak_core, vnode_routing}, [proxy,legacy]),
    assert_supported(CCap2, {riak_kv, index_backpressure}, [true,false]),
    assert_supported(CCap2, {riak_kv, legacy_keylisting}, [false]),
    assert_supported(CCap2, {riak_kv, listkeys_backpressure}, [true,false]),
    assert_supported(CCap2, {riak_kv, mapred_2i_pipe}, [true,false]),
    assert_supported(CCap2, {riak_kv, mapred_system}, [pipe]),
    assert_supported(CCap2, {riak_kv, vnode_vclocks}, [true,false]),
    assert_supported(CCap2, {riak_pipe, trace_format}, [ordsets,sets]),

    %% We've upgraded both legacy and previous versions; we should see raw selected by everyone:
    [assert_using(Node, {riak_kv, handoff_data_encoding}, encode_raw) || Node <- [CNode, PNode, LNode]],

    %% All nodes are now current version. Test override behavior.
    Override = fun(undefined, Prefer) ->
                       [{riak_core, [{override_capability,
                                      [{vnode_routing,
                                        [{prefer, Prefer}]
                                       }]}]
                        }];
                  (Use, Prefer) ->
                       [{riak_core, [{override_capability,
                                      [{vnode_routing,
                                        [{use, Use},
                                         {prefer, Prefer}]
                                       }]}]
                        }]
               end,

    lager:info("Override: (use: legacy), (prefer: proxy)"),
    [rt:update_app_config(Node, Override(legacy, proxy)) || Node <- Nodes],

    lager:info("Verify vnode_routing == legacy"),
    ?assertEqual(legacy, rt:capability(CNode, {riak_core, vnode_routing})),

    lager:info("Override: (use: proxy), (prefer: legacy)"),
    [rt:update_app_config(Node, Override(proxy, legacy)) || Node <- Nodes],

    lager:info("Verify vnode_routing == proxy"),
    ?assertEqual(proxy, rt:capability(CNode, {riak_core, vnode_routing})),

    lager:info("Override: (prefer: legacy)"),
    [rt:update_app_config(Node, Override(undefined, legacy)) || Node <- Nodes],

    lager:info("Verify vnode_routing == legacy"),
    ?assertEqual(legacy, rt:capability(CNode, {riak_core, vnode_routing})),

    [rt:stop(Node) || Node <- Nodes],
    pass.

assert_capability(Capabilities, Capability, Value) ->
    lager:info("Checking Capability Setting ~p =:= ~p", [Capability, Value]),
    ?assertEqual(Value, proplists:get_value(Capability, Capabilities)).

assert_supported(Capabilities, Capability, Value) ->
    lager:info("Checking Capability Supported Values ~p =:= ~p", [Capability, Value]),
    ?assertEqual(Value, proplists:get_value(Capability, proplists:get_value('$supported', Capabilities))).

assert_using(Node, {CapabilityCategory, CapabilityName}, ExpectedCapabilityName) ->
    lager:info("assert_using ~p =:= ~p", [ExpectedCapabilityName, CapabilityName]),
    try ExpectedCapabilityName =:= rt:capability(Node, {CapabilityCategory, CapabilityName}) of
        X -> X
    catch
        %% This is for catching a case in which a legacy node doesn't support capabilities at all:
        exit:Exception -> lager:info("assert_using() caught exception: ~p", [Exception]), 
                          false
    end.

restart_capability_server(Node) ->
    Pid = rpc:call(Node, erlang, whereis, [riak_core_capability]),
    rpc:call(Node, erlang, exit, [Pid, kill]),
    HasNewPid =
        fun(N) ->
            case rpc:call(N, erlang, whereis, [riak_core_capability]) of
                Pid -> false;
                NewPid when is_pid(NewPid) -> true;
                _ -> false
            end
        end,
    rt:wait_until(Node, HasNewPid).

