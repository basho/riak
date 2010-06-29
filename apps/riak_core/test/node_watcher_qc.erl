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
-module(node_watcher_qc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-record(state, { up_nodes = [],
                 services = [],
                 service_pids = [],
                 peers = []}).

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

-define(ORDSET(L), ordsets:from_list(L)).

qc_test_() ->
    {timeout, 120, fun() -> ?assert(eqc:quickcheck(?QC_OUT(prop_main()))) end}.

prop_main() ->
    %% Initialize necessary env settings
    application:load(riak_core),
    application:set_env(riak_core, gossip_interval, 250),
    application:set_env(riak_core, ring_creation_size, 8),

    %% Start supporting processes
    riak_core_ring_events:start_link(),
    riak_core_node_watcher_events:start_link(),

    ?FORALL(Cmds, commands(?MODULE),
            begin
                %% Setup ETS table to recv broadcasts
                ets:new(?MODULE, [ordered_set, named_table, public]),
                ets:insert_new(?MODULE, {bcast_id, 0}),

                %% Start the watcher
                {ok, Pid} = riak_core_node_watcher:start_link(),

                %% Internal call to the node watcher to override default broadcast mechanism
                gen_server:call(riak_core_node_watcher, {set_bcast_mod, ?MODULE, on_broadcast}),

                %% Run the test
                {_H, _S, Res} = run_commands(?MODULE, Cmds),

                %% Unlink and kill our PID
                unlink(Pid),
                kill_and_wait(Pid),

                %% Delete the ETS table
                ets:delete(?MODULE),

                case Res of
                    ok -> ok;
                    _  -> io:format(user, "QC result: ~p\n", [Res])
                end,
                aggregate(command_names(Cmds), Res == ok)
            end).


%% ====================================================================
%% eqc_statem callbacks
%% ====================================================================

initial_state() ->
    #state{ up_nodes = [node()] }.

command(S) ->
    oneof([
           {call, ?MODULE, ring_update, [g_ring_nodes()]},
           {call, ?MODULE, local_service_up, [g_service()]},
           {call, ?MODULE, local_service_down, [g_service()]},
           {call, ?MODULE, local_service_kill, [g_service(), S]},
           {call, ?MODULE, local_node_up, []},
           {call, ?MODULE, local_node_down, []},
           {call, ?MODULE, remote_service_up, [g_node(), g_services()]},
           {call, ?MODULE, remote_service_down, [g_node()]},
           {call, ?MODULE, remote_service_down_disterl, [g_node()]},
           {call, ?MODULE, wait_for_bcast, []}
          ]).

precondition(S, {call, _, local_service_kill, [Service, S]}) ->
    orddict:is_key(Service, S#state.service_pids);
precondition(S, {call, _, wait_for_bcast, _}) ->
    is_node_up(node(), S);
precondition(_, _) ->
    true.


next_state(S, Res, {call, _, local_service_up, [Service]}) ->
    S2 = service_up(node(), Service, S),
    Pids = orddict:store(Service, Res, S2#state.service_pids),
    S2#state { service_pids = Pids };

next_state(S, _Res, {call, _, local_service_down, [Service]}) ->
    S2 = service_down(node(), Service, S),
    Pids = orddict:erase(Service, S2#state.service_pids),
    S2#state { service_pids = Pids };

next_state(S, _Res, {call, _, local_service_kill, [Service, _]}) ->
    S2 = service_down(node(), Service, S),
    Pids = orddict:erase(Service, S2#state.service_pids),
    S2#state { service_pids = Pids };

next_state(S, _Res, {call, _, local_node_up, []}) ->
    node_up(node(), S);

next_state(S, _Res, {call, _, local_node_down, []}) ->
    node_down(node(), S);

next_state(S, _Res, {call, _, remote_service_up, [Node, Services]}) ->
    peer_up(Node, Services, S);

next_state(S, _Res, {call, _, Fn, [Node]})
  when Fn == remote_service_down; Fn == remote_service_down_disterl ->
    peer_down(Node, S);

next_state(S, _Res, {call, _, wait_for_bcast, _}) ->
    S;

next_state(S, _Res, {call, _, ring_update, [Nodes]}) ->
    Peers = ordsets:del_element(node(), ordsets:from_list(Nodes)),
    peer_filter(S#state { peers = Peers }).



postcondition(S, {call, _, local_service_up, [Service]}, _Res) ->
    S2 = service_up(node(), Service, S),
    validate_broadcast(S, S2, service),
    deep_validate(S2);

postcondition(S, {call, _, local_service_down, [Service]}, _Res) ->
    S2 = service_down(node(), Service, S),
    validate_broadcast(S, S2, service),
    deep_validate(S2);

postcondition(S, {call, _, local_service_kill, [Service, _]}, _Res) ->
    S2 = service_down(node(), Service, S),
    validate_broadcast(S, S2, service),
    deep_validate(S2);

postcondition(S, {call, _, local_node_up, _}, _Res) ->
    S2 = node_up(node(), S),
    validate_broadcast(S, S2, node),
    deep_validate(S2);

postcondition(S, {call, _, local_node_down, _}, _Res) ->
    S2 = node_down(node(), S),
    validate_broadcast(S, S2, node),
    deep_validate(S2);

postcondition(S, {call, _, remote_service_up, [Node, Services]}, _Res) ->
    %% If the remote service WAS down, expect a broadcast to it, otherwise no
    %% bcast should be present
    Bcasts = broadcasts(),
    case is_peer(Node, S) andalso not is_node_up(Node, S) of
        true ->
            case is_node_up(node(), S) of
                true ->
                    ExpServices = services(node(), S),
                    ?assertEqual({{up, node(), ExpServices}, [Node]}, hd(Bcasts));
                false ->
                    ?assertEqual({{down, node()}, [Node]}, hd(Bcasts))
            end;
        false ->
            ?assertEqual([], Bcasts)
    end,
    S2 = peer_up(Node, Services, S),
    deep_validate(S2);

postcondition(S, {call, _, Fn, [Node]}, _Res)
  when Fn == remote_service_down; Fn == remote_service_down_disterl ->
    ?assertEqual([], broadcasts()),
    S2 = case is_peer(Node, S) of
             true ->
                 node_down(Node, S);
             false ->
                 S
         end,
    deep_validate(S2);

postcondition(S, {call, _, wait_for_bcast, _}, _Res) ->
    validate_broadcast(S, S, service);

postcondition(S, {call, _, ring_update, [Nodes]}, _Res) ->
    %% Ring update should generate a broadcast to all NEW peers
    Bcasts = broadcasts(),
    Peers = ordsets:del_element(node(), ordsets:from_list(Nodes)),
    NewPeers = ordsets:subtract(Peers, S#state.peers),
    case is_node_up(node(), S) of
        true ->
            ExpServices = services(node(), S),
            ?assertEqual({{up, node(), ExpServices}, NewPeers}, hd(Bcasts));
        false ->
            ?assertEqual({{down, node()}, NewPeers}, hd(Bcasts))
    end,
    S2 = peer_filter(S#state { peers = Peers }),
    deep_validate(S2).




deep_validate(S) ->
    %% Verify that the list of services in the state match what the node watcher reports
    ExpAllServices = services(S),
    ActAllServices = riak_core_node_watcher:services(),
    ?assertEqual(ExpAllServices, ActAllServices),

    %% Now that we verified the list of services match, build a list of node lists, per
    %% service.
    ExpNodes = ?ORDSET([snodes(Svc, S) || Svc <- ExpAllServices]),
    ActNodes = ?ORDSET([?ORDSET(riak_core_node_watcher:nodes(Svc)) || Svc <- ExpAllServices]),
    ?assertEqual(ExpNodes, ActNodes),
    true.

validate_broadcast(S0, Sfinal, Op) ->
    Bcasts = broadcasts(),
    Transition = {is_node_up(node(), S0), is_node_up(node(), Sfinal), Op},
    ExpPeers = Sfinal#state.peers,
    case Transition of
        {false, true, _} -> %% down -> up
            ExpServices = services(node(), Sfinal),
            ?assertEqual({{up, node(), ExpServices}, ExpPeers}, hd(Bcasts));

        {true, false, _} -> %% up -> down
            ?assertEqual({{down, node()}, ExpPeers}, hd(Bcasts));

        {true, true, service} -> %% up -> up (service change)
            ExpServices = services(node(), Sfinal),
            ?assertEqual({{up, node(), ExpServices}, ExpPeers}, hd(Bcasts));

        _ ->
            ?assertEqual([], Bcasts)
    end,
    true.


%% ====================================================================
%% Generators
%% ====================================================================

g_service() ->
    oneof([s1, s2, s3, s4]).

g_node() ->
    oneof(['n1@127.0.0.1', 'n2@127.0.0.1', 'n3@127.0.0.1']).

g_services() ->
    list(elements([s1, s2, s3, s4])).

g_ring_nodes() ->
    vector(app_helper:get_env(riak_core, ring_creation_size),
           oneof([node(), 'n1@127.0.0.1', 'n2@127.0.0.1', 'n3@127.0.0.1'])).


%% ====================================================================
%% Calls
%% ====================================================================

local_service_up(Service) ->
    Pid = spawn(fun() -> service_loop() end),
    ok = riak_core_node_watcher:service_up(Service, Pid),
    Pid.

local_service_down(Service) ->
    ok = riak_core_node_watcher:service_down(Service).

local_service_kill(Service, State) ->
    Avsn0 = riak_core_node_watcher:avsn(),
    Pid = orddict:fetch(Service, State#state.service_pids),
    kill_and_wait(Pid),
    wait_for_avsn(Avsn0).

local_node_up() ->
    riak_core_node_watcher:node_up().

local_node_down() ->
    riak_core_node_watcher:node_down().

remote_service_up(Node, Services) ->
    Avsn0 = riak_core_node_watcher:avsn(),
    gen_server:cast(riak_core_node_watcher, {up, Node, Services}),
    wait_for_avsn(Avsn0).

remote_service_down(Node) ->
    Avsn0 = riak_core_node_watcher:avsn(),
    gen_server:cast(riak_core_node_watcher, {down, Node}),
    wait_for_avsn(Avsn0).

remote_service_down_disterl(Node) ->
    Avsn0 = riak_core_node_watcher:avsn(),
    riak_core_node_watcher ! {nodedown, Node},
    wait_for_avsn(Avsn0).

wait_for_bcast() ->
    {ok, Interval} = application:get_env(riak_core, gossip_interval),
    timer:sleep(Interval + 50).

ring_update(Nodes) ->
    Ring = build_ring(Nodes),
    Avsn0 = riak_core_node_watcher:avsn(),
    gen_server:cast(riak_core_node_watcher, {ring_update, Ring}),
    wait_for_avsn(Avsn0),
    ?ORDSET(Nodes).


%% ====================================================================
%% State functions
%% ====================================================================

node_up(Node, S) ->
    S#state { up_nodes = ordsets:add_element(Node, S#state.up_nodes) }.

node_down(Node, S) ->
    S#state { up_nodes = ordsets:del_element(Node, S#state.up_nodes) }.

service_up(Node, Service, S) ->
    S#state { services = ordsets:add_element({Node, Service}, S#state.services) }.

services_up(Node, Services, S) ->
    NewServices = ?ORDSET([{Node, Svc} || Svc <- Services]),
    OldServices = [{N, Svc} || {N, Svc} <- S#state.services,
                               Node /= N],
    S#state { services = ordsets:union(NewServices, OldServices) }.

service_down(Node, Svc, S) ->
    S#state { services = ordsets:del_element({Node, Svc}, S#state.services) }.


is_node_up(Node, S) ->
    ordsets:is_element(Node, S#state.up_nodes).

is_peer(Node, S) ->
    ordsets:is_element(Node, S#state.peers).

peer_up(Node, Services, S) ->
    case is_peer(Node, S) of
        true ->
            node_up(Node, services_up(Node, Services, S));
        false ->
            S
    end.

peer_down(Node, S) ->
    case is_peer(Node, S) of
        true ->
            Services = [{N, Svc} || {N, Svc} <- S#state.services,
                                    N /= Node],
            node_down(Node, S#state { services = Services });
        false ->
            S
    end.

peer_filter(S) ->
    ThisNode = node(),
    Services = [{N, Svc} || {N, Svc} <- S#state.services,
                            is_peer(N, S) orelse N == ThisNode],
    UpNodes = [N || N <- S#state.up_nodes,
                    is_peer(N, S) orelse N == ThisNode],
    S#state { services = Services, up_nodes = UpNodes }.

services(S) ->
    ?ORDSET([Svc || {N, Svc} <- S#state.services,
                    ordsets:is_element(N, S#state.up_nodes)]).

services(Node, S) ->
    case ordsets:is_element(Node, S#state.up_nodes) of
        true ->
            all_services(Node, S);
        false ->
            []
    end.

snodes(S) ->
    S#state.up_nodes.

snodes(Service, S) ->
    ?ORDSET([Node || {Node, Svc} <- S#state.services,
                     ordsets:is_element(Node, S#state.up_nodes),
                     Svc == Service]).

all_services(Node, S) ->
    ?ORDSET([Svc || {N, Svc} <- S#state.services,
                    N == Node]).



%% ====================================================================
%% Internal functions
%% ====================================================================

on_broadcast(Nodes, _Name, Msg) ->
    Id = ets:update_counter(?MODULE, bcast_id, {2, 1}),
    ets:insert_new(?MODULE, {Id, Msg, Nodes}).

broadcasts() ->
    Bcasts = [list_to_tuple(L) || L <- ets:match(?MODULE, {'_', '$1', '$2'})],
    ets:match_delete(?MODULE, {'_', '_', '_'}),
    Bcasts.


kill_and_wait(Pid) ->
    Mref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mref, _, _, _} ->
            ok
    end.

wait_for_avsn(Avsn0) ->
    case riak_core_node_watcher:avsn() of
        Avsn0 ->
            erlang:yield(),
            wait_for_avsn(Avsn0);
        _ ->
            ok
    end.


service_loop() ->
    receive
        _Any ->
            service_loop()
    end.

build_ring([Node | Rest]) ->
    Inc = trunc(math:pow(2,160)-1) div app_helper:get_env(riak_core, ring_creation_size),
    build_ring(Rest, 0, Inc, riak_core_ring:fresh(Node)).

build_ring([], _Id, _Inc, R) ->
    R;
build_ring([Node | Rest], Id, Inc, R) ->
    R2 = riak_core_ring:transfer_node(Id+Inc, Node, R),
    build_ring(Rest, Id+Inc, Inc, R2).

-endif.
