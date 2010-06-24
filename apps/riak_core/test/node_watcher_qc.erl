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
                 service_pids = [] }).

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

-define(ORDSET(L), ordsets:from_list(L)).

qc_test_() ->
    {timeout, 60, fun() -> ?assert(eqc:quickcheck(?QC_OUT(prop_main()))) end}.

prop_main() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                %% Start the watcher and supporting processes
                riak_core_ring_events:start_link(),
                riak_core_node_watcher_events:start_link(),
                {ok, Pid} = riak_core_node_watcher:start_link(),

                %% Run the test
                {_H, _S, Res} = run_commands(?MODULE, Cmds),

                %% Unlink and kill our PID
                unlink(Pid),
                kill_and_wait(Pid),

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
           {call, ?MODULE, local_service_up, [g_service()]},
           {call, ?MODULE, local_service_down, [g_service()]},
           {call, ?MODULE, local_service_kill, [g_service(), S]},
           {call, ?MODULE, local_node_up, []},
           {call, ?MODULE, local_node_down, []},
           {call, ?MODULE, remote_service_up, [g_node(), g_services()]},
           {call, ?MODULE, remote_service_down, [g_node()]},
           {call, ?MODULE, remote_service_down_disterl, [g_node()]}
          ]).

precondition(S, {call, _, local_service_kill, [Service, S]}) ->
    orddict:is_key(Service, S#state.service_pids);
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
    S2 = services_up(Node, Services, S),
    node_up(Node, S2);

next_state(S, _Res, {call, _, Fn, [Node]})
  when Fn == remote_service_down; Fn == remote_service_down_disterl ->
    node_down(Node, S).




postcondition(S, {call, _, local_service_up, [Service]}, _Res) ->
    S2 = service_up(node(), Service, S),
    deep_validate(S2);

postcondition(S, {call, _, local_service_down, [Service]}, _Res) ->
    S2 = service_down(node(), Service, S),
    deep_validate(S2);

postcondition(S, {call, _, local_service_kill, [Service, _]}, _Res) ->
    S2 = service_down(node(), Service, S),
    deep_validate(S2);

postcondition(S, {call, _, local_node_up, _}, _Res) ->
    S2 = node_up(node(), S),
    deep_validate(S2);

postcondition(S, {call, _, local_node_down, _}, _Res) ->
    S2 = node_down(node(), S),
    deep_validate(S2);

postcondition(S, {call, _, remote_service_up, [Node, Services]}, _Res) ->
    S2 = node_up(Node, services_up(Node, Services, S)),
    deep_validate(S2);

postcondition(S, {call, _, Fn, [Node]}, _Res)
  when Fn == remote_service_down; Fn == remote_service_down_disterl ->
    S2 = node_down(Node, S),
    deep_validate(S2);

postcondition(_S, _Call, _Res) ->
    true.


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


%% ====================================================================
%% Generators
%% ====================================================================

g_service() ->
    oneof([s1, s2, s3, s4]).

g_node() ->
    oneof(['n1@127.0.0.1', 'n2@127.0.0.1', 'n3@127.0.0.1']).

g_services() ->
    list(elements([s1, s2, s3, s4])).


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

-endif.
