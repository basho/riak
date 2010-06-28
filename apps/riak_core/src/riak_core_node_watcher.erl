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
-module(riak_core_node_watcher).

-behaviour(gen_server).

%% API
-export([start_link/0,
         service_up/2,
         service_down/1,
         node_up/0,
         node_down/0,
         services/0, services/1,
         nodes/1,
         avsn/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { status = up,
                 services = [],
                 peers = [],
                 avsn = 0,
                 bcast_tref,
                 bcast_mod = {gen_server, abcast}}).


%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

service_up(Id, Pid) ->
    gen_server:call(?MODULE, {service_up, Id, Pid}).

service_down(Id) ->
    gen_server:call(?MODULE, {service_down, Id}).

node_up() ->
    gen_server:call(?MODULE, {node_status, up}).

node_down() ->
    gen_server:call(?MODULE, {node_status, down}).

services() ->
    ordsets:from_list([Service || [Service] <- ets:match(?MODULE, {{'_', '$1'}, '_'})]).

services(Node) ->
    [Service || [Service] <- ets:match(?MODULE, {{Node, '$1'}, '_'})].

nodes(Service) ->
    [Node || [Node] <- ets:match(?MODULE, {{'$1', Service}, '_'})].


%% ===================================================================
%% Test API
%% ===================================================================

avsn() ->
    gen_server:call(?MODULE, get_avsn).


%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
    %% Trap exits so that terminate/2 will get called
    process_flag(trap_exit, true),

    %% Setup callback notification for ring changes; note that we use the
    %% supervised variation so that the callback gets removed if this process
    %% exits
    watch_for_ring_events(),

    %% Watch for node up/down events
    net_kernel:monitor_nodes(true),

    %% Setup ETS table to track node status
    ets:new(?MODULE, [protected, named_table]),

    {ok, schedule_broadcast(#state{})}.

handle_call({set_bcast_mod, Module, Fn}, _From, State) ->
    %% Call available for swapping out how broadcasts are generated
    {reply, ok, State#state {bcast_mod = {Module, Fn}}};

handle_call(get_avsn, _From, State) ->
    {reply, State#state.avsn, State};

handle_call({service_up, Id, Pid}, _From, State) ->
    %% Update the set of active services locally
    Services = ordsets:add_element(Id, State#state.services),
    S2 = State#state { services = Services },

    %% Remove any existing mrefs for this service
    delete_service_mref(Id),

    %% Setup a monitor for the Pid representing this service
    Mref = erlang:monitor(process, Pid),
    erlang:put(Mref, Id),
    erlang:put(Id, Mref),

    %% Update our local ETS table and broadcast
    S3 = local_update(S2),
    {reply, ok, update_avsn(S3)};

handle_call({service_down, Id}, _From, State) ->
    %% Update the set of active services locally
    Services = ordsets:del_element(Id, State#state.services),
    S2 = State#state { services = Services },

    %% Remove any existing mrefs for this service
    delete_service_mref(Id),

    %% Update local ETS table and broadcast
    S3 = local_update(S2),
    {reply, ok, update_avsn(S3)};

handle_call({node_status, Status}, _From, State) ->
    Transition = {State#state.status, Status},
    S2 = case Transition of
             {up, down} -> %% up -> down
                 local_delete(State#state { status = down });

             {down, up} -> %% down -> up
                 local_update(State#state { status = up });

             {Status, Status} -> %% noop
                 State
    end,
    {reply, ok, update_avsn(S2)}.


handle_cast({ring_update, R}, State) ->
    %% Ring has changed; determine what peers are new to us
    %% and broadcast out current status to those peers.
    Peers0 = ordsets:from_list(riak_core_ring:all_members(R)),
    Peers = ordsets:del_element(node(), Peers0),

    S2 = peers_update(Peers, State),
    {noreply, update_avsn(S2)};

handle_cast({up, Node, Services}, State) ->
    S2 = node_up(Node, Services, State),
    {noreply, update_avsn(S2)};

handle_cast({down, Node}, State) ->
    node_down(Node, State),
    {noreply, update_avsn(State)}.


handle_info({nodeup, _Node}, State) ->
    %% Ignore node up events; nothing to do here...
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    node_down(Node, State),
    {noreply, update_avsn(State)};

handle_info({'DOWN', Mref, _, _Pid, _Info}, State) ->
    %% A sub-system monitored process has terminated. Identify
    %% the sub-system in question and notify our peers.
    case erlang:get(Mref) of
        undefined ->
            %% No entry found for this monitor; ignore the message
            {noreply, update_avsn(State)};

        Id ->
            %% Remove the id<->mref entries in the pdict
            delete_service_mref(Id),

            %% Update our list of active services and ETS table
            Services = ordsets:del_element(Id, State#state.services),
            S2 = State#state { services = Services },
            local_update(S2),
            {noreply, update_avsn(S2)}
    end;

handle_info({gen_event_EXIT, _, _}, State) ->
    %% Ring event handler has been removed for some reason; re-register
    watch_for_ring_events(),
    {noreply, update_avsn(State)};

handle_info(broadcast, State) ->
    S2 = broadcast(State#state.peers, State),
    {noreply, S2}.


terminate(_Reason, State) ->
    %% Let our peers know that we are shutting down
    broadcast(State#state.peers, State#state { status = down }).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
%% ====================================================================

update_avsn(State) ->
    State#state { avsn = State#state.avsn + 1 }.

watch_for_ring_events() ->
    Self = self(),
    Fn = fun(R) ->
                 gen_server:cast(Self, {ring_update, R})
         end,
    riak_core_ring_events:add_sup_callback(Fn).

delete_service_mref(Id) ->
    %% Cleanup the monitor if one exists
    case erlang:get(Id) of
        undefined ->
            ok;
        Mref ->
            erlang:erase(Mref),
            erlang:erase(Id),
            erlang:demonitor(Mref)
    end.


broadcast(Nodes, State) ->
    case (State#state.status) of
        up ->
            Msg = {up, node(), State#state.services};
        down ->
            Msg = {down, node()}
    end,
    {Mod, Fn} = State#state.bcast_mod,
    Mod:Fn(Nodes, ?MODULE, Msg),
    schedule_broadcast(State).

schedule_broadcast(State) ->
    case (State#state.bcast_tref) of
        undefined ->
            ok;
        OldTref ->
            erlang:cancel_timer(OldTref)
    end,
    Interval = app_helper:get_env(riak_core, gossip_interval),
    Tref = erlang:send_after(Interval, self(), broadcast),
    State#state { bcast_tref = Tref }.

is_peer(Node, State) ->
    ordsets:is_element(Node, State#state.peers).

is_node_up(Node) ->
    ets:member(?MODULE, Node).


node_up(Node, Services, State) ->
    case is_peer(Node, State) of
        true ->
            %% Before we alter the ETS table, see if this node was previously down. In
            %% that situation, we'll go ahead broadcast out.
            S2 = case is_node_up(Node) of
                     false ->
                         broadcast([Node], State);
                     true ->
                         State
                 end,

            case node_update(Node, Services) of
                [] ->
                    ok;
                AffectedServices ->
                    riak_core_node_watcher_events:service_update(AffectedServices)
            end,
            S2;

        false ->
            State
    end.

node_down(Node, State) ->
    case is_peer(Node, State) of
        true ->
            case node_delete(Node) of
                [] ->
                    ok;
                AffectedServices ->
                    riak_core_node_watcher_events:service_update(AffectedServices)
            end;
        false ->
            ok
    end.


node_delete(Node) ->
    Services = services(Node),
    ets:match_delete(?MODULE, {{Node, '_'}, '_'}),
    ets:delete(?MODULE, Node),
    Services.

node_update(Node, Services) ->
    %% Check the list of up services against what we already
    %% know and determine what's changed (if anything).
    Now = riak_core_util:moment(),
    NewStatus = ordsets:from_list(Services),
    OldStatus = ordsets:from_list(services(Node)),

    Added     = ordsets:subtract(NewStatus, OldStatus),
    Deleted   = ordsets:subtract(OldStatus, NewStatus),
    Unchanged = ordsets:intersection(NewStatus, OldStatus),

    %% Update ets table with changes; make sure to touch unchanged
    %% service with latest timestamp
    [ets:delete(?MODULE, {Node, Ss}) || Ss <- Deleted],
    ets:insert(?MODULE, [{{Node, Ss}, Now} || Ss <- Added ++ Unchanged]),

    %% Keep track of the last time we recv'd data from a node
    ets:insert(?MODULE, {Node, Now}),

    %% Return the list of affected services (added or deleted)
    ordsets:union(Added, Deleted).

local_update(#state { status = down } = State) ->
    %% Ignore subsystem changes when we're marked as down
    State;
local_update(State) ->
    %% Update our local ETS table
    case node_update(node(), State#state.services) of
        [] ->
            %% No material changes; no local notification necessary
            ok;

        AffectedServices ->
            %% Generate a local notification about the affected services and
            %% also broadcast our status
            riak_core_node_watcher_events:service_update(AffectedServices)
    end,
    broadcast(State#state.peers, State).

local_delete(State) ->
    case node_delete(node()) of
        [] ->
            %% No services changed; no local notification required
            State;

        AffectedServices ->
            riak_core_node_watcher_events:service_update(AffectedServices)
    end,
    broadcast(State#state.peers, State).

peers_update(NewPeers, State) ->
    %% Identify what peers have been added and deleted
    Added   = ordsets:subtract(NewPeers, State#state.peers),
    Deleted = ordsets:subtract(State#state.peers, NewPeers),

    %% For peers that have been deleted, remove their entries from
    %% the ETS table; we no longer care about their status
    Services0 = (lists:foldl(fun(Node, Acc) ->
                                    S = node_delete(Node),
                                    S ++ Acc
                            end, [], Deleted)),
    Services = ordsets:from_list(Services0),

    %% Notify local parties if any services are affected by this change
    case Services of
        [] ->
            ok;
        _  ->
            riak_core_node_watcher_events:service_update(Services)
    end,

    %% Broadcast our current status to new peers
    broadcast(Added, State#state { peers = NewPeers }).

