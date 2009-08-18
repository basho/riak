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

-module(riak_ring_gossiper).

-export([start_link/0, init/0]).
-export([gossip_to/1, get_ring_from/1, remove_from_cluster/1]).

start_link() -> {ok, spawn_link(node(), ?MODULE, init, [])}.

%% @private
init() ->
    register(riak_ring_gossiper, self()),
    loop(write).

loop(Write) ->
    MaxInterval = case Write of
        write -> 600;
        no_write -> riak:get_app_env(gossip_interval)
    end,
    Interval = random:uniform(MaxInterval),
    receive
        {gossip_ring, ExternRing} ->
            {ok, MyRing} = riak_ring_manager:get_my_ring(),
            case riak_ring:reconcile(ExternRing, MyRing) of
                {no_change, _} ->
                    loop(Write);
                {new_ring, NewRing} ->
                    riak_ring_manager:set_my_ring(NewRing),
                    {ok, MyNewRing} = maybe_claim(),
                    riak_ring_manager:set_my_ring(MyNewRing),
                    Changed = riak_ring:diff_nodes(MyNewRing, NewRing),
                    [gossip_to(X) ||
                        X <- [riak_ring:random_node(MyNewRing)|Changed],
                        X =/= node()],
                    riak_eventer:notify(riak_ring_gossiper, changed_ring, 
                                        {gossip_changed, length(Changed)}),
                    loop(write)
            end;
        {set_ring, ExternRing} ->
            riak_eventer:notify(riak_ring_gossiper,
                                fresh_ring, fresh_ring),
            riak_ring_manager:set_my_ring(
              riak_ring:fresh_from_extern(ExternRing, node())),
            {ok, MyNewRing} = maybe_claim(),
            riak_ring_manager:set_my_ring(MyNewRing),
            Changed = riak_ring:diff_nodes(MyNewRing, ExternRing),
            [gossip_to(X) ||
                X <- [riak_ring:random_node(MyNewRing)|Changed],
                X =/= node()],
            riak_eventer:notify(riak_ring_gossiper, changed_ring, 
                                {set_changed, length(Changed)}),
            loop(write);
        {get_ring, RemoteNode} ->
            set_remote_ring(RemoteNode),
            loop(Write)
    after Interval ->
            riak_eventer:notify(riak_ring_gossiper, interval, interval),
            {ok, MyRing} = riak_ring_manager:get_my_ring(),
            Indices = [I || {I,_} <- riak_ring:all_owners(MyRing)],
            gen_server:cast({riak_vnode_master, node()}, {start_vnode, 
               lists:nth(crypto:rand_uniform(1, length(Indices)+1), Indices)}),
            case Write of
                no_write -> nop;
                write -> riak_ring_manager:write_ringfile()
            end,
            Me = node(),
            case riak_ring:random_node(MyRing) of
                Me -> nop;
                RandNode -> gossip_to(RandNode)
            end,
            loop(no_write)                         
    end.

gossip_to(RemoteNode) ->
    riak_eventer:notify(riak_ring_gossiper, send, RemoteNode),
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    riak_connect:cast(RemoteNode, {gossip_ring, MyRing}).

set_remote_ring(RemoteNode) ->
    riak_eventer:notify(riak_ring_gossiper, set_remote_ring, RemoteNode),
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    riak_connect:cast(RemoteNode, {set_ring, MyRing}).

get_ring_from(RemoteNode) ->
    riak_eventer:notify(riak_ring_gossiper, get_remote_ring, RemoteNode),
    riak_connect:cast(RemoteNode, {get_ring, node()}).

maybe_claim() ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    {WMod, WFun} = riak:get_app_env(wants_claim_fun),
    case apply(WMod, WFun, [Ring]) of
        no -> {ok, Ring};
        {yes, Wanted} ->
            riak_eventer:notify(riak_ring_gossiper, want_claim, Wanted),
            do_claim(Ring, Wanted)
    end.

do_claim(Ring,Wanted) ->
    case Wanted of
        0 -> {ok, Ring};
        _ ->
            {CMod, CFun} = riak:get_app_env(choose_claim_fun),
            do_claim(apply(CMod, CFun, [Ring]), Wanted-1)
    end.

remove_from_cluster(ExitingNode) ->
    rpc:call(ExitingNode, application, set_env, [riak, wants_claim_fun,
                                        {riak_claim, never_wants_claim}]),
    % ignore return of rpc as this should succeed even if node is offline
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    AllOwners = riak_ring:all_owners(Ring),
    AllIndices = [I || {I,_Owner} <- AllOwners],
    Indices = [I || {I,Owner} <- AllOwners, 
                       Owner =:= ExitingNode],
    riak_eventer:notify(riak_ring_gossiper, remove_from_cluster,
                        {ExitingNode, length(Indices)}),
    Others = lists:delete(ExitingNode, riak_ring:all_members(Ring)),
    ExitRing = lists:foldl(
      fun(I,R) ->
          riak_ring:transfer_node(I,
            lists:nth(crypto:rand_uniform(1,length(Others)+1),Others),R) end, 
      Ring, Indices),
    riak_ring_manager:set_my_ring(ExitRing),    
    [gossip_to(X) || X <- Others],
    [gen_server:cast({riak_vnode_master, ExitingNode}, {start_vnode, P}) ||
        P <- AllIndices].
        


