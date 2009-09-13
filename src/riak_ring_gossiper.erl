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
                    {ok, MyNewRing} = maybe_claim(NewRing),
                    riak_ring_manager:set_my_ring(MyNewRing),
                    Me = node(),
                    case riak_ring:random_node(MyNewRing) of
                        Me -> nop;
                        RandNode -> gossip_to(RandNode)
                    end,
                    riak_eventer:notify(riak_ring_gossiper, changed_ring, 
                                        gossip_changed),
                    loop(write)
            end;
        {get_ring, RemoteNode} ->
            gossip_to(RemoteNode),
            loop(Write)
    after Interval ->
            riak_eventer:notify(riak_ring_gossiper, interval, interval),
            {ok, MyRing} = riak_ring_manager:get_my_ring(),
            VNodes2Start = case length(riak_ring:all_members(MyRing)) of
               1 -> riak_ring:my_indices(MyRing);
               _ -> [riak_ring:random_other_index(MyRing)|
                     riak_ring:my_indices(MyRing)]
            end,
            [gen_server:cast({riak_vnode_master, node()},
                   {start_vnode, I}) || I <- VNodes2Start],                             
            case Write of
                no_write -> nop;
                write ->
                    riak_ring_manager:prune_ringfiles(),
                    riak_ring_manager:write_ringfile()
            end,
            riak_ring_gossiper:gossip_to(
              riak_ring:index_owner(MyRing,
                                    riak_ring:random_other_index(MyRing))),
            loop(no_write)                         
    end.

gossip_to(RemoteNode) ->
    case lists:member(riak_ring_gossiper, registered()) of
        false -> nop; % only gossip if we can also receive
        true ->
            riak_eventer:notify(riak_ring_gossiper, send, RemoteNode),
            {ok, MyRing} = riak_ring_manager:get_my_ring(),
            riak_connect:cast(RemoteNode, {gossip_ring, MyRing})
    end.

gossip_ring_to(RemoteNode,Ring) ->
    riak_eventer:notify(riak_ring_gossiper, send, RemoteNode),
    riak_connect:cast(RemoteNode, {gossip_ring, Ring}).

get_ring_from(RemoteNode) ->
    riak_eventer:notify(riak_ring_gossiper, get_remote_ring, RemoteNode),
    riak_connect:cast(RemoteNode, {get_ring, node()}).

maybe_claim(Ring) ->
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
    [gossip_ring_to(X,ExitRing) || X <- riak_ring:all_members(Ring)],
    [gen_server:cast({riak_vnode_master, ExitingNode}, {start_vnode, P}) ||
        P <- AllIndices].
        


