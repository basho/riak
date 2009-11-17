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

%% @doc
%% riak_connect takes care of the mechanics of shuttling a
%% from one node to another upon request by other
%% Riak processes.
%%
%% Additionally, it occasionally checks to make sure the current
%% node has its fair share of partitions, and also sends
%% a copy of the ring to some other random node, ensuring
%% that all nodes eventually synchronize on the same understanding
%% of the Riak cluster. This interval is configurable, but defaults
%% to once per minute.

-module(riak_connect).

-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([send_ring/1, send_ring/2, remove_from_cluster/1]).

-include_lib("eunit/include/eunit.hrl").
-define (SERVER, ?MODULE).

%% send_ring/1 -
%% Send the current node's ring to some other node.
send_ring(ToNode) -> send_ring(node(), ToNode).

%% send_ring/2 -
%% Send the ring from one node to another node. 
%% Does nothing if the two nodes are the same.
send_ring(Node, Node) -> ok;
send_ring(FromNode, ToNode) ->
    gen_server:cast({?SERVER, FromNode}, {send_ring_to, ToNode}).
    

%% @private
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
%% @private
init(_State) -> 
    schedule_next_gossip(),
    {ok, false}.
    
schedule_next_gossip() ->
    MaxInterval = riak:get_app_env(gossip_interval),
    Interval = random:uniform(MaxInterval),
    timer:apply_after(Interval, gen_server, cast, [?SERVER, gossip_ring]). 

stop() -> gen_server:cast(?SERVER, stop).

%% @private
handle_cast({send_ring_to, Node}, RingChanged) ->
    riak_eventer:notify(riak_connect, send_ring_to, Node),
    {ok, MyRing} = riak_ring_manager:get_my_ring(),    
    gen_server:cast({?SERVER, Node}, {reconcile_ring, MyRing}),
    {noreply, RingChanged};
    
handle_cast({reconcile_ring, OtherRing}, RingChanged) ->
    % Compare the two rings, see if there is anything that
    % must be done to make them equal...
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    case riak_ring:reconcile(OtherRing, MyRing) of
        {no_change, _} -> 
            {noreply, RingChanged};
            
        {new_ring, ReconciledRing} ->
            BalancedRing = claim_until_balanced(ReconciledRing),
            riak_ring_manager:set_my_ring(BalancedRing),
            RandomNode = riak_ring:random_node(BalancedRing),
            send_ring(node(), RandomNode),
            riak_eventer:notify(riak_connect, changed_ring, gossip_changed),
            {noreply, true}
    end;
    
handle_cast(gossip_ring, RingChanged) ->
    % First, schedule the next round of gossip...
    schedule_next_gossip(),
    riak_eventer:notify(riak_connect, interval, interval),
    
    % Make sure all vnodes are started...
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    ensure_vnodes_started(MyRing),
    
    % If the ring has changed since our last write,
    % then rewrite the ring...
    case RingChanged of
        true ->
            riak_ring_manager:prune_ringfiles(),
            riak_ring_manager:write_ringfile();
        false -> 
            ignore
    end,
      
    % Finally, gossip the ring to some random other node...
    RandomNode = riak_ring:index_owner(MyRing, riak_ring:random_other_index(MyRing)),
    send_ring(node(), RandomNode),
    {noreply, false};                         

handle_cast(_, State) -> 
    {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
handle_call(_, _From, State) -> {reply, ok, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

claim_until_balanced(Ring) ->
    {WMod, WFun} = riak:get_app_env(wants_claim_fun),
    NeedsIndexes = apply(WMod, WFun, [Ring]),
    case NeedsIndexes of
        no -> 
            Ring;
        {yes, _NumToClaim} ->
            {CMod, CFun} = riak:get_app_env(choose_claim_fun),
            NewRing = CMod:CFun(Ring),
            claim_until_balanced(NewRing)
    end.

ensure_vnodes_started(Ring) ->
    VNodes2Start = case length(riak_ring:all_members(Ring)) of
       1 -> riak_ring:my_indices(Ring);
       _ -> [riak_ring:random_other_index(Ring)| riak_ring:my_indices(Ring)]
    end,
    [begin
        gen_server:cast({riak_vnode_master, node()}, {start_vnode, I}) 
    end|| I <- VNodes2Start].

remove_from_cluster(ExitingNode) ->
    % Set the remote node to stop claiming.
    % Ignore return of rpc as this should succeed even if node is offline
    rpc:call(ExitingNode, application, set_env, [riak, wants_claim_fun, {riak_claim, never_wants_claim}]),
    
    % Get a list of indices owned by the ExitingNode...
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    AllOwners = riak_ring:all_owners(Ring),
    AllIndices = [I || {I,_Owner} <- AllOwners],
    Indices = [I || {I,Owner} <- AllOwners, Owner =:= ExitingNode],
    riak_eventer:notify(riak_connect, remove_from_cluster,
                        {ExitingNode, length(Indices)}),

    % Transfer indexes to other nodes...
    ExitRing = 
        case attempt_simple_transfer(Ring, AllOwners, ExitingNode) of
            {ok, NR} -> NR;
            target_n_fail ->
                %% re-diagonalize
                %% first hand off all claims to *any* one else,
                %% just so rebalance doesn't include exiting node
                Other = hd(lists:delete(ExitingNode, riak_ring:all_members(Ring))),
                TempRing = lists:foldl(fun({I,N}, R) when N == ExitingNode ->
                                               riak_ring:transfer_node(
                                                 I, Other, R);
                                          (_, R) -> R
                                       end,
                                       Ring,
                                       AllOwners),
                riak_claim:claim_rebalance_n(TempRing, Other)
        end,
    riak_ring_manager:set_my_ring(ExitRing),

    % Send the ring to all other rings...
    [send_ring(X) || X <- riak_ring:all_members(Ring)],
    
    %% This line is right!
    [gen_server:cast({riak_vnode_master, ExitingNode}, {start_vnode, P}) ||
        P <- AllIndices].    

attempt_simple_transfer(Ring, Owners, ExitingNode) ->
    attempt_simple_transfer(Ring, Owners,
                            riak:get_app_env(target_n_val, 3),
                            ExitingNode, 0, []).
attempt_simple_transfer(Ring, [{P, Exit}|Rest], TargetN, Exit, Idx, Last) ->
    %% handoff
    case [ N || {N, I} <- Last, Idx-I >= TargetN ] of
        [] ->
            target_n_fail;
        Candidates ->
            %% these nodes don't violate target_n in the reverse direction
            StepsToNext = fun(Node) ->
                                  length(lists:takewhile(
                                           fun({_, Owner}) -> Node /= Owner end,
                                           Rest))
                          end,
            case lists:filter(fun(N) -> 
                                 Next = StepsToNext(N),
                                 (Next >= TargetN) orelse (Next == length(Rest))
                              end,
                              Candidates) of
                [] ->
                    target_n_fail;
                Qualifiers ->
                    %% these nodes don't violate target_n forward
                    Chosen = lists:nth(crypto:rand_uniform(
                                         1, length(Qualifiers)+1),
                                       Qualifiers),
                    %% choose one, and do the rest of the ring
                    attempt_simple_transfer(
                      riak_ring:transfer_node(P, Chosen, Ring),
                      Rest, TargetN, Exit, Idx+1,
                      lists:keyreplace(Chosen, 1, Last, {Chosen, Idx}))
            end
    end;
attempt_simple_transfer(Ring, [{_, N}|Rest], TargetN, Exit, Idx, Last) ->
    %% just keep track of seeing this node
    attempt_simple_transfer(Ring, Rest, TargetN, Exit, Idx+1,
                            lists:keyreplace(N, 1, Last, {N, Idx}));
attempt_simple_transfer(Ring, [], _, _, _, _) ->
    {ok, Ring}.
