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

% @doc riak_ring manages a riak node's local view of partition
%      ownership.  The functions in this module revolve around use
%      of the hstate record, which should be treated as opaque by
%      other modules.  Riak nodes exchange instances of
%      these records via gossip in order to converge on a common
%      view of node/partition ownership.

-module(riak_ring).
-include_lib("eunit/include/eunit.hrl").

-export([fresh/0,fresh/1,fresh/2,preflist/2,filtered_preflist/3,
	 owner_node/1,all_members/1,num_partitions/1,all_owners/1,
         transfer_node/3,reconcile/2, my_indices/1,
	 index_owner/2,diff_nodes/2,random_node/1, random_other_index/1,
         get_meta/2, update_meta/3]).	 

% @type riak_ring(). The opaque data type used for partition ownership.
-record(hstate, {nodename, % the Node responsible for this hstate
		 vclock, % for this hstate object, entries are {Node, Ctr}
		 ring, % chash ring of {IndexAsInt, Node} mappings
                 meta}). % dict of cluster-wide other data
                         % (primarily bucket N-value, etc)

% need to re-create this test now that we no longer have fresh_from_extern
%% sequence_test() ->
%%     test_ensure(crypto),
%%     I1 = 365375409332725729550921208179070754913983135744,
%%     I2 = 730750818665451459101842416358141509827966271488,
%%     A = fresh(4,a),
%%     B1 = fresh_from_extern(A,b),
%%     B2 = transfer_node(I1, b, B1),
%%     {new_ring, A1} = reconcile(B1,A),
%%     C1 = fresh_from_extern(A,c),
%%     C2 = transfer_node(I1, c, C1),
%%     {new_ring, A2} = reconcile(C2,A1),
%%     {new_ring, A3} = reconcile(B2,A2),
%%     C3 = transfer_node(I2,c,C2),
%%     {new_ring, C4} = reconcile(A3,C3),
%%     {new_ring, A4} = reconcile(C4,A3),
%%     {new_ring, B3} = reconcile(A4,B2),
%%     (A4#hstate.ring == B3#hstate.ring) == (
%%       B3#hstate.ring == C4#hstate.ring).
%% % @spec test_ensure(App :: atom()) -> ok
%% test_ensure(App) ->
%%     case [X || {X,_,_} <- application:which_applications(), X == App] of
%% 	[] ->
%% 	    application:start(App);
%% 	_ ->
%% 	    ok
%%     end.

% @doc This is used only when this node is creating a brand new cluster.
% @spec fresh() -> hstate()
fresh() ->
    % use this when starting a new cluster via this node
    fresh(node()).

% @doc Equivalent to fresh/0 but allows specification of the local node name.
%      Called by fresh/0, and otherwise only intended for testing purposes.
% @spec fresh(NodeName :: term()) -> hstate()
fresh(NodeName) ->
    fresh(riak:get_app_env(ring_creation_size), NodeName).

% @doc Equivalent to fresh/1 but allows specification of the ring size.
%      Called by fresh/1, and otherwise only intended for testing purposes.
% @spec fresh(RingSize :: integer(), NodeName :: term()) -> hstate()
fresh(RingSize, NodeName) ->
    #hstate{nodename=NodeName,
	    vclock=vclock:fresh(),
	    ring=chash:fresh(RingSize, NodeName),
            meta=dict:new()}.

% @doc Return all partition indices owned by the node executing this function.
% @spec my_indices(State :: hstate()) -> [integer()]
my_indices(State) ->
    [I || {I,Owner} <- ?MODULE:all_owners(State), Owner =:= node()].

% @doc Return a partition index not owned by the node executing this function.
%      If this node owns all partitions, return any index.
% @spec random_other_index(State :: hstate()) -> integer()
random_other_index(State) ->
    L = [I || {I,Owner} <- ?MODULE:all_owners(State), Owner =/= node()],
    case L of
        [] -> hd(my_indices(State));
        _ -> lists:nth(crypto:rand_uniform(1, length(L)+1), L)
    end.

% @doc Return the node that owns the given index.
% @spec index_owner(State :: hstate(), Idx :: integer()) -> Node :: term()
index_owner(State, Idx) ->
    hd([Owner || {I, Owner} <- ?MODULE:all_owners(State), I =:= Idx]).

% @doc Return the node that is responsible for a given hstate.
% owner_node(State :: hstate()) -> Node :: term()
owner_node(State) ->
    State#hstate.nodename.

% @doc Produce a list of all nodes that own any partitions.
% @spec all_members(State :: hstate()) -> [Node :: term()]
all_members(State) ->
    chash:members(State#hstate.ring).

% @doc Return a randomly-chosen node from amongst the owners.
% @spec random_node(State :: hstate()) -> Node :: term()
random_node(State) ->
    L = lists:usort(all_members(State)),
    lists:nth(crypto:rand_uniform(1, length(L)+1), L).

% @doc Provide all ownership information in the form of {Index,Node} pairs.
% @spec all_owners(State :: hstate()) -> [{Index :: integer(), Node :: term()}]
all_owners(State) ->
    chash:nodes(State#hstate.ring).

% @doc For two rings, return the list of owners that have differing ownership.
% @spec diff_nodes(State1,State2) -> [node()]
diff_nodes(State1,State2) ->
    AO = lists:zip(all_owners(State1),all_owners(State2)),
    AllDiff = [[N1,N2] || {{I,N1},{I,N2}} <- AO, N1 =/= N2],
    lists:usort(lists:flatten(AllDiff)).

% @doc Return the number of partitions in this Riak ring.
% @spec num_partitions(State :: hstate()) -> integer()
num_partitions(State) ->
    chash:size(State#hstate.ring).

% @doc For a given object key, produce the ordered list of
%      {partition,node} pairs that could be responsible for that object.
% @spec preflist(Key :: binary(), State :: hstate()) ->
%                                 [{Index :: integer(), Node :: term()}]
preflist(Key, State) ->
    chash:successors(Key, State#hstate.ring).

filtered_preflist(Key, State, N) ->
    Preflist = preflist(Key, State),
    Try1 = filtered_preflist1(Preflist, [], []),
    case length(Try1) >= N of
        true -> Try1;
        false -> Preflist
    end.
filtered_preflist1([], _Seen, Acc) ->
    Acc;
filtered_preflist1([{I,Node}|Preflist], Seen, Acc) ->
    case lists:member(Node, Seen) of
        true -> filtered_preflist1(Preflist, Seen, Acc);
        false -> filtered_preflist1(Preflist, [Node|Seen], [{I,Node}|Acc])
    end.

% @doc Transfer ownership of partition at Idx to Node.
% @spec transfer_node(Idx :: integer(), Node :: term(), MyState :: hstate()) ->
%           hstate()
transfer_node(Idx, Node, MyState) ->
    case chash:lookup(Idx, MyState#hstate.ring) of
	Node ->
	    MyState;
	_ ->
	    Me = MyState#hstate.nodename,
	    VClock = vclock:increment(Me, MyState#hstate.vclock),
	    Ring = chash:update(Idx, Node, MyState#hstate.ring),
	    #hstate{nodename=Me,vclock=VClock,ring=Ring,
                    meta=MyState#hstate.meta}
    end.

ancestors(RingStates) ->
    Ancest = [[O2 || O2 <- RingStates,
     vclock:descends(O1#hstate.vclock,O2#hstate.vclock),
     (vclock:descends(O2#hstate.vclock,O1#hstate.vclock) == false)]
		|| O1 <- RingStates],
    lists:flatten(Ancest).

% @doc Incorporate another node's state into our view of the Riak world.
% @spec reconcile(ExternState :: hstate(), MyState :: hstate()) ->
%       {no_change, hstate()} | {new_ring, hstate()}
reconcile(ExternState, MyState) ->
    case vclock:equal(MyState#hstate.vclock, vclock:fresh()) of
        true -> 
            {new_ring, #hstate{nodename=MyState#hstate.nodename,
                               vclock=ExternState#hstate.vclock,
                               ring=ExternState#hstate.ring,
                               meta=ExternState#hstate.meta}};
        false ->
            case ancestors([ExternState, MyState]) of
                [OlderState] ->
                    case vclock:equal(OlderState#hstate.vclock,
                                      MyState#hstate.vclock) of
                        true ->
                            {new_ring, #hstate{nodename=MyState#hstate.nodename,
                                               vclock=ExternState#hstate.vclock,
                                               ring=ExternState#hstate.ring,
                                               meta=ExternState#hstate.meta}};
                        false -> {no_change, MyState}
                    end;
                [] -> 
                    case equal_rings(ExternState,MyState) of
                        true -> {no_change, MyState};
                        false -> {new_ring, reconcile(MyState#hstate.nodename,
                                                      ExternState, MyState)}
                    end
            end
    end.

% @private
equal_rings(_A=#hstate{ring=RA,meta=MA},_B=#hstate{ring=RB,meta=MB}) ->
    MDA = lists:sort(dict:to_list(MA)),
    MDB = lists:sort(dict:to_list(MB)),
    case MDA =:= MDB of
        false -> false;
        true -> RA =:= RB
    end.

% @doc If two states are mutually non-descendant, merge them anyway.
%      This can cause a bit of churn, but should converge.
% @spec reconcile(MyNodeName :: term(), StateA :: hstate(), StateB :: hstate())
%              -> hstate()
reconcile(MyNodeName, StateA, StateB) ->
    % take two states (non-descendant) and merge them
    VClock = vclock:increment(MyNodeName,
				 vclock:merge([StateA#hstate.vclock,
					       StateB#hstate.vclock])),
    Ring = chash:merge_rings(StateA#hstate.ring,StateB#hstate.ring),
    Meta = merge_meta(StateA#hstate.meta, StateB#hstate.meta),
    #hstate{nodename=MyNodeName,
	    vclock=VClock,
	    ring=Ring,
            meta=Meta}.

merge_meta(M1,M2) ->
    dict:merge(fun(_,D1,D2) -> pick_val(D1,D2) end, M1, M2).

pick_val(D1,D2) ->
    {ok, L1} = dict:find(lastmod, D1),
    {ok, L2} = dict:find(lastmod, D2),
    case L1 > L2 of
        true -> D1;
        false -> D2
    end.

% @doc Return a value from the cluster metadata dict
% @spec get_meta(Key :: term(), State :: hstate()) -> 
%    {ok, term()} | error | undefined
get_meta(Key, State) -> 
    case dict:find(Key, State#hstate.meta) of
        error -> undefined;
        {ok, InnerDict} -> dict:find(Key, InnerDict)
    end.
                
% @doc Set a key in the cluster metadata dict
% @spec update_meta(Key :: term(), Val :: term(), State :: hstate()) ->
%     State :: hstate() | error
update_meta(Key, Val, State) ->
    InnerDict = case dict:find(Key, State#hstate.meta) of
        error -> dict:new();
        {ok, X} -> X
    end,
    I1 = dict:store(lastmod,
                    calendar:datetime_to_gregorian_seconds(
                      calendar:universal_time()),
                    dict:store(Key, Val, InnerDict)),
    VClock = vclock:increment(State#hstate.nodename,
                              State#hstate.vclock),
    State#hstate{vclock=VClock, meta=dict:store(Key, I1, State#hstate.meta)}.
