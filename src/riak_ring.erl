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
%      of the chstate record, which should be treated as opaque by
%      other modules.  Riak nodes exchange instances of
%      these records via gossip in order to converge on a common
%      view of node/partition ownership.

-module(riak_ring).
-include_lib("eunit/include/eunit.hrl").

-export([fresh/0,fresh/1,fresh/2,preflist/2,
	 owner_node/1,all_members/1,num_partitions/1,all_owners/1,
         transfer_node/3,reconcile/2, my_indices/1,
	 index_owner/2,diff_nodes/2,random_node/1, random_other_index/1,
         get_meta/2, update_meta/3, equal_rings/2]).	 

% @type riak_ring(). The opaque data type used for partition ownership.
-record(chstate, {
    nodename, % the Node responsible for this chstate
    vclock,   % for this chstate object, entries are {Node, Ctr}
    chring,   % chash ring of {IndexAsInt, Node} mappings
    meta      % dict of cluster-wide other data (primarily bucket N-value, etc)
}). 

% @type meta_entry(). Record for each entry in #chstate.meta
-record(meta_entry, {
    value,    % The value stored under this entry
    lastmod   % The last modified time of this entry, 
              %  from calendar:datetime_to_gregorian_seconds(
              %                             calendar:universal_time()), 
}).

% @doc This is used only when this node is creating a brand new cluster.
% @spec fresh() -> chstate()
fresh() ->
    % use this when starting a new cluster via this node
    fresh(node()).

% @doc Equivalent to fresh/0 but allows specification of the local node name.
%      Called by fresh/0, and otherwise only intended for testing purposes.
% @spec fresh(NodeName :: term()) -> chstate()
fresh(NodeName) ->
    fresh(riak:get_app_env(ring_creation_size), NodeName).

% @doc Equivalent to fresh/1 but allows specification of the ring size.
%      Called by fresh/1, and otherwise only intended for testing purposes.
% @spec fresh(RingSize :: integer(), NodeName :: term()) -> chstate()
fresh(RingSize, NodeName) ->
    #chstate{nodename=NodeName,
	    vclock=vclock:fresh(),
	    chring=chash:fresh(RingSize, NodeName),
            meta=dict:new()}.

% @doc Return all partition indices owned by the node executing this function.
% @spec my_indices(State :: chstate()) -> [integer()]
my_indices(State) ->
    [I || {I,Owner} <- ?MODULE:all_owners(State), Owner =:= node()].

% @doc Return a partition index not owned by the node executing this function.
%      If this node owns all partitions, return any index.
% @spec random_other_index(State :: chstate()) -> integer()
random_other_index(State) ->
    L = [I || {I,Owner} <- ?MODULE:all_owners(State), Owner =/= node()],
    case L of
        [] -> hd(my_indices(State));
        _ -> lists:nth(crypto:rand_uniform(1, length(L)+1), L)
    end.

% @doc Return the node that owns the given index.
% @spec index_owner(State :: chstate(), Idx :: integer()) -> Node :: term()
index_owner(State, Idx) ->
    hd([Owner || {I, Owner} <- ?MODULE:all_owners(State), I =:= Idx]).

% @doc Return the node that is responsible for a given chstate.
% owner_node(State :: chstate()) -> Node :: term()
owner_node(State) ->
    State#chstate.nodename.

% @doc Produce a list of all nodes that own any partitions.
% @spec all_members(State :: chstate()) -> [Node :: term()]
all_members(State) ->
    chash:members(State#chstate.chring).

% @doc Return a randomly-chosen node from amongst the owners.
% @spec random_node(State :: chstate()) -> Node :: term()
random_node(State) ->
    L = all_members(State),
    lists:nth(crypto:rand_uniform(1, length(L)+1), L).

% @doc Provide all ownership information in the form of {Index,Node} pairs.
% @spec all_owners(State :: chstate()) -> [{Index :: integer(), Node :: term()}]
all_owners(State) ->
    chash:nodes(State#chstate.chring).

% @doc For two rings, return the list of owners that have differing ownership.
% @spec diff_nodes(State1,State2) -> [node()]
diff_nodes(State1,State2) ->
    AO = lists:zip(all_owners(State1),all_owners(State2)),
    AllDiff = [[N1,N2] || {{I,N1},{I,N2}} <- AO, N1 =/= N2],
    lists:usort(lists:flatten(AllDiff)).

% @doc Return the number of partitions in this Riak ring.
% @spec num_partitions(State :: chstate()) -> integer()
num_partitions(State) ->
    chash:size(State#chstate.chring).

% @doc For a given object key, produce the ordered list of
%      {partition,node} pairs that could be responsible for that object.
% @spec preflist(Key :: binary(), State :: chstate()) ->
%                                 [{Index :: integer(), Node :: term()}]
preflist(Key, State) -> chash:successors(Key, State#chstate.chring).

% @spec transfer_node(Idx :: integer(), Node :: term(), MyState :: chstate()) ->
%           chstate()
transfer_node(Idx, Node, MyState) ->
    case chash:lookup(Idx, MyState#chstate.chring) of
	Node ->
	    MyState;
	_ ->
	    Me = MyState#chstate.nodename,
	    VClock = vclock:increment(Me, MyState#chstate.vclock),
	    CHRing = chash:update(Idx, Node, MyState#chstate.chring),
	    #chstate{nodename=Me,vclock=VClock,chring=CHRing,
                    meta=MyState#chstate.meta}
    end.

ancestors(RingStates) ->
    Ancest = [[O2 || O2 <- RingStates,
     vclock:descends(O1#chstate.vclock,O2#chstate.vclock),
     (vclock:descends(O2#chstate.vclock,O1#chstate.vclock) == false)]
		|| O1 <- RingStates],
    lists:flatten(Ancest).

% @doc Incorporate another node's state into our view of the Riak world.
% @spec reconcile(ExternState :: chstate(), MyState :: chstate()) ->
%       {no_change, chstate()} | {new_ring, chstate()}
reconcile(ExternState, MyState) ->
    case vclock:equal(MyState#chstate.vclock, vclock:fresh()) of
        true -> 
            {new_ring, #chstate{nodename=MyState#chstate.nodename,
                                vclock=ExternState#chstate.vclock,
                                chring=ExternState#chstate.chring,
                                meta=ExternState#chstate.meta}};
        false ->
            case ancestors([ExternState, MyState]) of
                [OlderState] ->
                    case vclock:equal(OlderState#chstate.vclock,
                                      MyState#chstate.vclock) of
                        true ->
                            {new_ring,
                             #chstate{nodename=MyState#chstate.nodename,
                                      vclock=ExternState#chstate.vclock,
                                      chring=ExternState#chstate.chring,
                                      meta=ExternState#chstate.meta}};
                        false -> {no_change, MyState}
                    end;
                [] -> 
                    case equal_rings(ExternState,MyState) of
                        true -> {no_change, MyState};
                        false -> {new_ring, reconcile(MyState#chstate.nodename,
                                                      ExternState, MyState)}
                    end
            end
    end.

% @private
equal_rings(_A=#chstate{chring=RA,meta=MA},_B=#chstate{chring=RB,meta=MB}) ->
    MDA = lists:sort(dict:to_list(MA)),
    MDB = lists:sort(dict:to_list(MB)),
    case MDA =:= MDB of
        false -> false;
        true -> RA =:= RB
    end.

% @doc If two states are mutually non-descendant, merge them anyway.
%      This can cause a bit of churn, but should converge.
% @spec reconcile(MyNodeName :: term(),
%                 StateA :: chstate(), StateB :: chstate())
%              -> chstate()
reconcile(MyNodeName, StateA, StateB) ->
    % take two states (non-descendant) and merge them
    VClock = vclock:increment(MyNodeName,
				 vclock:merge([StateA#chstate.vclock,
					       StateB#chstate.vclock])),
    CHRing = chash:merge_rings(StateA#chstate.chring,StateB#chstate.chring),
    Meta = merge_meta(StateA#chstate.meta, StateB#chstate.meta),
    #chstate{nodename=MyNodeName,
	    vclock=VClock,
	    chring=CHRing,
            meta=Meta}.

merge_meta(M1,M2) ->
    dict:merge(fun(_,D1,D2) -> pick_val(D1,D2) end, M1, M2).


pick_val(M1,M2) ->
    case M1#meta_entry.lastmod > M2#meta_entry.lastmod of
        true -> M1;
        false -> M2
    end.
    

% @doc Return a value from the cluster metadata dict
% @spec get_meta(Key :: term(), State :: chstate()) -> 
%    {ok, term()} | error | undefined
get_meta(Key, State) -> 
    case dict:find(Key, State#chstate.meta) of
        error -> undefined;
        {ok, M} -> {ok, M#meta_entry.value}
    end.

                
% @doc Set a key in the cluster metadata dict
% @spec update_meta(Key :: term(), Val :: term(), State :: chstate()) ->
%     State :: chstate() | error
update_meta(Key, Val, State) ->
    M = #meta_entry { 
        lastmod = calendar:datetime_to_gregorian_seconds(
                    calendar:universal_time()),
        value = Val
    },    
    VClock = vclock:increment(State#chstate.nodename, State#chstate.vclock),
    State#chstate{vclock=VClock, meta=dict:store(Key, M, State#chstate.meta)}.

sequence_test() ->
    I1 = 365375409332725729550921208179070754913983135744,
    I2 = 730750818665451459101842416358141509827966271488,
    A = fresh(4,a),
    B1 = A#chstate{nodename=b},
    B2 = transfer_node(I1, b, B1),
    ?assertEqual(B2, transfer_node(I1, b, B2)),
    {new_ring, A1} = reconcile(B1,A),
    C1 = A#chstate{nodename=c},
    C2 = transfer_node(I1, c, C1),
    {new_ring, A2} = reconcile(C2,A1),
    {new_ring, A3} = reconcile(B2,A2),
    C3 = transfer_node(I2,c,C2),
    {new_ring, C4} = reconcile(A3,C3),
    {new_ring, A4} = reconcile(C4,A3),
    {new_ring, B3} = reconcile(A4,B2),
    ?assertEqual(A4#chstate.chring, B3#chstate.chring),
    ?assertEqual(B3#chstate.chring, C4#chstate.chring).

param_fresh_test() ->
    application:set_env(riak,ring_creation_size,4),
    ?assertEqual(fresh(), fresh(4,node())),
    ?assertEqual(owner_node(fresh()),node()).

index_test() ->
    Ring0 = fresh(2,node()),
    Ring1 = transfer_node(0,x,Ring0),
    ?assertEqual(0,random_other_index(Ring0)),
    ?assertEqual(0,random_other_index(Ring1)),
    ?assertEqual(node(),index_owner(Ring0,0)),
    ?assertEqual(x,index_owner(Ring1,0)),
    ?assertEqual(lists:sort([x,node()]),lists:sort(diff_nodes(Ring0,Ring1))).

reconcile_test() ->
    Ring0 = fresh(2,node()),
    Ring1 = transfer_node(0,x,Ring0),
    ?assertEqual({no_change,Ring1},reconcile(fresh(2,someone_else),Ring1)),
    RingB0 = fresh(2,node()),
    RingB1 = transfer_node(0,x,RingB0),
    RingB2 = RingB1#chstate{nodename=b},
    ?assertEqual({no_change,RingB2},reconcile(Ring1,RingB2)).

metadata_inequality_test() ->
    Ring0 = fresh(2,node()),
    Ring1 = update_meta(key,val,Ring0),
    ?assertNot(equal_rings(Ring0,Ring1)),
    ?assertEqual(Ring1#chstate.meta,
                 merge_meta(Ring0#chstate.meta,Ring1#chstate.meta)),
    timer:sleep(1001), % ensure that lastmod is at least a second later
    Ring2 = update_meta(key,val2,Ring1),
    ?assertEqual(get_meta(key,Ring2),
                 get_meta(key,#chstate{meta=
                            merge_meta(Ring1#chstate.meta,
                                       Ring2#chstate.meta)})),
    ?assertEqual(get_meta(key,Ring2),
                 get_meta(key,#chstate{meta=
                            merge_meta(Ring2#chstate.meta,
                                       Ring1#chstate.meta)})).

