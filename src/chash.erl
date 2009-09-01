%% @copyright 2007-2008 Basho Technologies

%% @reference Karger, D.; Lehman, E.; Leighton, T.; Panigrahy, R.; Levine, M.; Lewin, D. (1997). "Consistent hashing and random trees". Proceedings of the twenty-ninth annual ACM symposium on Theory of computing: 654~663. ACM Press New York, NY, USA

% @author Justin Sheehy <justin@basho.com>
% @author Andy Gross <andy@basho.com>

% @doc A consistent hashing implementation.
%      The space described by the ring coincides with SHA-1 hashes,
%      and so any two keys producing the same SHA-1 hash are
%      considered identical within the ring.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%% http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(chash).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([fresh/2,update/3,lookup/2,members/1,size/1,nodes/1,
     successors/2,successors/3,
     predecessors/2,predecessors/3,
     contains_name/2,key_of/1,
     merge_rings/2]).
    
-export ([update_test/0]).

-define(RINGTOP, trunc(math:pow(2,160)-1)).  % SHA-1 space
-include_lib("eunit/include/eunit.hrl").

% @type chash() = {NumPartitions, [NodeEntry]}
%  NumPartitions = integer()
%  NodeEntry = {IndexAsInt, Node}
%  IndexAsInt = integer()
%  Node = node().
% It is not recommended that code outside this module make use
% of the structure of a chash.

% @type index() = binary().
% Indices into the ring, used as keys for object location, are binary
% representations of 160-bit integers.

% @type node() = term().
% A Node is the unique identifier for the owner of a given partition.
% An Erlang Pid works well here, but the chash module allows it to
% be any term.

% @doc Create a brand new ring.  The size and seednode are specified;
%      initially all partitions are owned by the seednode.  If NumPartitions
%      is not much larger than the intended eventual number of
%       participating nodes, then performance will suffer.
% @spec fresh(NumPartitions :: integer(), SeedNode :: node()) -> chash()
fresh(NumPartitions, SeedNode) ->
    Inc = ?RINGTOP div NumPartitions,
    {NumPartitions, [{IndexAsInt, SeedNode} ||
           IndexAsInt <- lists:seq(0,(?RINGTOP-1),Inc)]}.

% @doc Find the Node that owns the partition identified by IndexAsInt.
% @spec lookup(IndexAsInt :: integer(), CHash :: chash()) -> node()
lookup(IndexAsInt, CHash) ->
    {_NumPartitions, Nodes} = CHash,
    {IndexAsInt, X} = proplists:lookup(IndexAsInt, Nodes),
    X.

% @doc Return true if named Node owns any partitions in the ring, else false.
% @spec contains_name(Name :: node(), CHash :: chash()) -> bool()
contains_name(Name, CHash) ->
    {_NumPartitions, Nodes} = CHash,
    case [X || {_,X} <- Nodes, X == Name] of
    [] ->
        false;
    _ ->
        true
    end.

% @doc Make the partition beginning at IndexAsInt owned by Name'd node.
% @spec update(IndexAsInt :: integer(), Name :: node(), CHash :: chash())
%                -> chash()
update(IndexAsInt, Name, CHash) ->
    {NumPartitions, Nodes} = CHash,
    NewNodes = lists:keyreplace(IndexAsInt, 1, Nodes, {IndexAsInt, Name}),
    {NumPartitions, NewNodes}.

% @doc Given an object key, return all NodeEntries in order starting at Index.
% @spec successors(Index :: index(), CHash :: chash()) -> [NodeEntry]
successors(Index, CHash) ->
    {NumPartitions, _Nodes} = CHash,
    successors(Index, CHash, NumPartitions).
% @doc Given an object key, return the next N NodeEntries in order
%      starting at Index.
% @spec successors(Index :: index(), CHash :: chash(), N :: integer())
%                                                             -> [NodeEntry]
successors(Index, CHash, N) ->
    Num = max_n(N, CHash),
    {Res, _} = lists:split(Num, ordered_from(Index, CHash)),
    Res.

% @doc Given an object key, return all NodeEntries in reverse order
%      starting at Index.
% @spec predecessors(Index :: index(), CHash :: chash()) -> [NodeEntry]
predecessors(Index, CHash) ->
    predecessors(Index, CHash, length(CHash)).
% @doc Given an object key, return the next N NodeEntries in reverse order
%      starting at Index.
% @spec predecessors(Index :: index(), CHash :: chash(), N :: integer())
%                                                             -> [NodeEntry]
predecessors(Index, CHash, N) ->
    Num = max_n(N, CHash),
    {Res, _} = lists:split(Num, lists:reverse(ordered_from(Index,CHash))),
    Res.

% @doc Return either N or the number of partitions in the ring, whichever
%      is greater.
% @spec max_n(N :: integer(), CHash :: chash()) -> integer()
max_n(N, {NumPartitions, _Nodes}) ->
    if
    N > NumPartitions ->
        NumPartitions;
    true ->
        N
    end.    

% @doc Given an object key, return all NodeEntries in order starting at Index.
% @spec ordered_from(Index :: index(), CHash :: chash()) -> [NodeEntry]
ordered_from(Index, {NumPartitions, Nodes}) ->
    <<IndexAsInt:160/integer>> = Index,
    Inc = ?RINGTOP div NumPartitions,
    {A, B} = lists:split((IndexAsInt div Inc)+1, Nodes),
    B ++ A.

% @doc Given any term used to name an object, produce that object's key
%      into the ring.  Two names with the same SHA-1 hash value are
%      considered the same name.
% @spec key_of(ObjectName :: term()) -> index()
key_of(ObjectName) ->    
    crypto:sha(term_to_binary(ObjectName)).

% @doc Return all Nodes that own any partitions in the ring.
% @spec members(CHash :: chash()) -> [Node]
members(CHash) ->
    {_NumPartitions, Nodes} = CHash,
    lists:usort([X || {_Idx,X} <- Nodes]).

% @doc Return the entire set of NodeEntries in the ring.
% @spec nodes(CHash :: chash()) -> [NodeEntry]
nodes(CHash) ->
    {_NumPartitions, Nodes} = CHash,
    Nodes.

% @doc Return a randomized merge of two rings based on a vclock.
%      If multiple nodes are actively claiming nodes in the same
%      time period, churn will occur.  Be prepared to live with it.
% @spec merge_rings(CHashA :: chash(), CHashB :: chash()) -> chash()
merge_rings(CHashA,CHashB) ->
    {NumPartitions, NodesA} = CHashA,
    {NumPartitions, NodesB} = CHashB,
    {NumPartitions, [{I,randomnode(A,B)} || 
           {{I,A},{I,B}} <- lists:zip(NodesA,NodesB)]}.

% @spec randomnode(NodeA :: node(), NodeB :: node()) -> node()
randomnode(NodeA,NodeA) -> NodeA;
randomnode(NodeA,NodeB) -> lists:nth(crypto:rand_uniform(1,3),[NodeA,NodeB]).

% @doc Return the number of partitions in the ring.
% @spec size(CHash :: chash()) -> integer()
size(CHash) ->
    {NumPartitions,_Nodes} = CHash,
    NumPartitions.


update_test() ->
    Node = 'old@host', NewNode = 'new@host',
    
    % Create a fresh ring...
    CHash = chash:fresh(5, Node),
    GetNthIndex = fun(N, {_, Nodes}) -> {Index, _} = lists:nth(N, Nodes), Index end,
    
    % Test update...
    FirstIndex = GetNthIndex(1, CHash),
    ThirdIndex = GetNthIndex(3, CHash),
    {5, [{_, NewNode}, {_, Node}, {_, Node}, {_, Node}, {_, Node}, {_, Node}]} = update(FirstIndex, NewNode, CHash),
    {5, [{_, Node}, {_, Node}, {_, NewNode}, {_, Node}, {_, Node}, {_, Node}]} = update(ThirdIndex, NewNode, CHash).
