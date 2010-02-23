%% @copyright 2007-2008 Basho Technologies

%% @reference Ralph C. Merkle, A Digital Signature Based on a Conventional Encryption Function, A Conference on the Theory and Applications of Cryptographic Techniques on Advances in Cryptology, p.369-378, August 16-20, 1987 

% @author Justin Sheehy <justin@basho.com>

% @doc An implementation of Merkle Trees for anti-entropy.
%
% Intended use is for synchronizing two key/value stores with
% similar but potentially-divergent content.
%
% Typical usage is when a pair (or more) of nodes or systems have
% views of a set of key/value objects which can change independently.
% Whenever a new object is created or an existing one is modified
% (there is no difference from the merkle point of view) the node
% seeing the change performs an insert/2 to record the change.  At any
% time, one node can send a representation of its tree to another
% node.  The receiving node can diff/2 the trees to see which objects
% differ on the two systems.  From this information, a system knows
% exactly which objects to send or request in order to converge toward
% a common view of the world.  Of course, if the objects contain
% versioning information it will be much easier to resolve which
% node's view for any given object is newer.
%
% See the code of merkle_test/0 for trivial example usage.
%
% Application usage note: the 'crypto' OTP application must be started
% before any of this module's functions will work.
%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%
%% http://www.apache.org/licenses/LICENSE-2.0
%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(merkerl).
-export([insert/2,delete/2,build_tree/1,diff/2,allkeys/1]).

-include_lib("eunit/include/eunit.hrl").

% TODO: fix doc, userdata is the ONLY user-exposed key
-record(merk, {nodetype,           % atom: expected values are 'leaf' or 'inner'
               key=undefined,      % if nodetype=leaf, then this is binary/160
                                   % (keys are 160b binaries)
               userdata=undefined, % (if user specified a non-binary key)
	       hashval,            % hash of value if leaf, of children if inner
	       offset=undefined,   % if inner, then offset to reach here
	       children=undefined  % if nodetype=inner, then this is orddict
	       }).

% TODO in doc: note that these are an internal-only form
-record(merkitem, {userdata=undefined, % for non-binary "keys"
                   hkey,               % SHA-1 of userdata
                   hval                % SHA-1 of value (user-supplied)
                  }).

% @type tree() = treeleaf() | treeinner() | undefined.
% The tree() type here is used as the internal representation of
% a Merkle tree.  It can be used locally with insert/2 or pickled
% via term_to_binary and inverse for use remotely in diff/2.

% @type treeleaf() = term().
% Not externally useful, this is one of two record types making up tree().

% @type treeinner() = term().
% Not externally useful, this is one of two record types making up tree().

% (NEED TO EDOC THE RECORD TYPES)
% The merkitem records...
% These make up the "real" leaves in the Merkle tree.
%
% This is the input that most clients of the library will need to provide.

% @type key() = binary().
% This is the key, or "name" for an object tracked by a Merkle tree.
% It should remain constant through changes to the object it references.
% It is expected to be a 160b binary, as produced by
% crypto:sha/1 -- if the natural names of objects are not such values,
% then simply crypto:sha(term_to_binary(the-name>).

% @type hash() = binary().
% This is a hash representing a unique content value for an object
% tracked by a Merkle tree.
% It should change if the object it references changes in value.
% It is expected to be a 160b binary, as produced by
% crypto:sha/1 -- crypto:sha(term_to_binary(value)) is the canonical
% way to produce a hash().

% %spec build_tree([kh()]) -> tree()
% @doc Build a Merkle tree from a list of KH's of objects.
build_tree([]) ->
    undefined;
build_tree([{K,H}|KHL]) ->
    insert({K,H},build_tree(KHL)).

delete(Key, Tree) when is_record(Tree, merk) ->
    mi_delete({0, #merkitem{userdata=Key,hkey=sha(Key),hval=undefined}}, Tree).
mi_delete({Offset, MI}, Tree) ->
    HKey = MI#merkitem.hkey,
    case Tree#merk.nodetype of
	leaf ->
	    case Tree#merk.key of
		HKey ->
		    undefined;
		_ ->
		    Tree
	    end;
	inner ->
            Kids = Tree#merk.children,
            OKey = offset_key(Offset,HKey),
            NewKids = case orddict:is_key(OKey,Kids) of
                          false ->
                              Kids;
                          true ->
                              SubTree = orddict:fetch(OKey,Kids),
                              orddict:store(OKey,
                                      mi_delete({Offset+8,MI},SubTree),Kids)
                      end,
            mkinner(Offset,NewKids)
    end.
    
% TODO: fix @spec to be merkitems instead of kh's
% spec insert(KH :: kh(),T :: tree()) -> tree()
% @doc Insert the KH for a new or changed object into T.
%
% This is used much like a typical tree-insert function.
% To create a new tree, this can be called with T set to the atom 'undefined'.
insert({Userdata, Hashval}, T) ->
    mi_insert(#merkitem{userdata=Userdata,hkey=sha(Userdata),hval=Hashval}, T).
mi_insert(MI,T) when is_record(MI, merkitem) ->
    mi_insert({0,MI},T);
mi_insert({_Offset,MI},undefined) ->
    mkleaf(MI);
mi_insert({160,MI},_Tree) ->
    % we're all the way deep!  replace.
    mkleaf(MI);
mi_insert({Offset,MI},Tree) ->
    Key = MI#merkitem.hkey,
    case Tree#merk.nodetype of
	leaf ->
	    case Tree#merk.key of
		Key -> % replacing!
		    mkleaf(MI);
		_ -> % turning a leaf into an inner
		    K0 = orddict:new(),
		    K1 = orddict:store(offset_key(Offset,Key),
				       mkleaf(MI),K0),
		    TKey = Tree#merk.key,
		    Kids = orddict:store(offset_key(Offset,TKey),Tree,K1),
		    mkinner(Offset,Kids)
	    end;
	inner ->
	    mi_insert1({Offset,MI},Tree)
    end.
mi_insert1({Offset,MI},Tree) ->
    Kids = Tree#merk.children,
    OKey = offset_key(Offset,MI#merkitem.hkey),
    NewKids = case orddict:is_key(OKey,Kids) of
		  false ->
		      orddict:store(OKey,mkleaf(MI),Kids);
		  true ->
		      SubTree = orddict:fetch(OKey,Kids),
		      orddict:store(OKey,
				   mi_insert({Offset+8,MI},SubTree),Kids)
	      end,
    mkinner(Offset,NewKids).

mkleaf(MI) ->
    #merk{nodetype=leaf,
          key=MI#merkitem.hkey,
          userdata=MI#merkitem.userdata,
          hashval=MI#merkitem.hval}.

mkinner(Offset,Kids) ->
    #merk{nodetype=inner,hashval=sha(Kids),offset=Offset,
          children=[{K,V} || {K,V} <- Kids, V =/= undefined]}.

offset_key(Offset,Key) ->
    % offset is a 8b-divisible integer from 0 to 152, inclusive
    % Key is a 160b binary
    <<_L:Offset/integer,RightKey/binary>> = Key,
    <<OKey:8/integer,_R/binary>> = RightKey,
    OKey.

% TODO FIX TO NOTE THAT WE RETURN USERDATA INSTEAD
% @spec diff(tree(), tree()) -> [key()]
% @doc Find the keys of objects which differ between the two trees.
%
% For this purpose, "differ" means that an object either exists in
% only one of the two trees or it exists in both but with different
% hash() values.
%
% No information about the differing objects is provided except the keys.
% (Objects with vector-clock versioning are helpful here)
diff(undefined, X) -> allkeys(X);
diff(X, undefined) -> allkeys(X);
diff(TreeA,TreeB) when is_record(TreeA,merk),is_record(TreeB,merk) ->
    % return the list of 'userdata' fields from inner nodes that differ
    lists:usort(diff1(TreeA,TreeB)).
diff1(TreeA,TreeB) ->
    % precondition: TreeA and TreeB are both merks at same offset
    case TreeA#merk.hashval == TreeB#merk.hashval of
 	true ->
 	    [];
 	false ->
	    diff2(TreeA,TreeB)
    end.
diff2(TreeA,TreeB) ->
    % precondition: TreeA and TreeB are both merks at same offset
    % precondition: TreeA and TreeB have different hashval
    case TreeA#merk.nodetype == TreeB#merk.nodetype andalso
	TreeA#merk.nodetype == leaf of
	true ->
	    [TreeA#merk.userdata,TreeB#merk.userdata];
	false ->
	    diff3(TreeA,TreeB)
    end.
diff3(TreeA,TreeB) ->
    % precondition: TreeA and TreeB are both merks at same offset
    % precondition: TreeA and TreeB have different hashval
    % precondition: at least one of TreeA and TreeB is not a leaf
    case TreeA#merk.nodetype == leaf of
	true ->
	    allbutmaybe(TreeB,TreeA);
	false ->
	    case TreeB#merk.nodetype == leaf of
		true ->
		    allbutmaybe(TreeA,TreeB);
		false ->
		    diff4(TreeA,TreeB)
	    end
    end.
diff4(TreeA,TreeB) ->
    % precondition: TreeA and TreeB are both merks at same offset
    % precondition: TreeA and TreeB have different hashval
    % precondition: TreeA and TreeB are both inner nodes
    diff4a(TreeA#merk.children,TreeB#merk.children,0,[]).
diff4a(KidsA,KidsB,Idx,Acc) ->
    % this is the ugly bit.
    case Idx > 255 of
	true ->
	    Acc;
	false ->
	    case KidsA of
		[] ->
		    lists:append(Acc,lists:flatten([allkeys(X) ||
                                                       {_Okey, X} <- KidsB]));
		_ ->
		    case KidsB of
			[] ->
			    lists:append(Acc,lists:append(
					       [allkeys(X) ||
                                                   {_Okey, X} <- KidsA]));
			_ ->
			    diff4b(KidsA,KidsB,Idx,Acc)
		    end
	    end
    end.
diff4b(KidsA,KidsB,Idx,Acc) ->
    % precondition: neither KidsA nor KidsB is empty
    [{OkeyA,NodeA}|RestA] = KidsA,
    [{OkeyB,NodeB}|RestB] = KidsB,
    case OkeyA == Idx of
	true ->
	    case OkeyB == Idx of
		true ->
		    diff4a(RestA,RestB,Idx+1,
			   lists:append(Acc,diff1(
					      NodeA,NodeB)));
		false ->
		    diff4a(RestA,KidsB,Idx+1,
			   lists:append(Acc,allkeys(
					      NodeA)))
	    end;
	false ->
	    case OkeyB == Idx of
		true ->
		    diff4a(KidsA,RestB,Idx+1,
			   lists:append(Acc,allkeys(
					      NodeB)));
		false ->
		    diff4a(KidsA,KidsB,Idx+1,Acc)
	    end
    end.

allkeys(undefined) -> [];
allkeys(Tree) when is_record(Tree, merk) ->
    case Tree#merk.nodetype of
	leaf ->
	    [Tree#merk.userdata];
	_ ->
	    lists:append([allkeys(Kid) || Kid <- getkids(Tree)])
    end.
	    
allbutmaybe(Tree,Leaf) when is_record(Tree, merk),is_record(Leaf,merk) ->
    % return all keys in Tree, maybe the one for Leaf
    % (depending on whether it is present&identical in Tree)
    case contains_node(Tree,Leaf) of
	true ->
	    lists:delete(Leaf#merk.userdata,allkeys(Tree));
	false ->
	    lists:append([Leaf#merk.userdata],allkeys(Tree))
    end.

contains_node(Tree,Node) ->
    case Tree#merk.nodetype of
	leaf ->
	    Tree#merk.hashval == Node#merk.hashval;
	_ ->
	    lists:any(fun(T) -> contains_node(T,Node) end, getkids(Tree))
    end.
	    
getkids(Tree) ->
    [V || {_K,V} <- orddict:to_list(Tree#merk.children)].

sha(X) ->
    crypto:sha(term_to_binary(X)).

% @spec merkle_test() -> bool()
% @doc A test function and example code.
%
% This should be changed into a proper unit test suite.
merkle_test() ->
    case lists:keymember(crypto, 1, application:loaded_applications()) of
        true  -> ok;
        false -> ok = application:start(crypto)
    end,
    A = [{one,"one data"},{two,"two data"},{three,"three data"},
	 {four,"four data"},{five,"five data"}],
    B = [{one,"one data"},{two,"other two"},{three,"three data"},
	 {four,"other four"},{five,"five data"}],
    A2 = build_tree(A),
    B2 = build_tree(B),
    ?assertEqual(lists:usort([two, four]), diff(A2,B2)),
    C = [{one,"one data"}],
    C2 = build_tree(C),
    ?assertEqual(lists:usort([two, three, four, five]), diff(A2,C2)),
    D = insert({four, sha("changed!")}, A2),
    ?assertEqual([four], diff(A2,D)),
    E = insert({five, sha("changed more!")}, D),
    ?assertEqual([five], diff(D,E)),
    ?assertEqual(lists:usort([four, five]), diff(A2,E)),
    F = delete(five,D),
    G = delete(five,E),
    ?assertEqual([], diff(F,G)),
    H = delete(two,A2),
    ?assertEqual([two], diff(A2,H)),
    ?assertEqual([one], diff(C2,undefined)),
    STree1 = build_tree([{"hello", "hi"},{"and", "what"}]),
    STree2 = build_tree([{"hello", "hi"},{"goodbye", "bye"}]),
    ?assertEqual(lists:usort(["and", "goodbye"]), diff(STree1, STree2)).


