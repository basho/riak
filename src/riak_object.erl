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

%% @doc The container for data stored in Riak.
%%      
%%      
-module(riak_object).
-include_lib("eunit/include/eunit.hrl").

-record(r_content, {
          metadata :: dict(),
          value :: term()
         }).

%% @type riak_object().  Opaque container for Riak objects.
-record(r_object, {
          bucket :: atom(),
          key :: binary(),
          contents :: [#r_content{}],
          vclock :: [vclock:vclock()],
          updatemetadata=dict:store(clean, true, dict:new()) :: dict(),
          updatevalue :: term()
         }).

-define(MAX_KEY_SIZE, 65536).

%% @type key()=binary().
%% @type bucket()=atom().
%% @type value()=term().

-export([new/3, ancestors/1, reconcile/2, increment_vclock/2, equal/2]).
-export([key/1, get_metadata/1, get_metadatas/1, get_values/1, get_value/1]).
-export([vclock/1, update_value/2, update_metadata/2, bucket/1, value_count/1]).
-export([get_update_metadata/1, get_update_value/1, get_contents/1]).
-export([merge/2, apply_updates/1, syntactic_merge/3]).
-export([set_contents/2, set_vclock/2]). %% INTERNAL, only for riak_*

%% @spec new(Bucket::bucket(), Key::key(), Value::value()) -> riak_object()
%% @doc Constructor for new riak objects.
new(B, K, V) when is_atom(B), is_binary(K) ->
    case size(K) > ?MAX_KEY_SIZE of
        true ->
            throw({error,key_too_large});
        false ->
            Contents = [#r_content{metadata=dict:new(), value=V}],
            #r_object{bucket=B,key=K,contents=Contents,vclock=vclock:fresh()}
    end.

%% @spec equal(riak_object(), riak_object()) -> true | false
%% @doc Deep (expensive) comparison of Riak objects.
equal(Obj1,Obj2) ->
    case Obj1#r_object.bucket =:= Obj2#r_object.bucket of
        false -> false;
        true ->
            case Obj1#r_object.key =:= Obj2#r_object.key of
                false -> false;
                true -> equal1(Obj1,Obj2)
            end
    end.
equal1(Obj1,Obj2) ->
    case vclock:equal(vclock(Obj1),vclock(Obj2)) of
        false -> false;
        true -> equal2(Obj1,Obj2)
    end.
equal2(Obj1,Obj2) ->
    UM1 = lists:sort(dict:to_list(Obj1#r_object.updatemetadata)),
    UM2 = lists:sort(dict:to_list(Obj2#r_object.updatemetadata)),
    case UM1 =:= UM2 of
        false -> false;
        true ->
            case Obj1#r_object.updatevalue =:= Obj2#r_object.updatevalue of
                false -> false;
                true -> 
                    Cont1 = lists:sort(Obj1#r_object.contents),
                    Cont2 = lists:sort(Obj2#r_object.contents),
                    equal_contents(Cont1,Cont2)
            end
    end.
equal_contents([],[]) -> true;
equal_contents(_,[]) -> false;
equal_contents([],_) -> false;
equal_contents([C1|R1],[C2|R2]) ->
    MD1 = lists:sort(dict:to_list(C1#r_content.metadata)),
    MD2 = lists:sort(dict:to_list(C2#r_content.metadata)),
    case MD1 =:= MD2 of
        false -> false;
        true ->
            case C1#r_content.value =:= C2#r_content.value of
                false -> false;
                true -> equal_contents(R1,R2)
            end
    end.

%% @spec reconcile([riak_object()], boolean()) -> riak_object()
%% @doc  Reconcile a list of riak objects.  If AllowMultiple is true,
%%       the riak_object returned may contain multiple values if Objects
%%       contains sibling versions (objects that could not be syntactically
%%       merged).   If AllowMultiple is false, the riak_object returned will
%%       contain the value of the most-recently-updated object, as per the
%%       X-Riak-Last-Modified header.
reconcile(Objects, AllowMultiple) ->
    RObjs = reconcile(Objects),
    AllContents = lists:flatten([O#r_object.contents || O <- RObjs]),
    Contents = case AllowMultiple of
        false ->
            [hd(lists:sort(fun compare_content_dates/2, AllContents))];
	true ->
	    AllContents
    end,
    VClock = vclock:merge([O#r_object.vclock || O <- RObjs]),
    HdObj = hd(RObjs),
    HdObj#r_object{contents=Contents,vclock=VClock,
                   updatemetadata=dict:store(clean, true, dict:new()),
                   updatevalue=undefined}.

%% @spec ancestors([riak_object()]) -> [riak_object()]
%% @doc  Given a list of riak_object()s, return the objects that are pure 
%%       ancestors of other objects in the list, if any.  The changes in the
%%       objects returned by this function are guaranteed to be reflected in
%%       the other objects in Objects, and can safely be discarded from the list
%%       without losing data.
ancestors(Objects) ->
    ToRemove = [[O2 || O2 <- Objects,
     vclock:descends(O1#r_object.vclock,O2#r_object.vclock),
     (vclock:descends(O2#r_object.vclock,O1#r_object.vclock) == false)]
		|| O1 <- Objects],
    lists:flatten(ToRemove).

%% @spec reconcile([riak_object()]) -> [riak_object()]
reconcile(Objects) ->
    All = sets:from_list(Objects),
    Del = sets:from_list(ancestors(Objects)),
    remove_duplicate_objects(sets:to_list(sets:subtract(All, Del))).

remove_duplicate_objects(Os) -> rem_dup_objs(Os,[]).
rem_dup_objs([],Acc) -> Acc;
rem_dup_objs([O|Rest],Acc) ->
    EqO = [AO || AO <- Acc, riak_object:equal(AO,O) =:= true],
    case EqO of
        [] -> rem_dup_objs(Rest,[O|Acc]);
        _ -> rem_dup_objs(Rest,Acc)
    end.

compare_content_dates(C1,C2) ->
    % true if C1 was modifed later than C2
    riak_util:compare_dates(
      dict:fetch(<<"X-Riak-Last-Modified">>, C1#r_content.metadata),
      dict:fetch(<<"X-Riak-Last-Modified">>, C2#r_content.metadata)).

%% @spec merge(riak_object(), riak_object()) -> riak_object()
%% @doc  Merge the contents and vclocks of OldObject and NewObject. 
%%       Note:  This function calls apply_updates on NewObject.
merge(OldObject, NewObject) ->
    NewObj1 = apply_updates(NewObject),
    OldObject#r_object{contents= NewObj1#r_object.contents ++
                                 OldObject#r_object.contents,
		     vclock=vclock:merge([OldObject#r_object.vclock,
					  NewObj1#r_object.vclock]),
		     updatemetadata=dict:store(clean, true, dict:new()),
		     updatevalue=undefined}.

%% @spec apply_updates(riak_object()) -> riak_object()
%% @doc  Promote pending updates (made with the update_value() and 
%%       update_metadata() calls) to this riak_object.
apply_updates(Object=#r_object{}) ->
    VL = case Object#r_object.updatevalue of
	     undefined ->
		 [C#r_content.value || C <- Object#r_object.contents];
	     _ ->
		 [Object#r_object.updatevalue]
	 end,
    MD = case dict:find(clean, Object#r_object.updatemetadata) of
             {ok,_} ->
                 MDs = [C#r_content.metadata || C <- Object#r_object.contents],
                 case Object#r_object.updatevalue of
                     undefined -> MDs;
                     _ -> [hd(MDs)]
                 end;
             error ->
		 [dict:erase(clean,Object#r_object.updatemetadata) || _X <- VL]
	 end,
    Contents = [#r_content{metadata=M,value=V} || {M,V} <- lists:zip(MD, VL)],
    Object#r_object{contents=Contents,
                 updatemetadata=dict:store(clean, true, dict:new()),
                 updatevalue=undefined}.

%% @spec bucket(riak_object()) -> bucket()
%% @doc Return the containing bucket for this riak_object.
bucket(#r_object{bucket=Bucket}) -> Bucket.

%% @spec key(riak_object()) -> key()
%% @doc  Return the key for this riak_object.
key(#r_object{key=Key}) -> Key.

%% @spec vclock(riak_object()) -> vclock:vclock()
%% @doc  Return the vector clock for this riak_object.
vclock(#r_object{vclock=VClock}) -> VClock.

%% @spec value_count(riak_object()) -> non_neg_integer()
%% @doc  Return the number of values (siblings) of this riak_object.
value_count(#r_object{contents=Contents}) -> length(Contents).

%% @spec get_contents(riak_object()) -> [{dict(), value()}]
%% @doc  Return the contents (a list of {metadata, value} tuples) for 
%%       this riak_object.
get_contents(#r_object{contents=Contents}) ->
    [{Content#r_content.metadata, Content#r_content.value} ||
        Content <- Contents].

%% @spec get_metadata(riak_object()) -> dict()
%% @doc  Assert that this riak_object has no siblings and return its associated
%%       metadata.  This function will fail with a badmatch error if the 
%%       object has siblings (value_count() > 1).
get_metadata(O=#r_object{}) ->
    % this blows up intentionally (badmatch) if more than one content value!
    [{Metadata,_V}] = get_contents(O), 
    Metadata.

%% @spec get_metadatas(riak_object()) -> [dict()]
%% @doc  Return a list of the metadata values for this riak_object.  
get_metadatas(#r_object{contents=Contents}) ->
    [Content#r_content.metadata || Content <- Contents].

%% @spec get_values(riak_object()) -> [value()]
%% @doc  Return a list of object values for this riak_object.
get_values(#r_object{contents=C}) -> [Content#r_content.value || Content <- C].

%% @spec get_value(riak_object()) -> value()
%% @doc  Assert that this riak_object has no siblings and return its associated
%%       value.  This function will fail with a badmatch error if the object
%%       has siblings (value_count() > 1).
get_value(Object=#r_object{}) ->
    % this blows up intentionally (badmatch) if more than one content value!
    [{_M,Value}] = get_contents(Object),
    Value.

%% @spec update_metadata(riak_object(), dict()) -> riak_object()
%% @doc  Set the updated metadata of an object to M.
update_metadata(Object=#r_object{}, M) ->
    Object#r_object{updatemetadata=dict:erase(clean, M)}.

%% @spec update_value(riak_object(), value()) -> riak_object()
%% @doc  Set the updated value of an object to V
update_value(Object=#r_object{}, V) -> Object#r_object{updatevalue=V}.

%% @spec get_update_metadata(riak_object()) -> dict()
%% @doc  Return the updated metadata of this riak_object.
get_update_metadata(#r_object{updatemetadata=UM}) -> UM.

%% @spec get_update_value(riak_object()) -> value()
%% @doc  Return the updated value of this riak_object.
get_update_value(#r_object{updatevalue=UV}) -> UV.

%% @spec set_vclock(riak_object(), vclock:vclock()) -> riak_object()
%% @doc  INTERNAL USE ONLY.  Set the vclock of riak_object O to V.
set_vclock(Object=#r_object{}, VClock) -> Object#r_object{vclock=VClock}.

%% @spec increment_vclock(riak_object(), term()) -> riak_object()
%% @doc  Increment the entry for ClientId in O's vclock.
increment_vclock(Object=#r_object{}, ClientId) ->
    Object#r_object{vclock=vclock:increment(ClientId, Object#r_object.vclock)}.

%% @spec set_contents(riak_object(), [{dict(), value()}]) -> riak_object()
%% @doc  INTERNAL USE ONLY.  Set the contents of riak_object to the 
%%       {Metadata, Value} pairs in MVs. Normal clients should use the
%%       set_update_[value|metadata]() + apply_updates() method for changing
%%       object contents.
set_contents(Object=#r_object{}, MVs) when is_list(MVs) ->
    Object#r_object{contents=[#r_content{metadata=M,value=V} || {M, V} <- MVs]}.

is_updated(_Object=#r_object{updatemetadata=M,updatevalue=V}) ->
    case dict:find(clean, M) of
        error -> true;
        {ok,_} ->
            case V of
                undefined -> false;
                _ -> true
            end
    end.
            
syntactic_merge(CurrentObject, NewObject, FromClientId) ->
    case ancestors([CurrentObject, NewObject]) of
        [OlderObject] ->
            WinObject = case vclock(OlderObject) =:= vclock(CurrentObject) of
                true -> NewObject;
                false -> CurrentObject
            end,
            case is_updated(WinObject) of
                true -> increment_vclock(apply_updates(WinObject),FromClientId);
                false -> WinObject
            end;
	[] -> 
            case riak_object:equal(CurrentObject, NewObject) of
                true ->
                    NewObject;
                false ->
                    increment_vclock(
                      merge(CurrentObject, NewObject), FromClientId)
            end
    end.

object_test() ->
    B = buckets_are_atoms,
    K = <<"keys are binaries">>,
    V = <<"values are anything">>,
    O = riak_object:new(B,K,V),
    B = riak_object:bucket(O),
    K = riak_object:key(O),
    V = riak_object:get_value(O),
    1 = riak_object:value_count(O),
    1 = length(riak_object:get_values(O)),
    1 = length(riak_object:get_metadatas(O)),
    O.

update_test() ->
    O = object_test(),
    V2 = <<"testvalue2">>,
    O1 = riak_object:update_value(O, V2),
    O2 = riak_object:apply_updates(O1),
    V2 = riak_object:get_value(O2),
    {O,O2}.

ancestor_test() ->
    {O,O2} = update_test(),
    O3 = riak_object:increment_vclock(O2,self()),
    [O] = riak_object:ancestors([O,O3]),
    {O,O3}.

reconcile_test() ->
    {O,O3} = ancestor_test(),
    O3 = riak_object:reconcile([O,O3],true),
    O3 = riak_object:reconcile([O,O3],false),
    {O,O3}.

merge1_test() ->
    {O,O3} = reconcile_test(),
    O3 = riak_object:syntactic_merge(O,O3,node_does_not_matter_here),
    {O,O3}.    

merge2_test() ->
    O1 = riak_object:increment_vclock(object_test(), node1),
    O2 = riak_object:increment_vclock(object_test(), node2),
    O3 = riak_object:syntactic_merge(O1, O2, other_node),
    [other_node, node1, node2] = [N || {N,_} <- riak_object:vclock(O3)],
    2 = riak_object:value_count(O3).

equality1_test() ->
    MD0 = dict:new(),
    MD = dict:store("X-Riak-Test", "value", MD0),
    O1 = riak_object:new(test, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"a">>, "value"),
    O3 = riak_object:increment_vclock(O1, self()),
    O4 = riak_object:increment_vclock(O2, self()),
    O5 = riak_object:update_metadata(O3, MD),
    O6 = riak_object:update_metadata(O4, MD),
    true = riak_object:equal(O5, O6).

inequality_value_test() ->
    O1 = riak_object:new(test, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"a">>, "value1"),
    false = riak_object:equal(O1, O2).    

inequality_multivalue_test() ->
    O1 = riak_object:new(test, <<"a">>, "value"),
    [C] = riak_object:get_contents(O1),
    O1p = riak_object:set_contents(O1, [C,C]),
    false = riak_object:equal(O1, O1p),
    false = riak_object:equal(O1p, O1).

inequality_metadata_test() ->
    O1 = riak_object:new(test, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"a">>, "value"),
    O1p = riak_object:apply_updates(
            riak_object:update_metadata(
              O1, dict:store(<<"X-Riak-Test">>, "value",
                             riak_object:get_metadata(O1)))),
    false = riak_object:equal(O1p, O2).

inequality_key_test() ->
    O1 = riak_object:new(test, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"b">>, "value"),
    false = riak_object:equal(O1, O2).    

inequality_vclock_test() ->
    O1 = riak_object:new(test, <<"a">>, "value"),
    false = riak_object:equal(O1, riak_object:increment_vclock(O1, foo)).

inequality_bucket_test() ->
    O1 = riak_object:new(test1, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"a">>, "value"),
    false = riak_object:equal(O1, O2). 

inequality_updatecontents_test() ->
    MD1 = dict:new(),
    MD2 = dict:store("X-Riak-Test", "value", MD1),
    MD3 = dict:store("X-Riak-Test", "value1", MD1),
    O1 = riak_object:new(test, <<"a">>, "value"),
    O2 = riak_object:new(test, <<"a">>, "value"),    
    O3 = riak_object:update_metadata(O1, MD2),
    false = riak_object:equal(O3, riak_object:update_metadata(O2, MD3)),
    O5 = riak_object:update_value(O1, "value1"),
    false = riak_object:equal(O5, riak_object:update_value(O2, "value2")).

largekey_test() ->
    TooLargeKey = <<0:(65537*8)>>,
    try
        riak_object:new(a, TooLargeKey, <<>>)
    catch throw:{error, key_too_large} ->
            ok
    end.
            
date_reconcile_test() ->
    {O,O3} = reconcile_test(),
    D = calendar:datetime_to_gregorian_seconds(
          httpd_util:convert_request_date(
            httpd_util:rfc1123_date())),
    O2 = apply_updates(
           riak_object:update_metadata(
             increment_vclock(O, date),
             dict:store(
               <<"X-Riak-Last-Modified">>,
               httpd_util:rfc1123_date(
                 calendar:gregorian_seconds_to_datetime(D)),
               get_metadata(O)))),
    O4 = apply_updates(
           riak_object:update_metadata(
             O3,
             dict:store(
               <<"X-Riak-Last-Modified">>,
               httpd_util:rfc1123_date(
                 calendar:gregorian_seconds_to_datetime(D+1)),
               get_metadata(O3)))),
    O5 = riak_object:reconcile([O2,O4], false),
    false = riak_object:equal(O2, O5),
    false = riak_object:equal(O4, O5).

get_update_value_test() ->
    O = riak_object:new(test, <<"test">>, old_val),
    NewVal = new_val,
    ?assertEqual(NewVal,
                 riak_object:get_update_value(
                   riak_object:update_value(O, NewVal))).

get_update_metadata_test() ->
    O = riak_object:new(test, <<"test">>, val),
    OldMD = riak_object:get_metadata(O),
    NewMD = dict:store(<<"X-Riak-Test">>, "testval", OldMD),
    ?assertNot(NewMD =:= OldMD),
    ?assertEqual(NewMD,
                 riak_object:get_update_metadata(
                   riak_object:update_metadata(O, NewMD))).
