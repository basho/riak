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

%% @doc jiak_object is a set of utilities for dealing with objects
%%      in the format that {@link jiak} uses.
%%
%%      Jiak's objects take the pre-JSON-encoding form expected
%%      by mochijson2.  Thus, a simple object looks something like:
%%```
%%      {struct,
%%       [{<<"bucket">>, foo},
%%        {<<"key">>, <<"123">>},
%%        {<<"object">>,
%%         {struct,
%%          [{<<"barField">>, <<"barValue">>},
%%           {<<"bazField">>, 42},
%%          ]}},
%%        {<<"links">>,
%%         [[quuxLink, <<"quuxKey">>, <<"quuxTag">>],
%%          [abcLink, <<"abcKey">>, <<"abcTag">>]
%%         ]},
%%        {<<"vclock">>, <<"opaque-riak-vclock">>},
%%        {<<"lastmod">>, <<"Mon, 03 Aug 2009 18:49:42 GMT">>}
%%       ]}
%%'''
%%       This format can be great for Erlang pattern matching,
%%       but can get in the way in some circumstances.  To aid use,
%%       this module provides functions like new/2,3,4 for creating
%%       new Jiak objects, bucket/1 and key/1 for top-level details,
%%       getp/2,3 and setp/3 for getting and modifying fields in the
%%       "object" structure, and links/1,2,3 for searching the
%%       links field.
%%
%%       For example, if the jiak_object above were bound to O, then
%%```
%%       foo = jiak_object:bucket(O).
%%       <<"barValue">> = jiak_object:getp(O, <<"barField">>).
%%       3 = jiak_object:getp(
%%             jiak_object:setp(O, <<"bazField">>, 3)).
%%       [[linkBucket, <<"linkKey">>, <<"linkTag">>]] =
%%         jiak_object:links(O, linkBucket).
%%'''
%%       are all true.
-module(jiak_object).

-export([new/2, new/3, new/4,
         from_riak_object/1, to_riak_object/1]).
-export([bucket/1, key/1, vclock/1, lastmod/1, vtag/1,
         object/1, set_object/2]).
-export([getp/2, getp/3, setp/3, removep/2, props/1]).
-export([setf/3]).
-export([links/1, links/2, links/3,
         set_links/2,
         add_link/2, add_link/4,
         remove_link/2, remove_link/4]).
-export([mapreduce_linkfun/3,
         mapreduce_identity/3,
         mapreduce_wrap_fun/3]).
-export([diff/2, undefined/0]).

-include_lib("eunit/include/eunit.hrl").

%% @spec new(riak_object:bucket(), riak_object:key()) -> jiak_object()
%% @doc produces an empty jiak object
%% @equiv new(B, K, {struct, []}, [])
new(B, K) -> new(B, K, {struct, []}, []).

%% @spec new(riak_object:bucket(),riak_object:key(),mochijson2())->jiak_object()
%% @doc produces a new jiak object with no links
%% @equiv new(B, K, O, [])
new(B, K, O) -> new(B, K, O, []).

%% @spec new(riak_object:bucket(), riak_object:key(), mochijson2(), 
%%           [link()]) -> jiak_object()
%% @doc produces empty jiak object, a mochijson2 structure that
%%      would encode to the JSON structure:
%%       {
%%        "bucket":B,
%%        "key"   :K,
%%        "object":O,
%%        "links" :L
%%       }
%%     Important: O should be a valid mochijson2 object
new(B, K, O={struct, _}, L) when is_atom(B), is_binary(K), is_list(L) ->
    {struct, [{<<"bucket">>, B},
              {<<"key">>, K},
              {<<"object">>, O},
              {<<"links">>, L}
             ]}.

%% @spec from_riak_object(riak_object:riak_object()) -> jiak_object()
%% @doc produce a jiak object from a riak object
%%      output will have form described in new/3, with the
%%      addition of "vclock" and "lastmod" fields
from_riak_object(RiakObject) ->
    {MD, {Object, Links}} =
        case riak_object:get_contents(RiakObject) of
            [{M,V}] -> {M,V};
            Multiple ->
                Bucket = riak_object:bucket(RiakObject),
                BucketMod = jiak_util:jiak_module_for_bucket(Bucket),
                BucketMod:merge_siblings(Multiple)
        end,
    {struct, J0} = new(riak_object:bucket(RiakObject),
                       riak_object:key(RiakObject),
                       Object,
                       Links),
    {struct, [{<<"vclock">>,vclock_to_headers(riak_object:vclock(RiakObject))},
              {<<"lastmod">>,
               list_to_binary(dict:fetch(<<"X-Riak-Last-Modified">>, MD))},
              {<<"vtag">>,
               list_to_binary(dict:fetch(<<"X-Riak-VTag">>, MD))}
              |J0]}.

%% @spec to_riak_object(jiak_object()) -> riak_object:riak_object()
%% @doc produce a riak object from a jiak object
to_riak_object(JiakObject) ->
    R0 = riak_object:new(bucket(JiakObject),
                         key(JiakObject),
                         {object(JiakObject), links(JiakObject)}),
    case vclock(JiakObject) of
        undefined -> R0;
        VClock    -> riak_object:set_vclock(R0, headers_to_vclock(VClock))
    end.
%% last-modified is handled by put process

bucket(JiakObject)  -> getf(JiakObject, <<"bucket">>).
key(JiakObject)     -> getf(JiakObject, <<"key">>).
vclock(JiakObject)  -> getf(JiakObject, <<"vclock">>).
lastmod(JiakObject) -> getf(JiakObject, <<"lastmod">>).
vtag(JiakObject)    -> getf(JiakObject, <<"vtag">>).
object(JiakObject)  -> getf(JiakObject, <<"object">>).
links(JiakObject)   -> getf(JiakObject, <<"links">>).

getf({struct, JOProps}, Field) ->
    proplists:get_value(Field, JOProps).

%% @spec set_object(jiak_object(), mochijson2:struct()) -> jiak_object()
%% @doc Set the value of the "object" structure.
set_object(JiakObject, NewObj={struct,_}) ->
    setf(JiakObject, <<"object">>, NewObj).

%% @spec set_links(jiak_object(), [link()]) -> jiak_object()
%% @doc Set the value of the "links" array.
set_links(JiakObject, NewLinks) when is_list(NewLinks) ->
    setf(JiakObject, <<"links">>, NewLinks).

setf({struct, JOProps}, Field, Val) when is_binary(Field) ->
    {struct, [{Field, Val}|[{K, V} || {K, V} <- JOProps, K /= Field]]}.
    

%% @spec getp(jiak_object(), binary()) -> term()|undefined
%% @doc Get the value of the field Prop in the "object" structure.
%% @equiv getp(JiakObject, Prop, undefined)
getp(JiakObject, Prop) -> getp(JiakObject, Prop, undefined).

%% @spec getp(jiak_object(), binary(), term()) -> term()
%% @doc Get the value of the field Prop in the "object" structure,
%%      returning Default if Prop is not present.
getp(JiakObject, Prop, Default) ->
    {struct, Obj} = object(JiakObject),
    proplists:get_value(Prop, Obj, Default).

%% @spec setp(jiak_object(), binary(), mochijson2()) -> jiak_object()
%% @doc Set the value of the field Prop in the "object" structure.
setp(JiakObject, Prop, Val) when is_binary(Prop) ->
    {struct, Obj} = object(JiakObject),
    set_object(JiakObject,
               {struct, [{Prop, Val}|[{K,V} || {K,V} <- Obj, K /= Prop]]}).

%% @spec removep(jiak_object(), binary()) -> jiak_object()
%% @doc Remove any setting for the field Prop in the "object"
%%      structure.
removep(JiakObject, Prop) ->
    {struct, Obj} = object(JiakObject),
    set_object(JiakObject, {struct, [{K,V} || {K,V} <- Obj, K /= Prop]}).

%% @spec props(jiak_object()) -> [binary()]
%% @doc Get the list of fields defined in the "object" structure.
props(JiakObject) ->
    {struct, Obj} = object(JiakObject),
    [K || {K, _} <- Obj].

%% @spec links(jiak_object(), riak_object:bucket()) -> [link()]
%% @doc Get all links in this object that point to objects in
%%      the bucket Bucket.  Pass the atom '_' to match any
%%      bucket.
%% @equiv links(jiak_object, Bucket, '_')
links(JiakObject, Bucket) -> links(JiakObject, Bucket, '_').

%% @spec links(jiak_object(), riak_object:bucket(), term()) -> [link()]
%% @doc Get all links in this object tagged Tag that point
%%      to the bucket Bucket.  Pass the atom '_' to match any
%%      bucket or tag.
links(JiakObject, '_', '_') -> links(JiakObject);
links(JiakObject, Bucket, Tag) ->
    Links = links(JiakObject),
    lists:filter(link_match_fun(Bucket, Tag), Links).

%% @private
link_match_fun('_', Tag) ->
    fun([_B, _K, T]) -> Tag == T end;
link_match_fun(Bucket, '_') ->
    fun([B, _K, _T]) -> Bucket == B end;
link_match_fun(Bucket, Tag) ->
    fun([B, _K, T]) -> Bucket == B andalso Tag == T end.

%% @spec add_link(jiak_object(), link()) -> jiak_object()
%% @doc Add NewLink to the object's links.  If the link is already in
%%      JiakObject, then JiakObject is not modified.
add_link(JiakObject, NewLink=[B, K, _T]) when is_atom(B), is_binary(K) ->
    Links = links(JiakObject),
    case lists:member(NewLink, Links) of
        true  -> JiakObject;
        false -> set_links(JiakObject, [NewLink|Links])
    end.

%% @spec add_link(jiak_object(), riak_object:bucket(), binary(), term()) -> jiak_object()
%% @equiv add_link(JiakObject, [Bucket, Key, Tag])
add_link(JiakObject, Bucket, Key, Tag) ->
    add_link(JiakObject, [Bucket, Key, Tag]).

%% @spec remove_link(jiak_object(), link()) -> jiak_object()
%% @doc Remove RemLink from JiakObject.  Has no effect if RemLink is
%%      not in JiakObject.
remove_link(JiakObject, RemLink) ->
    Links = links(JiakObject),
    set_links(JiakObject, [ L || L <- Links, L /= RemLink]).

%% @spec remove_link(jiak_object, riak_object:bucket(), riak_object:key(), term()) 
%%         -> jiak_object()
%% @equiv remove_link(JiakObject, [Bucket, Key, Tag])
remove_link(JiakObject, Bucket, Key, Tag) ->
    remove_link(JiakObject, [Bucket, Key, Tag]).

%% @spec mapreduce_linkfun(riak_object:riak_object(), term(), 
%%                         {link_bucket(), link_tag()})
%%            -> [{{atom(), binary()}, binary()}]
%% @type link_tag() = term()|'_'
%% @type link_bucket() = riak_object:bucket()|'_'
%% @doc This function implements the map phase to which link map/reduce
%%      specs are mapped.  The atom '_' means "match all", so for example,
%%      {foo, '_'} would match all links to bucket 'foo' with any tag, while
%%      {foo, bar} would match only links to bucket 'foo' with tag 'bar'.
%%      The tags of the links will be included as the "KeyData" for the
%%      bucket-key pair returned.
%%      Keys that were inputs to this phase that reference non-existent
%%      objects are ignored (i.e. {error,notfound} => []).
mapreduce_linkfun({error, notfound}, _, _) -> [];
mapreduce_linkfun(RiakObject, _, {Bucket, Tag}) ->
    %% TODO: could just pull links out of riak object directly, if that
    %%       would improve performance
    %%       {_,Links} = riak_object:get_value(RiakObject),
    %%       lists:filter(link_match_fun(Bucket, Tag), Links)
    JiakObject = from_riak_object(RiakObject),
    [{{B, K}, T} || [B, K, T] <- links(JiakObject, Bucket, Tag)].

%% @spec mapreduce_identity(riak_object:riak_object(), term(), term())
%%          -> [jiak_object()|{jiak_object(), term()}]
%% @doc A simple map/reduce phase function for just getting jiak_object()s.
%%      It attempts to be smart, in that if KeyData is the atom
%%      'undefined', then just the jiak_object is put in the result, but
%%      if it's anything else, then {jiak_object(), KeyData} is put in
%%      the result.  Use it like:
%%      {map, {modfun, jiak_object, mapreduce_identity}, none, boolean()}
%%      Keys that were inputs to this phase that reference non-existent
%%      objects are ignored (i.e. {error,notfound} => []).
mapreduce_identity({error, notfound}, _, _) -> [];
mapreduce_identity(RiakObject, undefined, _) ->
    [from_riak_object(RiakObject)];
mapreduce_identity(RiakObject, KeyData, _) ->
    [{from_riak_object(RiakObject), KeyData}].

%% @spec mapreduce_wrap_fun(riak_object:riak_object(), term(),
%%                          {mapreduce_funterm(), term()}) -> [term()]
%% @doc A simple map/reduce phase function for wraping the jiak_object()
%%      creation outside your real map function.  FunTerm will be called
%%      with either the tuple {error, notfound} or a jiak_object(), as
%%      well as the KeyData and Arg, as if it were a first-level map
%%      phase function.  Use it like:
%%      {map, {modfun, jiak_object, mapreduce_wrap_fun},
%%            {{modfun, foo_module, bar_function}, term()}, boolean()}
mapreduce_wrap_fun(RiakObject, KeyData, {FunTerm, Arg}) ->
    JiakObject = case RiakObject of
                     {error, notfound} -> {error, notfound};
                     _ -> from_riak_object(RiakObject)
                 end,
    case FunTerm of
        {modfun, Module, Function} when is_atom(Module), is_atom(Function) ->
            Module:Function(JiakObject, KeyData, Arg);
        {qfun, Fun} when is_function(Fun) ->
            Fun(JiakObject, KeyData, Arg)
    end.

%%
%% Vclock
%%

%% @spec vclock_to_headers(riak:vclock()) -> binary()
%% @doc Convert a vclock to a binary appropriate for storing in
%%      a Jiak object.
vclock_to_headers(VClock) ->
    base64:encode(zlib:zip(term_to_binary(VClock))).

%% @spec headers_to_vclock(binary()) -> riak:vclock()
%% @doc Convert a Jiak object vclock (a binary created by
%%      vclock_to_headers/1) into a vclock appropriate for storing
%%      in a riak_object.
headers_to_vclock(Header) ->
    binary_to_term(zlib:unzip(base64:decode(Header))).

%%
%% Diffing Objects
%%

-define(UNDEFINED, 'JIAK_UNDEFINED').

%% @spec undefined() -> term()
%% @doc Get the sentinal value used by diff/2 to indicate a
%%      non-defined value in an object diff.
undefined() -> ?UNDEFINED.

%% @spec diff(undefined|jiak_object(), jiak_object())
%%         -> jiak_resource:diff()
%% @doc Compute the set of changes needed to transform FromObj
%%      into ToObj.  Pass the atom 'undefined' as FromObj to
%%      diff against an empty object.
diff(undefined, ToObj) ->
    diff({struct, [{<<"object">>, {struct,[]}},
                   {<<"links">>,[]}]},
         ToObj);
diff(FromObj={struct,_}, ToObj={struct, _}) ->
    {Props0, NewProps} = diff_props(FromObj, ToObj),
    {Added, Removed} = diff_links(FromObj, ToObj),
    {[{K, ?UNDEFINED, V} || {K,V} <- NewProps] ++ Props0,
     {Added, Removed}}.

%% @private
diff_props(FromObj, ToObj) ->
    {struct, FromProps} = object(FromObj),
    {struct, ToProps} = object(ToObj),
    diff_props(FromProps, ToProps, []).

diff_props([], NewProps, Acc) ->
    {lists:reverse(Acc), NewProps};
diff_props([{Key, Val}|T], NewObjPL, Acc) ->
    case proplists:get_value(Key, NewObjPL, ?UNDEFINED) of
	?UNDEFINED  ->
	    diff_props(T, NewObjPL, [{Key, Val, ?UNDEFINED}|Acc]);
	NewVal ->
	    NewObjPL1 = proplists:delete(Key, NewObjPL),
	    case Val =:= NewVal of
		true ->
		    diff_props(T, NewObjPL1, Acc);
		false ->
		    diff_props(T, NewObjPL1, [{Key, Val, NewVal}|Acc])
	    end
    end.

%% @private
diff_links(FromObj, ToObj) ->
    FromLinks = sets:from_list(links(FromObj)),
    ToLinks = sets:from_list(links(ToObj)),
    Common = sets:intersection(FromLinks, ToLinks),
    {sets:to_list(sets:subtract(ToLinks, Common)),
     sets:to_list(sets:subtract(FromLinks, Common))}.

%%
%% TEST
%%

roundtrip_vclock_test() ->
    Vclock = vclock:increment(riak_util:mkclientid(node()),
                              vclock:fresh()),
    ?assertEqual(Vclock, headers_to_vclock(vclock_to_headers(Vclock))),
    Vclock2 = vclock:increment(node(), vclock:fresh()),
    ?assertEqual(Vclock2, headers_to_vclock(vclock_to_headers(Vclock2))).

roundtrip_riak_object_test_() ->
    [fun test_to_riak_object/0,
     fun test_from_riak_object/0].

test_to_riak_object() ->
    Object = {struct, [{<<"fake_field">>, <<"fake_value">>}]},
    Links = [[other_fake_bucket,<<"other_fake_key">>,<<"fake_tag">>]],
    J0 = jiak_object:new(fake_bucket, <<"fake_key">>, Object, Links),
    R0 = to_riak_object(J0),
    ?assertEqual(bucket(J0), riak_object:bucket(R0)),
    ?assertEqual(key(J0), riak_object:key(R0)),
    ?assertEqual({Object, Links}, riak_object:get_value(R0)),
    ?assertEqual(vclock:fresh(), riak_object:vclock(R0)),
    VClock = vclock:increment(<<"a">>, vclock:fresh()),
    J1 = setf(J0, <<"vclock">>, vclock_to_headers(VClock)),
    R1 = to_riak_object(J1),
    ?assertEqual(VClock, riak_object:vclock(R1)).

test_from_riak_object() ->
    R0 = to_riak_object(
           setf(jiak_object:new(
                  fake_bucket, <<"fake_key">>,
                  {struct, [{<<"fake_field">>, <<"fake_value">>}]},
                  [[other_fake_bucket,<<"other_fake_key">>,
                    <<"fake_tag">>]]),
                <<"vclock">>,
                vclock_to_headers(vclock:increment(<<"a">>, vclock:fresh())))),
    LM = httpd_util:rfc1123_date(),
    [{M, D}] = riak_object:get_contents(R0),
    R1 = riak_object:set_contents(
           R0, [{dict:store(
                   <<"X-Riak-Last-Modified">>, LM,
                   dict:store(<<"X-Riak-VTag">>, "hello", M)), D}]),
    J1 = from_riak_object(R1),
    ?assertEqual(riak_object:bucket(R1), bucket(J1)),
    ?assertEqual(riak_object:key(R1), key(J1)),
    ?assertEqual(element(1, riak_object:get_value(R1)),
                 getf(J1, <<"object">>)),
    ?assertEqual(element(2, riak_object:get_value(R1)),
                 getf(J1, <<"links">>)),
    ?assertEqual(riak_object:vclock(R1), headers_to_vclock(vclock(J1))),
    ?assertEqual(dict:fetch(<<"X-Riak-Last-Modified">>,
                            riak_object:get_metadata(R1)),
                 binary_to_list(lastmod(J1))),
    ?assertEqual(dict:fetch(<<"X-Riak-VTag">>,
                            riak_object:get_metadata(R1)),
                 binary_to_list(vtag(J1))).

object_props_test() ->
    A = jiak_object:new(fake_bucket, <<"fake_key">>),
    ?assertEqual([], props(A)),

    B = setp(A, <<"foo">>, 42),
    ?assertEqual([<<"foo">>], props(B)),
    ?assertEqual(42, getp(B, <<"foo">>)),

    C = setp(B, <<"foo">>, <<"forty-two">>),
    ?assertEqual([<<"foo">>], props(C)),
    ?assertEqual(<<"forty-two">>, getp(C, <<"foo">>)),

    D = setp(C, <<"bar">>, <<"check">>),
    ?assertEqual(2, length(props(D))),
    ?assert(lists:all(fun(P) -> lists:member(P, props(D)) end,
                      [<<"foo">>, <<"bar">>])),
    ?assertEqual(<<"forty-two">>, getp(D, <<"foo">>)),
    ?assertEqual(<<"check">>, getp(D, <<"bar">>)),

    E = removep(D, <<"foo">>),
    ?assertEqual([<<"bar">>], props(E)),
    ?assertEqual(<<"check">>, getp(E, <<"bar">>)).

links_test() ->
    A = jiak_object:new(fake_object, <<"fake_key">>),
    ?assertEqual([], links(A)),
    
    L0 = [other_fake_bucket,<<"other_fake_key">>,<<"fake_tag">>],
    B = add_link(A, L0),
    ?assertEqual([L0], links(B)),
    ?assertEqual([L0], links(B, '_', '_')),
    ?assertEqual([L0], links(B, other_fake_bucket)),
    ?assertEqual([],   links(B, wrong_fake_bucket)),
    ?assertEqual([L0], links(B, '_', <<"fake_tag">>)),
    ?assertEqual([],   links(B, '_', <<"wrong_tag">>)),
    ?assertEqual([L0], links(B, other_fake_bucket, <<"fake_tag">>)),
    ?assertEqual([],   links(B, other_fake_bucket, <<"wrong_tag">>)),
    ?assertEqual([],   links(B, wrong_fake_bucket, <<"fake_tag">>)),
    
    ?assertEqual(B, add_link(B, L0)), %%don't double-add links

    %% add_link/4 should do same as add_link/2
    ?assertEqual(add_link(A, L0),
                 add_link(A, hd(L0), hd(tl(L0)), hd(tl(tl(L0))))),
    
    ?assertEqual([], links(remove_link(B, L0))),

    %% remove_link/4 should do same as remove_link/2
    ?assertEqual(remove_link(B, L0),
                 remove_link(B, hd(L0), hd(tl(L0)), hd(tl(tl(L0))))),
    
    L1 = [other_fake_bucket,<<"second_fake_key">>,<<"new_fake_tag">>],
    L2 = [new_fake_bucket,<<"third_fake_key">>,<<"fake_tag">>],
    C = add_link(add_link(B, L1), L2),
    ?assertEqual(3, length(links(C))),
    ?assertEqual(2, length(links(C, other_fake_bucket))),
    ?assertEqual(2, length(links(C, '_', <<"fake_tag">>))),
    ?assertEqual([L1], links(C, '_', <<"new_fake_tag">>)),
    ?assertEqual([L2], links(C, new_fake_bucket)).

mapreduce_test_() ->
    [fun test_linkfun/0,
     fun test_identity/0,
     fun test_wrap_fun/0].

mr_riak_object() ->
    R0 = to_riak_object(
           jiak_object:new(fake_bucket, <<"fake_key">>,
                           {struct, [{<<"a">>, 1}]},
                           [[b1, <<"k1">>, <<"t1">>],
                            [b1, <<"k2">>, <<"t2">>],
                            [b2, <<"k3">>, <<"t1">>]])),
    [{M,V}] = riak_object:get_contents(R0),
    riak_object:set_contents(
      R0,
      [{dict:store(<<"X-Riak-Last-Modified">>,
                   httpd_util:rfc1123_date(),
                   dict:store(<<"X-Riak-VTag">>, "hello", M)),
        V}]).
                                  
test_linkfun() ->
    ?assertEqual([], mapreduce_linkfun({error, notfound}, ignored, ignored)),
    ?assertEqual([{{b2, <<"k3">>}, <<"t1">>}],
                 mapreduce_linkfun(mr_riak_object(), ignored, {b2, '_'})).

test_identity() ->
    ?assertEqual([], mapreduce_identity({error, notfound}, ignored, ignored)),
    ?assertEqual([from_riak_object(mr_riak_object())],
                 mapreduce_identity(mr_riak_object(), undefined, ignored)),
    ?assertEqual([{from_riak_object(mr_riak_object()), keydata}],
                 mapreduce_identity(mr_riak_object(), keydata, ignored)).

test_wrap_fun() ->
    ?assertEqual(test_pass,
                 mapreduce_wrap_fun(
                   {error, notfound}, keydata_ignored,
                   {{qfun, fun({error, notfound},
                               keydata_ignored,
                               arg_ignored) ->
                                   test_pass
                           end},
                    arg_ignored})),
    ?assertEqual([[b1, <<"k2">>, <<"t2">>]],
                 mapreduce_wrap_fun(
                   mr_riak_object(), '_',
                   {{modfun, jiak_object, links}, <<"t2">>})).
