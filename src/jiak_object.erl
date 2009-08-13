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
-export([test_roundtrip_vclock/0]).

%% @type jiak_key()=riak_object:binary_key().

%% @spec new(riak_object:bucket(), jiak_key()) -> jiak_object()
%% @doc produces an empty jiak object
%% @equiv new(B, K, {struct, []}, [])
new(B, K) -> new(B, K, {struct, []}, []).

%% @spec new(riak_object:bucket(), jiak_key(), mochijson2()) -> jiak_object()
%% @doc produces a new jiak object with no links
%% @equiv new(B, K, O, [])
new(B, K, O) -> new(B, K, O, []).

%% @spec new(riak_object:bucket(), jiak_key(), mochijson2(), 
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

%% @spec remove_link(jiak_object, riak_object:bucket(), jiak_key(), term()) 
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
    base64:encode(zlib:zip(make_vclock_headers(lists:sort(VClock), []))).

%% @private
make_vclock_headers([], Acc) ->
    list_to_binary(string:join(lists:reverse(Acc), ","));
make_vclock_headers([{Node, {Counter, Timestamp}}|T], Acc) ->
    Hdr = format_vclock_header(Node, Counter, Timestamp),
    make_vclock_headers(T, [Hdr|Acc]).    

%% @private
format_vclock_header(Node, Counter, Timestamp) when is_list(Node); is_binary(Node) ->
    io_lib:format("~s/~B/~B", [Node, 
                               Counter, 
                               Timestamp]);
format_vclock_header(Node, Counter, Timestamp) ->
    format_vclock_header(atom_to_list(Node), Counter, Timestamp).

%% @spec headers_to_vclock(binary()) -> riak:vclock()
%% @doc Convert a Jiak object vclock (a binary created by
%%      vclock_to_headers/1) into a vclock appropriate for storing
%%      in a riak_object.
headers_to_vclock(Header) when is_binary(Header) ->
    headers_to_vclock(binary_to_list(zlib:unzip(base64:decode(Header))));
headers_to_vclock(Header) ->
    [{list_to_binary(N), 
      {list_to_integer(C), list_to_integer(T)}} || 
	{N, C, T} <- 
	    [list_to_tuple(string:tokens(H, "/")) 
	     || H <- [string:strip(S) || S <- string:tokens(Header, ",")]]].

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

test_roundtrip_vclock() ->
    Vclock = vclock:increment(riak_util:mkclientid(node()),
                              vclock:fresh()),
    Vclock = headers_to_vclock(vclock_to_headers(Vclock)).
