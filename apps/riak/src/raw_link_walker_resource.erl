%% -------------------------------------------------------------------
%%
%% raw_link_walker_resource: HTTP access to Riak link traversal
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%      @doc Raw link walker resource provides an interface to riak object
%%      linkwalking over HTTP.  The interface exposed is:
%%
%%      /riak/Bucket/Key[/b,t,acc]
%%
%%      where:
%%
%%      Bucket/Key tells the link walker where to start
%%
%%      each /b,t,acc segment is a request to follow some links
%%
%%      b is a filter on buckets
%%      t is a filter on tags
%%      acc is whether or not to return the objects from that step
%%
%%      each of b,t,acc may be underscore, to signify wildcard
%%
%%      acc is by default '0' (do not return these objects), except
%%      for the final /b,t,acc segment, for which it is by default '1'
%%      (return the objects)
%%
%%      Return from the walker resource is a multipart/mixed body each
%%      portion of that body being a list of results for the
%%      corresponding link step (itself a multipart/mixed list, each
%%      portion of which is a matching object, encoded as an HTTP
%%      request would have been from the raw_http_resource).
%%
%%      so:
%%
%%      /riak/foo/123/bar,_,_ : returns all bar objects
%%      attached to foo 123:
%%        Content-type: multipart/mixed; boundary=ABC
%%
%%        --ABC
%%        Content-type: multipart/mixed; boundary=XYZ
%%
%%        --XYZ
%%        Content-type: bar1-content-type
%%
%%        bar1-body
%%        --XYZ
%%        Content-type: bar2-content-type
%%
%%        bar2-body
%%        --XYZ--
%%      --ABC--
%%
%%      /riak/foo/123/bar,_,1/_,_,_ : returns all
%%      bar objects attached to foo 123, and all objects attached
%%      to those bar objects:
%%        Content-type: multipart/mixed; boundary=ABC
%%
%%        --ABC
%%        Content-type: multipart/mixed; boundary=XYZ
%%
%%        --XYZ
%%        Content-type: bar1-content-type
%%
%%        bar1-body
%%        --XYZ
%%        Content-type: bar2-content-type
%%
%%        bar2-body
%%        --XYZ--
%%        --ABC
%%        Content-type: multipart/mixed; boundary=QRS
%%
%%        --QRS
%%        Content-type: baz1-content-type
%%
%%        baz1-body
%%        --QRS
%%        Content-type: quux2-content-type
%%
%%        quux2-body
%%        --QRS--
%%      --ABC--
%%
%% Webmachine dispatch line for this resource should look like:
%%
%%  {["riak", bucket, key, '*'],
%%   raw_http_resource,
%%   [{prefix, "riak"},
%%    {riak, local}, %% or {riak, {'riak@127.0.0.1', riak_cookie}}
%%    {cache_secs, 60}
%%   ]}.
%%
%% These example dispatch lines will expose this resource at
%% /riak/Bucket/Key/*.  The resource will attempt to
%% connect to Riak on the same Erlang node one which the resource
%% is executing.  Using the alternate {riak, {Node, Cookie}} form
%% will cause the resource to connect to riak on the specified
%% Node with the specified Cookie.  The Expires header will be
%% set 60 seconds in the future (default is 600 seconds).
-module(raw_link_walker_resource).
-author('Bryan Fink <bryan@basho.com>').

%% webmachine resource exports
-export([
         init/1,
         service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         expires/2,
         to_multipart_mixed/2,
         process_post/2
        ]).

%% map/reduce link-syntax export
-export([mapreduce_linkfun/3]).

-include_lib("webmachine/include/webmachine.hrl").
-include("raw_http.hrl").

%% @type context() = term()
-record(ctx, {prefix,     %% string() - prefix for resource urls
              riak,       %% local | {node(), atom()} - params for riak client
              bucket,     %% binary() - Bucket name (from uri)
              key,        %% binary() - Key (from uri)
              start,      %% riak_object() - the starting point of the walk
              cache_secs, %% integer() - number of seconds to add for expires header
              client      %% riak_client() - the store client
             }).

%% @spec mapreduce_linkfun({error, notfound}|riak_object(), term(), {binary(), binary()}) ->
%%          [link()]
%% @type link() = {{Bucket::binary(), Key::binary()}, Tag::binary()}
%% @doc Extract the links from Object that match {Bucket, Tag}.
%%      Set this function as the bucket property linkfun to enable
%%      {link, Bucket, Key, Acc} syntax in mapreduce queries on the bucket.
%%      Client:set_bucket(Bucket, [{linkfun, {modfun, raw_link_walker_resource,
%%                                            mapreduce_linkfun}}])
mapreduce_linkfun({error, notfound}, _, _) -> [];
mapreduce_linkfun(Object, _, {Bucket, Tag}) ->
    links(Object, Bucket, Tag).

%% @spec links(riak_object()) -> [link()]
%% @doc Get all of the links that Object has.
links(Object) ->
    MDs = riak_object:get_metadatas(Object),
    lists:umerge(
      [ case dict:find(?MD_LINKS, MD) of
            {ok, L} ->
                [ [B,K,T] || {{B,K},T} <- lists:sort(L) ];
            error -> []
        end
        || MD <- MDs ]).

%% @spec links(riak_object(), binary()|'_', binary()|'_') -> [link()]
%% @doc Get all of the links Object has that match Bucket and Tag.
%%      Pass binaries for Bucket or Tag to match the bucket or
%%      tag of the link exactly.  Pass the atom '_' to match any
%%      bucket or tag.
links(Object, Bucket, Tag) ->
    lists:filter(link_match_fun(Bucket, Tag), links(Object)).

%% @spec link_match_fun(binary()|'_', binary()|'_') -> function()
%% @doc Create a function suitable for lists:filter/2 for filtering
%%      links by Bucket and Tag.
link_match_fun('_', '_') ->
    fun(_) -> true end;
link_match_fun('_', Tag) ->
    fun([_B, _K, T]) -> Tag == T end;
link_match_fun(Bucket, '_') ->
    fun([B, _K, _T]) -> Bucket == B end;
link_match_fun(Bucket, Tag) ->
    fun([B, _K, T]) -> Bucket == B andalso Tag == T end.

%% @spec init(proplist()) -> {ok, context()}
%% @doc Initialize the resource.  This function extacts the 'prefix',
%%      'riak', and 'chache_secs' properties from the dispatch args.
init(Props) ->
    {ok, #ctx{prefix=proplists:get_value(prefix, Props),
              riak=proplists:get_value(riak, Props),
              cache_secs=proplists:get_value(cache_secs, Props, 600)
             }}.

%% @spec service_available(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Determine whether or not a connection to Riak
%%      can be established.  This function also takes this
%%      opportunity to extract the 'bucket' and 'key' path
%%      bindings from the dispatch.
service_available(RD, Ctx=#ctx{riak=RiakProps}) ->
    case get_riak_client(RiakProps) of
        {ok, C} ->
            {true,
             RD,
             Ctx#ctx{
               client=C,
               bucket=list_to_binary(wrq:path_info(bucket, RD)),
               key=list_to_binary(wrq:path_info(key, RD))
              }};
        Error ->
            {false,
             wrq:set_resp_body(
               io_lib:format("Unable to connect to Riak: ~p~n", [Error]),
               wrq:set_resp_header(?HEAD_CTYPE, "text/plain", RD)),
             Ctx}
    end.

%% @spec get_riak_client(local|{node(),Cookie::atom()}) ->
%%          {ok, riak_client()} | error()
%% @doc Get a riak_client.
get_riak_client(local) ->
    riak:local_client();
get_riak_client({Node, Cookie}) ->
    erlang:set_cookie(node(), Cookie),
    riak:client_connect(Node).

%% @spec allowed_methods(reqdata(), context()) ->
%%          {[method()], reqdata(), context()}
%% @doc Get the list of methods this resource supports.
%%      HEAD, GET, and POST are supported.  POST does nothing,
%%      though, and is only exposed for browser-cache-clearing
%%      purposes
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

%% @spec content_types_provided(reqdata(), context()) ->
%%          {[{ContentType::string(), Producer::atom()}], reqdata(), context()}
%% @doc List the content types available for representing this
%%      resource.  Currently only multipart/mixed is supported.
content_types_provided(RD, Ctx) ->
    {[{"multipart/mixed", to_multipart_mixed}], RD, Ctx}.

expires(RD, Ctx=#ctx{cache_secs=Secs}) ->
    {calendar:gregorian_seconds_to_datetime(
       Secs+calendar:datetime_to_gregorian_seconds(
              calendar:universal_time())),
     RD, Ctx}.

%% @spec resource_exists(reqdata(), context()) -> {boolean(), reqdata(), context()}
%% @doc This resource exists if Riak returns {ok, riak_object()} from
%%      a get of the starting document.
resource_exists(RD, Ctx=#ctx{bucket=B, key=K, client=C}) ->
    case C:get(B, K, 2) of
        {ok, Start} ->
            {true, RD, Ctx#ctx{start=Start}};
        _ ->
            {false, RD, Ctx}
    end.

%% @spec to_multipart_mixed(reqdata(), context()) -> {iolist(), reqdata(), context()}
%% @doc Execute the link walking query, and build the response body.
%%      This function has to explicitly set the Content-Type header,
%%      because Webmachine doesn't know to add the "boundary" parameter to it.
to_multipart_mixed(RD, Ctx=#ctx{start=Start, client=C}) ->
    Results = execute_query(C, [Start], extract_query(RD)),
    Boundary = riak_util:unique_id_62(),
    {multipart_mixed_encode(Results, Boundary, Ctx),
     %% reset content-type now that we now what it is
     wrq:set_resp_header(?HEAD_CTYPE,
                         "multipart/mixed; boundary="++Boundary,
                         RD),
     Ctx}.

%% @spec execute_query(riak_client(), [riak_object()], [linkquery()]) ->
%%          [[riak_object()]]
%% @type linkquery() = {Bucket::binary()|'_', Tag::binary()|'_', Acc::boolean()}
%% @doc Execute the link query.  Return a list of link step results,
%%      each link step result being a list of Riak objects.  Link
%%      step results are only returns for those steps that specify
%%      Acc as 'true'.
%%      This function chops up the list of steps into segments of contiguous
%%      Acc==false steps.  Acc==true requires an end to a map/reduce query in
%%      order to package up the results of that step for delivery to the client.
execute_query(_, _, []) -> [];
execute_query(C, StartObjects, [{Bucket, Tag, Acc}|RestQuery]) ->
    StartLinks = lists:append([links(O, Bucket, Tag)
                               || O <- StartObjects]),
    {SegResults,Leftover} =
        if Acc ->
                {execute_segment(C, StartLinks, []), RestQuery};
        true ->
            {SafeQuery, [LastSafe|UnsafeQuery]} =
                lists:splitwith(fun({_,_,SegAcc}) -> not SegAcc end,
                                RestQuery),
            {execute_segment(C, StartLinks,SafeQuery++[LastSafe]),
             UnsafeQuery}
     end,
    [SegResults|execute_query(C,SegResults,Leftover)].

%% @spec execute_segment(riak_client, [bkeytag()], [linkquery()]) ->
%%          [riak_object()]
%% @doc Execute a string of link steps, where only the last step's
%%      result will be kept for later.
execute_segment(C, Start, Steps) ->
    MR = [{link, Bucket, Key, false} || {Bucket, Key, _} <- Steps]
        ++[riak_mapreduce:reduce_set_union(false),
           riak_mapreduce:map_identity(true)],
    {ok, Objects} = C:mapred(Start, MR),
    %% strip link tags from objects
    lists:map(fun({O,_Tag}) -> O;
                 (O)        -> O
              end,
              Objects).

%% @spec extract_query(reqdata()) -> [linkquery()]
%% @doc Extract the link-walking query from the URL chunk after the
%%      bucket and key.
extract_query(RD) ->
    Path = wrq:disp_path(RD),
    Parts = [ string:tokens(P, ",") || P <- string:tokens(Path, "/") ],
    parts_to_query(Parts, []).

%% @spec parts_to_query([toeknizedlink()], [linkquery()]) ->
%%          [linkquery()]
%% @type tokenizedlink() = [string()]
%% @doc Translate each token-ized string link query to the real link
%%      query format.
parts_to_query([], Acc) -> lists:reverse(Acc);
parts_to_query([[B,T,A]|Rest], Acc) ->
    parts_to_query(Rest,
                   [{if B == "_" -> '_';
                        true     -> list_to_binary(mochiweb_util:unquote(B))
                     end,
                     if T == "_" -> '_';
                        true     -> list_to_binary(mochiweb_util:unquote(T))
                     end,
                     if A == "1"          -> true;
                        A == "0"          -> false;
%%% default of "acc" is 'true' for final step
                        length(Rest) == 0 -> true;
%%% default of "acc" is 'false' for intermediate steps
                        true              -> false
                     end}
                    |Acc]).

%% @spec process_post(reqdata(), context()) -> {true, reqdata(), context()}
%% @doc do nothing with POST
%%      just allow client to use it to invalidate browser cache
process_post(RD, Ctx) ->
    {true, RD, Ctx}.

%% @spec multipart_mixed_encode([riak_object()]|[[riak_object()]], string(), context()) -> iolist()
%% @doc Encode the list of result lists, or a single result list in a
%%      multipart body.
multipart_mixed_encode(WalkResults, Boundary, Ctx) ->
    [[["\n--",Boundary,"\n",multipart_encode_body(R, Ctx)]
      || R <- WalkResults],
     "\n--",Boundary,"--\n"].

%% @spec multipart_encode_body(riak_object()|[riak_object()], context()) -> iolist()
%% @doc Encode a riak object (as an HTTP response much like what raw_http_resource
%%      would produce) or a result list (as a multipart/mixed document).
%%      Riak object body will include a Location header to describe where to find
%%      the object.  An object with siblings will encode as one of the siblings
%%      (arbitrary choice), with an included vtag query param in the Location header.
multipart_encode_body(NestedResults, Ctx) when is_list(NestedResults) ->
    Boundary = riak_util:unique_id_62(),
    [?HEAD_CTYPE, ": multipart/mixed; boundary=",Boundary,"\n",
     multipart_mixed_encode(NestedResults, Boundary, Ctx)];
multipart_encode_body(RiakObject, #ctx{prefix=Prefix}) ->
    [{MD, V}|Rest] = riak_object:get_contents(RiakObject),
    {VHead, Vclock} = raw_http_resource:vclock_header(RiakObject),
    [VHead,": ",Vclock,"\n",

     "Location: /",Prefix,"/",
     mochiweb_util:quote_plus(riak_object:bucket(RiakObject)),"/",
     mochiweb_util:quote_plus(riak_object:key(RiakObject)),
     if Rest /= [] ->
             ["?",?Q_VTAG,"=",dict:fetch(?MD_VTAG, MD)];
        true ->
             []
     end,
     "\n",

     if Rest /= [] ->
             ["X-Riak-Sibling-VTags: ",
              dict:fetch(?MD_VTAG, element(1, hd(Rest))),
              [[",", dict:fetch(?MD_VTAG, SMD)]
               || {SMD,_} <- tl(Rest)],
              "\n"];
        true ->
             []
     end|
     raw_http_resource:multipart_encode_body(
       Prefix,
       riak_object:bucket(RiakObject),
       {MD,V})].
