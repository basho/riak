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

%% @doc Jiak: Riak utilities, focused on JSON-encoded data.
%%
%%      Riak is web-shaped, and one of the most useful
%%      interchange formats on the web is JSON.  The Jiak libraries
%%      are intended to provide a simple system for serving data from
%%      Riak in the JSON format.
%%
%%      Jiak objects take the shape of JSON objects of the form:
%%<pre>
%%      {
%%       "bucket":"foo",
%%       "key":"123",
%%       "object":{
%%                 "barField":"bar",
%%                 "bazField":42
%%                }
%%       "links":[
%%                ["quuxbucket","quuxkey","quuxtag"],
%%                ["abcbucket","abckey","qbctag"]
%%               ]
%%       "vclock":"opaque-riak-vclock",
%%       "lastmod":"Mon, 03 Aug 2009 18:49:42 GMT"
%%      }
%%</pre>
%%      Within Riak, these fields get broken into their appropriate
%%      places in riak_object, but using the Jiak interface,
%%      applications see only mochijson2-encoded objects.  An
%%      application can choose to either dig through the structure the
%%      Erlang pattern-match way, or by using the interface provided
%%      by the jiak_object module.
%%
%%      Access to these objects as JSON is provided to clients that
%%      can speak HTTP by way of the jiak_resource Webmachine resource
%%      module.  Clients can GET bucket schemas and key lists, as well
%%      as fetch and store objects.  Semantics about which fields are
%%      allowed/readable/editable/etc. are provided by user-defined
%%      Erlang modules named for the bucket of the object.  For an
%%      example of such a module, see {@link jiak_example}.
%%
%%      Further access is provided by means of 'link' mapreduce specs
%%      and jaywalker_resource.  To support each of these, you should
%%      open a jiak_client, then call set_bucket/2 with your bucket
%%      name and the result of default_jiak_bucket_props/0.  Without
%%      that setting, the mapreduce system won't know how to extract
%%      links for following.
-module(jiak).

-export([local_client/0,
         client_connect/3, client_connect/4]).
-export([default_jiak_bucket_props/0]).
-export([standard_sibling_merge/1]).

%% @spec local_client() -> {ok, jiak_client()}|error_term()
%% @doc Open a Riak client for modifying Jiak objects.
%% @see riak:local_client/0
local_client() ->
    {ok, C} = riak:local_client(),
    {ok, jiak_client:new(C)}.

%% @spec client_connect(IP :: list(), Port :: integer(), RiakCookie :: atom())
%%        -> {ok, Client :: jiak_client()} | {error, timeout}
%% @doc The usual way to get a client.  Timeout often means either a bad
%%      cookie or a poorly-connected distributed erlang network.
client_connect(IP,Port,RiakCookie)->
    client_connect(IP, Port, RiakCookie, 1000).

%% @spec client_connect(IP :: list(), Port :: integer(), RiakCookie :: atom(),
%%                      TimeoutMillisecs :: integer())
%%        -> {ok, Client :: jiak_client()} | {error, timeout}
%% @doc The usual way to get a client.  Timeout often means either a bad
%%      cookie or a poorly-connected distributed erlang network.
client_connect(IP,Port,RiakCookie,Timeout) ->
    case riak:client_connect(IP,Port,RiakCookie,Timeout) of
        {ok, C} -> {ok, jiak_client:new(C)};
        Error   -> Error
    end.

%% @spec default_jiak_bucket_props() -> [bucket_prop()]
%% @doc Returns the default additional bucket parameters for Jiak
%%      buckets, suitable for the second parameter in a call to
%%      jiak_client:set_bucket/2.
%%      The only property is currently 'linkfun' which sets up
%%      the given bucket to support the {link, Bucket, Tag, Acc}
%%      mapreduce spec.
default_jiak_bucket_props() ->
    [{linkfun, {modfun, jiak_object, mapreduce_linkfun}}].

%%
%% Utility - Merging siblings
%%

%% @doc basic strategy:
%%<ul><li> set-union all links
%%</li><li>create an object with most-recent version of each field,
%%           according to object's last-modified date - fields missing
%%           in newer versions will have their values taken from older
%%           versions
%%</li></ul>
standard_sibling_merge(Sibs) ->
    [{Ms,{{struct,Os},Ls}}|Rest] = sort_sibs(Sibs),
    lists:foldl(fun merge_sibs/2,
                {Ms,{{struct,lists:keysort(1,Os)},Ls}},
                Rest).

%% @private
sort_sibs(Sibs) ->
    lists:sort(
      fun({MD1, _}, {MD2, _}) ->
              riak_util:compare_dates(
                dict:fetch(<<"X-Riak-Last-Modified">>, MD1),
                dict:fetch(<<"X-Riak-Last-Modified">>, MD2))
      end,
      Sibs).

%% @private
merge_sibs({Min,  {{struct, Oin},  Lin}},
           {Macc, {{struct, Oacc}, Lacc}}) ->
    %% add keys to M, but do not overwrite
    M = dict:merge(fun(_, Ma, _) -> Ma end, Macc, Min),
    %% add keys to O, but do not overwrite
    O = lists:foldl(fun({Field, Value}, Acc) ->
                            case proplists:is_defined(Field, Acc) of
                                true -> Acc;
                                false -> [{Field, Value}|Acc]
                            end
                    end,
                    Oacc, Oin),
    %% union set of links
    L = sets:to_list(
          sets:union(sets:from_list(Lin), sets:from_list(Lacc))),
    {M, {{struct, O}, L}}.
