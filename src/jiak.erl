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

-export([local_client/0, client_connect/1]).
-export([default_jiak_bucket_props/0]).
-export([standard_sibling_merge/1]).

-include_lib("eunit/include/eunit.hrl").

%% @spec local_client() -> {ok, jiak_client()}|error_term()
%% @doc Open a Riak client for modifying Jiak objects.
%% @see riak:local_client/0
local_client() -> client_connect(node()).

%% @spec client_connect(Node :: node())
%%        -> {ok, Client :: jiak_client()} | exception
%% @doc The usual way to get a client.  Timeout often means either a bad
%%      cookie or a poorly-connected distributed erlang network.
client_connect(Node) -> 
    case riak:client_connect(Node) of
        {ok, C} -> {ok, jiak_client:new(C)};
        Error -> Error
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

%%
%% Testing
%%

sib_sort_test() ->
    Dated = fun(D,N) ->
                    {dict:store(<<"X-Riak-Last-Modified">>,
                                httpd_util:rfc1123_date(D),
                                dict:new()),
                     N}
            end,
    A = Dated({{2009,8,28},{14,0,0}}, a),
    B = Dated({{2009,8,28},{14,0,1}}, b),
    C = Dated({{2009,8,28},{14,1,0}}, c),
    D = Dated({{2009,8,28},{15,0,0}}, d),
    E = Dated({{2009,8,29},{14,0,0}}, e),
    F = Dated({{2009,9,28},{14,0,0}}, f),
    G = Dated({{2010,8,28},{14,0,0}}, g),
    [G,F,E,D,C,B,A] = sort_sibs([D,G,B,A,C,F,E]).

merge_sibs_test_() ->
    [fun merge_output_structure_t/0,
     fun merge_metadata_t/0,
     fun merge_fields_t/0,
     fun merge_links_t/0].

%% merge didn't fail
merge_output_structure_t() ->
    {_CM, {{struct, _CO}, _CL}} =
        merge_sibs({dict:new(),{{struct,[]},[]}},
                   {dict:new(),{{struct,[]},[]}}).

%% metadata properly constructed
merge_metadata_t() ->
    {CM,_} = merge_sibs({dict:store(b, b, dict:store(c, b, dict:new())),
                         {{struct,[]},[]}},
                        {dict:store(a, a, dict:store(c, a, dict:new())),
                         {{struct,[]},[]}}),
    3 = dict:size(CM),
    a = dict:fetch(a, CM),
    b = dict:fetch(b, CM),
    a = dict:fetch(c, CM).
    
%% fields properly constructed
merge_fields_t() ->
    {_,{{struct,CO},_}} = merge_sibs({dict:new(),
                                      {{struct, [{b,b},{c,b}]},[]}},
                                     {dict:new(),
                                      {{struct, [{a,a},{c,a}]},[]}}),
    3 = length(CO),
    a = proplists:get_value(a, CO),
    b = proplists:get_value(b, CO),
    a = proplists:get_value(c, CO).

%% links properly constructed
merge_links_t() ->
    {_,{_,CL}} = merge_sibs({dict:new(),{{struct,[]},[b,c]}},
                            {dict:new(),{{struct,[]},[a,c]}}),
    3 = length(CL),
    true = lists:member(a, CL),
    true = lists:member(b, CL),
    true = lists:member(c, CL).

standard_sibling_merge_test() ->
    A = {dict:store(<<"X-Riak-Last-Modified">>,
                    httpd_util:rfc1123_date({{2009,8,28},{14,0,0}}),
                    dict:store(a,a,dict:store(x,a,dict:store(y,a,dict:new())))),
         {{struct,[{a,a},{x,a},{y,a}]},[a,z]}},
    B = {dict:store(<<"X-Riak-Last-Modified">>,
                    httpd_util:rfc1123_date({{2009,8,28},{14,0,1}}),
                    dict:store(b,b,dict:store(x,b,dict:store(z,b,dict:new())))),
         {{struct,[{b,b},{x,b},{z,b}]},[b,z]}},
    C = {dict:store(<<"X-Riak-Last-Modified">>,
                    httpd_util:rfc1123_date({{2009,8,28},{14,0,2}}),
                    dict:store(c,c,dict:store(y,c,dict:store(z,c,dict:new())))),
         {{struct,[{c,c},{y,c},{z,c}]},[c,z]}},

    %% structure okay
    {MM, {{struct, MF}, ML}} = standard_sibling_merge([A,B,C]),
    
    %% metadata okay
    7 = dict:size(MM),
    ?assertEqual(httpd_util:rfc1123_date({{2009,8,28},{14,0,2}}),
                 dict:fetch(<<"X-Riak-Last-Modified">>, MM)),
    [a,b,c,b,c,c] = [ dict:fetch(Q, MM) || Q <- [a,b,c,x,y,z] ],
    
    %% fields okay
    6 = length(MF),
    [a,b,c,b,c,c] = [ proplists:get_value(Q, MF) || Q <- [a,b,c,x,y,z] ],
    
    %% links okay
    4 = length(ML),
    true = lists:all(fun(Q) -> lists:member(Q, ML) end, [a,b,c,z]).
