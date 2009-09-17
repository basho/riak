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



%% @author Bryan Fink <bryan@basho.com>
%% @doc Jaywalker resource provides a limited interface to jiak object
%%      linkwalking over HTTP.  The interface exposed is:
%%
%%      /jaywalker/Bucket/Key[/b,t,acc]
%%
%%      where:
%%
%%      Bucket/Key tells jaywalker where to start
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
%%      Return from jaywalker resource is a JSON object with one
%%      field, "results", which is a list of lists. Each lists in
%%      "results" is the list of results from the corresponding step
%%      in the query.
%%
%%      so:
%%
%%      /jaywalker/foo/123/bar,_,_ : returns all bar objects
%%      attached to foo 123 {results: [[bar1, bar2, ...]]}
%%
%%      /jaywalker/foo/123/bar,_,1/_,_,_ : returns all
%%      bar objects attached to foo 123, and all objects attached
%%      to those bar objects {results: [[bar1, bar2, ...], [baz1,
%%      quux2, ...]]}
-module(jaywalker_resource).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         expires/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {start, cache_secs, jiak_client}).

init(Props) ->
    {ok, JiakClient} = 
        case proplists:get_value(riak_local, Props) of
            true ->
                jiak:local_client();
            _ ->
                jiak:client_connect(
                  proplists:get_value(riak_ip, Props),
                  proplists:get_value(riak_port, Props),
                  proplists:get_value(riak_cookie, Props))
        end,
    {ok, #ctx{cache_secs=proplists:get_value(cache_secs, Props, 600),
              jiak_client=JiakClient}}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

is_authorized(RD, Ctx) -> {true, RD, Ctx}. %%TODO: auth

expires(RD, Ctx=#ctx{cache_secs=Secs}) ->
    {calendar:gregorian_seconds_to_datetime(
       Secs+calendar:datetime_to_gregorian_seconds(
              calendar:universal_time())),
     RD, Ctx}.

resource_exists(RD, Ctx=#ctx{jiak_client=JiakClient}) ->
    Bucket = list_to_binary(mochiweb_util:unquote(
                              wrq:path_info(bucket, RD))),
    Key = list_to_binary(mochiweb_util:unquote(
                           wrq:path_info(key, RD))),
    case JiakClient:get(Bucket, Key, 2) of
        {ok, Start} ->
            {true, RD, Ctx#ctx{start=Start}};
        _ ->
            {false, RD, Ctx}
    end.

to_json(RD, Ctx=#ctx{start=Start, jiak_client=JiakClient}) ->
    Results = execute_query(JiakClient, [Start], extract_query(RD)),
    {mochijson2:encode({struct, [{<<"results">>, Results}]}), RD, Ctx}.

execute_query(_, _, []) -> [];
execute_query(JiakClient, StartObjects, [{Bucket, Tag, Acc}|RestQuery]) ->
    StartLinks = lists:append([jiak_object:links(O, Bucket, Tag)
                               || O <- StartObjects]),
    StartBKs = [{{B, K},T} || [B, K, T] <- StartLinks],
    {SegResults,Leftover} =
        if Acc ->
                {execute_segment(JiakClient, StartBKs, []), RestQuery};
        true ->
            {SafeQuery, [LastSafe|UnsafeQuery]} =
                lists:splitwith(fun({_,_,SegAcc}) -> not SegAcc end,
                                RestQuery),
            {execute_segment(JiakClient, StartBKs,SafeQuery++[LastSafe]),
             UnsafeQuery}
     end,
    [[jiak_resource:apply_read_mask(R) || R <- SegResults]
     |execute_query(JiakClient,SegResults,Leftover)].

execute_segment(JiakClient, Start, Steps) ->
    MR = [{link, Bucket, Key, false} || {Bucket, Key, _} <- Steps]
        ++[{reduce, {modfun, riak_mapreduce, reduce_set_union}, none, false},
           {map, {modfun, jiak_object, mapreduce_identity}, none, true}],
    {ok, Objects} = (JiakClient:riak_client()):mapred(Start, MR),
    %% strip link tags from objects
    lists:map(fun(O={struct,_}) -> O;
                 ({O={struct,_},_Tag}) -> O
              end,
              Objects).

extract_query(RD) ->
    Path = wrq:disp_path(RD),
    Parts = [ string:tokens(P, ",") || P <- string:tokens(Path, "/") ],
    parts_to_query(Parts, []).

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

%% do nothing with POST
%% just allow client to use it to invalidate browser cache
process_post(RD, Ctx) ->
    {true, RD, Ctx}.
