%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
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
%% @doc Verify that POSTing to a bucket URL generates a key for an
%% object correctly.
-module(post_generate_key).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    Nodes = rt:build_cluster(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),
    
    [Base|_] = rt:http_url(Nodes),

    Bucket = "post_generate_key",
    OldPostUrl = old_url(Base, Bucket),
    NewPostUrl = new_url(Base, Bucket),

    OldPostResult = post(OldPostUrl),
    NewPostResult = post(NewPostUrl),

    ?assert(post_was_successful(OldPostResult)),
    ?assert(post_was_successful(NewPostResult)),

    OldLocation = location_header(OldPostResult),
    NewLocation = location_header(NewPostResult),
    ?assert(is_old_url(OldLocation)),
    ?assert(is_new_url(NewLocation)),

    OldGetResult = get_url(Base++OldLocation),
    NewGetResult = get_url(Base++NewLocation),

    ?assert(get_was_successful(OldGetResult)),
    ?assert(get_was_successful(NewGetResult)),

    pass.

old_url(Base, Bucket) ->
    Base++"/riak/"++Bucket.

new_url(Base, Bucket) ->
    Base++"/buckets/"++Bucket++"/keys".

post(Url) ->
    ibrowse:send_req(Url, [{"content-type", "text/plain"}],
                     post, "foobar").

get_url(Url) ->
    ibrowse:send_req(Url, [{"accept", "text/plain"}], get).

post_was_successful({ok, "201", _, _}) -> true;
post_was_successful(Other) ->
    lager:warning("That's not a 201: ~p", [Other]),
    false.

location_header({ok, _, Headers, _}) ->
    proplists:get_value("Location", Headers).

is_old_url(Url) ->
    case re:run(Url, "^/riak/") of
        {match, _} ->
            true;
        nomatch ->
            lager:warning("That's not an old url: ~s", [Url]),
            false
    end.

is_new_url(Url) ->
    case re:run(Url, "^/buckets/.*/keys/") of
        {match, _} ->
            true;
        nomatch ->
            lager:warning("That's not a new url: ~s", [Url]),
            false
    end.


get_was_successful({ok, "200", _, _}) -> true;
get_was_successful(Other) ->
    lager:warning("That's not a 200: ~p", [Other]),
    false.
