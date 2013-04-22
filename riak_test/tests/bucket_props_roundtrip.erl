%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
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
-module(bucket_props_roundtrip).
-behaviour(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-define(BUCKET, <<"pbc_props_verify">>).
-define(COMMIT_HOOK, {struct, [{<<"mod">>, <<"foo">>}, {<<"fun">>, <<"bar">>}]}).
-define(CHASHFUN, {riak_core_util, chash_bucketonly_keyfun}).
-define(LINKFUN, {modfun, raw_link_walker, mapreduce_linkfun}).
-define(PROPS,
        [
         {allow_mult, true, false},
         {backend, <<"custom">>, <<"other">>},
         {basic_quorum, true, false},
         {big_vclock, 100, 50},
         {chash_keyfun, ?CHASHFUN, {riak_core_util, chash_std_keyfun}},
         {dw, 0, quorum},
         {last_write_wins, true, false},
         {linkfun, ?LINKFUN, {modfun, riak_kv_wm_link_walker, mapreduce_linkfun}},
         {n_val, 2, 3},
         {notfound_ok, false, true},
         {old_vclock, 10000, 86400},
         {postcommit, [?COMMIT_HOOK], []},
         {pr, 2, 0},
         {precommit, [?COMMIT_HOOK], []},
         {pw, all, 0},
         {r, all, quorum},
         {repl, realtime, false},
         {rw, 1, quorum},
         {search, true, false},
         {small_vclock, 10, 50},
         {w, one, quorum},
         {young_vclock, 0, 20}
        ]).

confirm() ->
    [Node] = Nodes = rt:build_cluster(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),

    [ check_prop_set_and_get(Node, Prop, FirstVal, SecondVal) ||
        {Prop, FirstVal, SecondVal} <- ?PROPS ],

    pass.

check_prop_set_and_get(Node, Prop, One, Two) ->
    lager:info("-------- Testing roundtrip for property '~p' ---------", [Prop]),
    HTTP = rt:httpc(Node),
    PBC = rt:pbc(Node),
    lager:info("HTTP set = ~p", [One]),
    http_set_property(HTTP, Prop, One),
    lager:info("PBC get should == ~p", [One]),
    ?assertEqual(One, pbc_get_property(PBC, Prop)),

    lager:info("PBC set = ~p", [Two]),
    pbc_set_property(PBC, Prop, Two),
    lager:info("HTTP get should = ~p", [Two]),
    ?assertEqual(Two, http_get_property(HTTP, Prop)),
    ok.


http_set_property(Client, Prop, Value) ->
    rhc:set_bucket(Client, ?BUCKET, [{Prop, Value}]).

http_get_property(Client, Prop) ->
    {ok, Props} = rhc:get_bucket(Client, ?BUCKET),
    case lists:keyfind(Prop, 1, Props) of
        {Prop, Value} ->
            Value;
        _ -> undefined
    end.

pbc_set_property(Client, Prop, Value) ->
    riakc_pb_socket:set_bucket(Client, ?BUCKET, [{Prop, Value}]).

pbc_get_property(Client, Prop) ->
    {ok, Props} = riakc_pb_socket:get_bucket(Client, ?BUCKET),
    case lists:keyfind(Prop, 1, Props) of
        {Prop, Value} ->
            Value;
        _ -> undefined
    end.
