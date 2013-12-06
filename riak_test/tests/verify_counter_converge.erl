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
%%% @author Russell Brown <russelldb@basho.com>
%%% @copyright (C) 2012, Basho Technologies
%%% @doc
%%% riak_test for riak_dt counter convergence,
%%% @end

-module(verify_counter_converge).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    inets:start(),
    Key = a,

    [N1, N2, N3, N4]=Nodes = rt:build_cluster(4),
    [HP1, HP2, HP3, HP4]=Hosts =  get_host_ports(Nodes),

    increment_counter(HP1, Key),
    increment_counter(HP2, Key, 10),

    [?assertEqual(11, get_counter(HP, Key)) || HP <- Hosts],

    decrement_counter(HP3, Key),
    decrement_counter(HP4, Key, 2),

    [?assertEqual(8, get_counter(HP, Key)) || HP <- Hosts],

    lager:info("Partition cluster in two."),

    PartInfo = rt:partition([N1, N2], [N3, N4]),

    %% increment one side
    increment_counter(HP1, Key, 5),

    %% check vaule on one side is different from other
    [?assertEqual(13, get_counter(HP, Key)) || HP <- [HP1, HP2]],
    [?assertEqual(8, get_counter(HP, Key)) || HP <- [HP3, HP4]],

    %% decrement other side
    decrement_counter(HP3, Key, 2),

    %% verify values differ
    [?assertEqual(13, get_counter(HP, Key)) || HP <- [HP1, HP2]],
    [?assertEqual(6, get_counter(HP, Key)) || HP <- [HP3, HP4]],

    %% heal
    lager:info("Heal and check merged values"),
    ok = rt:heal(PartInfo),
    ok = rt:wait_for_cluster_service(Nodes, riak_dt),

    %% verify all nodes agree
    [?assertEqual(ok, rt:wait_until(HP, fun(N) ->
                                                11 == get_counter(N, Key)
                                        end)) ||  HP <- Hosts],

    pass.

get_host_ports(Nodes) ->
    {ResL, []} = rpc:multicall(Nodes, application, get_env, [riak_core, http]),
    [{Host, Port} || {ok, [{Host, Port}]} <- ResL].

%% Counter API
get_counter(HostPort, Key) ->
    get(HostPort, Key).

increment_counter(HostPort, Key) ->
    increment_counter(HostPort, Key, 1).

increment_counter(HostPort, Key, Amt) ->
    update_counter(HostPort, Key, increment, Amt).

decrement_counter(HostPort, Key) ->
    decrement_counter(HostPort, Key, 1).

decrement_counter(HostPort, Key, Amt) ->
    update_counter(HostPort, Key, decrement, Amt).

update_counter(HostPort, Key, Action, Amt) ->
    post(HostPort, [Key, Action], integer_to_list(Amt)).

%% HTTP API
url({Host, Port}, PathElements) ->
    Path = path([counters|PathElements]),
    lists:flatten(io_lib:format("http://~s:~p~s", [Host, Port, Path])).

path(Elements) ->
    [lists:concat(['/', E]) || E <- Elements].

get(HostPort, Key) ->
    Url = url(HostPort, [Key]),
    {ok, {{_Version, 200, _}, _, Body}} =
        httpc:request(Url),
    list_to_integer(Body).

post(HostPort, PathElements, Body) ->
    Url = url(HostPort, PathElements),
    {ok, {{_Version, 204, _}, _, _}} =
        httpc:request(post, {Url, [], [], Body}, [], []),
    ok.
