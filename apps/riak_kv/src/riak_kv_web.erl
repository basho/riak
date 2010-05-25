%% -------------------------------------------------------------------
%%
%% riak_kv_web: setup Riak's KV HTTP interface
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

%% @doc Convenience functions for setting up the HTTP interface
%%      of Riak.  This module loads parameters from the application
%%      environment:
%%
%%<dl><dt> raw_name
%%</dt><dd>   the base path under which the riak_kv_wm_raw
%%            should be exposed; defaulted to "raw"
%%</dd></dl>
-module(riak_kv_web).

-export([dispatch_table/0]).

dispatch_table() ->
    MapredProps = mapred_props(),
    StatsProps = stats_props(),

    lists:append(
      raw_dispatch(),
      [{[proplists:get_value(prefix, MapredProps)],
        riak_kv_wm_mapred, MapredProps},
       {[proplists:get_value(prefix, StatsProps)],
        riak_kv_wm_stats, StatsProps},
       {["ping"], riak_kv_wm_ping, []}]).

raw_dispatch() ->
    case app_helper:get_env(riak_kv, raw_name) of
        undefined -> raw_dispatch("riak");
        Name -> lists:append(raw_dispatch(Name), raw_dispatch("riak"))
    end.

raw_dispatch(Name) ->
    Props = raw_props(Name),
    [{[Name, bucket], riak_kv_wm_raw, Props},
     {[Name, bucket, key], riak_kv_wm_raw, Props},
     {[Name, bucket, key, '*'], riak_kv_wm_link_walker, Props}].

raw_props(Prefix) ->
    [{prefix, Prefix}, {riak, local}].

mapred_props() ->
    [{prefix, app_helper:get_env(riak_kv, mapred_name, "mapred")}].

stats_props() ->
    [{prefix, app_helper:get_env(riak_kv, stats_urlpath, "stats")}].
