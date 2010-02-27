%% -------------------------------------------------------------------
%%
%% riak_console: interface for Riak admin commands
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

%% @doc interface for Riak admin commands

-module(riak_console).

-export([join/1, status/1, reip/1]).

join([NodeStr]) ->
    case riak:join(NodeStr) of
        ok ->
            io:format("Sent join request to ~s\n", [NodeStr]),
            ok;
        {error, not_reachable} ->
            io:format("Node ~s is not reachable!\n", [NodeStr]),
            error
    end;
join(_) ->
    io:format("Join requires a node to join with.\n"),
    error.

status([]) ->
    case whereis(riak_stat) of
        undefined ->
            io:format("riak_stat is not enabled.~n");
        _ ->
            StatString =
                format_stats(riak_stat:get_stats(), 
                             ["-------------------------------------------\n",
                              io_lib:format("1-minute stats for ~p~n",[node()])]),
            io:format("~s~n", [StatString])
    end.

reip([OldNode, NewNode]) ->
    application:load(riak),
    RingStateDir = riak:get_app_env(ring_state_dir),
    {ok, RingFile} = riak_ring_manager:find_latest_ringfile(),
    BackupFN = filename:join([RingStateDir, filename:basename(RingFile)++".BAK"]),
    {ok, _} = file:copy(RingFile, BackupFN),
    io:format("Backed up existing ring file to ~p~n", [BackupFN]),
    Ring = riak_ring_manager:read_ringfile(RingFile),
    NewRing = riak_ring:rename_node(Ring, OldNode, NewNode),
    riak_ring_manager:do_write_ringfile(NewRing),
    io:format("New ring file written to ~p~n", 
              [element(2, riak_ring_manager:find_latest_ringfile())]).


format_stats([], Acc) ->
    lists:reverse(Acc);
format_stats([{vnode_gets, V}|T], Acc) ->
    format_stats(T, [io_lib:format("vnode gets : ~p~n", [V])|Acc]);
format_stats([{Stat, V}|T], Acc) ->
    format_stats(T, [io_lib:format("~p : ~p~n", [Stat, V])|Acc]).
    
