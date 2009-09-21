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

%% @doc Riak: A lightweight, decentralized key-value store.
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies, Inc.  All Rights Reserved.

-module(riak).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@basho.com>').
-author('Bryan Fink <bryan@basho.com>').
-export([start/0, start/1, stop/0, stop/1]).
-export([get_app_env/1,get_app_env/2]).
-export([client_connect/1,local_client/0]).

-include_lib("eunit/include/eunit.hrl").

%% @spec start([ConfigPath :: list()]) -> ok
%% @doc Start the riak server.
%%      ConfigPath specifies the location of the riak configuration file.
start([ConfigPath]) ->
    application:set_env(riak, configpath, ConfigPath),
    start().
    
%% @spec start() -> ok
%% @doc Start the riak server.
start() ->
    %% force full GC sweeps more often to resolve memory usage issues, mostly
    %% on Linux.  Using erlang:system_flag() is a relatively blunt way of 
    %% solving this (the value can be specified per-process with spawn_opt,
    %% but this works and doesn't have a noticeable impact on performance.
    erlang:system_flag(fullsweep_after, 20),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(riak, permanent).

%% @spec stop() -> ok
%% @doc Stop the riak application and the calling process.
stop() -> stop("riak stop requested").
stop(Reason) ->
    % we never do an application:stop because that makes it very hard
    %  to really halt the runtime, which is what we need here.
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    init:stop().    

%% @spec get_app_env(Opt :: atom()) -> term()
%% @doc The official way to get the values set in riak's configuration file.
%%      Will return the undefined atom if that option is unset.
get_app_env(Opt) -> get_app_env(Opt, undefined).

%% @spec get_app_env(Opt :: atom(), Default :: term()) -> term()
%% @doc The official way to get the values set in riak's configuration file.
%%      Will return Default if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(riak, Opt) of
	{ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
	    {ok, [[Val | _]]} -> Val;
	    error       -> Default
        end
    end.

%% @spec local_client() -> {ok, Client :: riak_client()}
%% @doc When you want a client for use on a running Riak node.
local_client() -> client_connect(node()).


%% @spec client_connect(Node :: node())
%%        -> {ok, Client :: riak_client()} | {error, timeout}
%% @doc The usual way to get a client.  Timeout often means either a bad
%%      cookie or a poorly-connected distributed erlang network.
client_connect(Node) -> 
    % Make sure we can reach this node...
    case net_adm:ping(Node) of
        pang -> throw({could_not_reach_node, Node});
        pong -> ok
    end,
        
    % Return the newly created node...
    {ok, riak_client:new(Node, riak_util:mkclientid(Node))}.



%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.