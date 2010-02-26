%% -------------------------------------------------------------------
%%
%% riak_core: Core Riak Application
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

-module(app_helper).

-export([get_env/1, get_env/2, get_env/3]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @spec get_env(App :: atom()) -> [{Key :: atom(), Value :: term()}]
%% @doc Retrieve all Key/Value pairs in the env for the specified app.
get_env(App) ->
    application:get_all_env(App).

%% @spec get_env(App :: atom(), Key :: atom()) -> term()
%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
get_env(App, Key) ->
    get_env(App, Key, undefined).

%% @spec get_env(App :: atom(), Key :: atom(), Default :: term()) -> term()
%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	{ok, Value} ->
            Value;
        _ ->
            Default
    end.
