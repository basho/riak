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
-module(riak_core_ring_events).

-behaviour(gen_event).

%% API
-export([start_link/0,
         add_handler/2,
         add_sup_handler/2,
         add_callback/1,
         add_sup_callback/1,
         ring_update/1,
         force_update/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, { callback }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

add_sup_handler(Handler, Args) ->
    gen_event:add_sup_handler(?MODULE, Handler, Args).

add_callback(Fn) when is_function(Fn) ->
    gen_event:add_handler(?MODULE, {?MODULE, make_ref()}, [Fn]).

add_sup_callback(Fn) when is_function(Fn) ->
    gen_event:add_sup_handler(?MODULE, {?MODULE, make_ref()}, [Fn]).

force_update() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    gen_event:notify(?MODULE, {ring_update, Ring}).

ring_update(Ring) ->
    gen_event:notify(?MODULE, {ring_update, Ring}).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([Fn]) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Fn(Ring),
    {ok, #state { callback = Fn }}.

handle_event({ring_update, Ring}, State) ->
    (State#state.callback)(Ring),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

