%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc This module implements a basic cache. This cache is used to store
%% files used to initialize each Javascript context. This is helpful because
%% it prevents erlang_js from accessing the filesystem more than necessary.

-module(js_cache).

-behaviour(gen_server).

%% API
-export([start_link/0, store/2, delete/1, fetch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cache=gb_trees:empty()}).

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec store(any(), any() -> ok
%% @doc Store a key/value pair
store(Key, Value) ->
    gen_server:cast(?SERVER, {store, Key, Value}).

%% @spec delete(any()) -> ok
%% @doc Deletes a key/value pair, if present, from the cache.
delete(Key) ->
    gen_server:cast(?SERVER, {delete, Key}).

%% @spec fetch(any()) -> any() | not_found
%% @doc Retrieves a key/value pair from the cache. If the key
%% is not in the cache, the atom 'not_found' is returned.
fetch(Key) ->
    gen_server:call(?SERVER, {fetch, Key}).

init([]) ->
    {ok, #state{}}.

% @private
handle_call({fetch, Key}, _From, #state{cache=Cache}=State) ->
    Result = case gb_trees:lookup(Key, Cache) of
                 {value, Value} ->
                     Value;
                 Other ->
                     Other
             end,
    {reply, Result, State};

% @private
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

% @private
handle_cast({store, Key, Value}, #state{cache=Cache}=State) ->
    {noreply, State#state{cache=gb_trees:enter(Key, Value, Cache)}};

% @private
handle_cast({delete, Key}, #state{cache=Cache}=State) ->
    {noreply, State#state{cache=gb_trees:delete(Key, Cache)}};

% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

% @private
handle_info(_Info, State) ->
    {noreply, State}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
