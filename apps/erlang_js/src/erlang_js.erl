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

%% @ doc This module is the entry point to start erlang_js as an OTP application.
-module(erlang_js).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).


%% @spec start() -> ok | {error, any()}
%% @doc Starts the erlang_js OTP application and all
%% dependencies. Intended for use with the Erlang VM's
%% -s option
start() ->
    start_deps([sasl]),
    application:start(erlang_js).

%% @private
start(_StartType, _StartArgs) ->
    erlang_js_sup:start_link().

%% @private
stop(_State) ->
    ok.

%% Internal functions
start_deps([]) ->
    ok;
start_deps([App|T]) ->
    case is_running(App, application:which_applications()) of
        false ->
            ok = application:start(App);
        true ->
            ok
    end,
    start_deps(T).

is_running(_App, []) ->
    false;
is_running(App, [{App, _, _}|_]) ->
    true;
is_running(App, [_|T]) ->
    is_running(App, T).
