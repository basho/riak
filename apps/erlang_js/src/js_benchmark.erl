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

-module(js_benchmark).

-define(COUNTS, [1000, 10000, 100000]).

-export([run/0]).

run() ->
    application:start(erlang_js),
    {ok, Ctx} = js_driver:new(),
    js:define(Ctx, "function add(x, y) { return x + y; }", []),
    Result = [time_calls(Ctx, Count) || Count <- ?COUNTS],
    js_driver:destroy(Ctx),
    Result.

time_calls(Ctx, Count) ->
    io:format("Starting: ~p~n", [Count]),
    Start = erlang:now(),
    do_calls(Ctx, Count),
    timer:now_diff(erlang:now(), Start) / Count.
do_calls(_Ctx, 0) ->
    ok;
do_calls(Ctx, Count) ->
    CorrectResult = Count * 2,
    {ok, CorrectResult} = js:call(Ctx, "add", [Count, Count]),
    do_calls(Ctx, Count - 1).
