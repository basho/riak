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

-module(js_memory).

-define(COUNT, 1000000).

-export([stress/1]).

stress(new) ->
    Start = erlang:memory(total),
    do(new, ?COUNT),
    display(end_test() - Start);

stress(error) ->
    Start = erlang:memory(total),
    do(error, ?COUNT),
    display(end_test() - Start).

%% Internal functions
do(error, 0) ->
    ok;
do(error, Count) ->
    show_count(Count),
    {ok, P} = js_driver:new(),
    {error, _Error} = js:define(P, <<"function foo(;">>),
    js_driver:destroy(P),
    do(error, Count - 1);

do(new, 0) ->
    ok;
do(new, Count) ->
    show_count(Count),
    {ok, P} = js_driver:new(),
    js_driver:destroy(P),
    do(new, Count - 1).

end_test() ->
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    erlang:memory(total).

display(Memory) ->
    io:format("Used ~p bytes during test.~n", [Memory]).

show_count(Count) ->
    if
        (?COUNT - Count) rem 1000 == 0 ->
            io:format("~p~n", [Count]);
        true ->
            ok
    end.
