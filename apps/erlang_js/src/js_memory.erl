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
