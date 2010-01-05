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
