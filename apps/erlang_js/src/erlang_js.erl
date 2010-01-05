-module(erlang_js).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  start_deps([sasl]),
  application:start(erlang_js).

start(_StartType, _StartArgs) ->
  erlang_js_sup:start_link().

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
