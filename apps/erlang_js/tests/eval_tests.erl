-module(eval_tests).

-include_lib("eunit/include/eunit.hrl").

var_test_() ->
    [{setup, fun test_util:port_setup/0,
      fun test_util:port_teardown/1,
      [fun() ->
               P = test_util:get_thing(),
               ?assertMatch(ok, js:define(P, <<"var x = 100;">>)),
               ?assertMatch({ok, 100}, js:eval(P, <<"x;">>)),
               erlang:unlink(P) end]}].

function_test_() ->
    [{setup, fun test_util:port_setup/0,
      fun test_util:port_teardown/1,
      [fun() ->
               P = test_util:get_thing(),
               ?assertMatch(ok, js:define(P, <<"function add_two(x, y) { return x + y; };">>)),
               ?assertMatch({ok, 95}, js:call(P, <<"add_two">>, [85, 10])),
               ?assertMatch({ok, <<"testing123">>}, js:call(P, <<"add_two">>, [<<"testing">>, <<"123">>])),
               erlang:unlink(P) end,
       fun() ->
               P = test_util:get_thing(),
               ?assertMatch(ok, js:define(P, <<"var f = function(x, y) { return y - x; };">>)),
               ?assertMatch({ok, 75}, js:call(P, <<"f">>, [10, 85])),
               erlang:unlink(P) end,
       fun() ->
               P = test_util:get_thing(),
               ?assertMatch(ok, js:define(P, <<"function get_first(data) { return data[\"first\"]; };">>)),
               Data = {struct, [{<<"first">>, <<"abc">>}]},
               ?assertMatch({ok, <<"abc">>}, js:call(P, <<"get_first">>, [Data])),
               erlang:unlink(P) end]}].

json_test_() ->
  [fun() ->
       Struct = {struct, [{<<"test">>, <<"1">>}]},
       ?assertMatch(Struct, js_mochijson2:decode(js_mochijson2:encode(Struct))) end].

error_test_() ->
    [{setup, fun test_util:port_setup/0,
      fun test_util:port_teardown/1,
      [fun() ->
               P = test_util:get_thing(),
               {error, ErrorDesc} = js:define(P, <<"functoin foo(x, y) { return true; };">>),
               ?assert(verify_error(ErrorDesc)),
               erlang:unlink(P) end,
       fun() ->
               P = test_util:get_thing(),
               ?assertMatch(ok, js:define(P, <<"function foo(x, y) { return true; };">>)),
               {error, ErrorDesc} = js:eval(P, <<"foo(100, 200,);">>),
               ?assert(verify_error(ErrorDesc)),
               erlang:unlink(P) end]}].

%% Internal functions
verify_error([{<<"lineno">>, LineNo},
              {<<"message">>, Msg},
              {<<"source">>, Source}]) when is_number(LineNo),
                                            is_binary(Msg),
                                            is_binary(Source) ->
    true;
verify_error(_) ->
    false.
