-module(driver_tests).

-include_lib("eunit/include/eunit.hrl").

load_test_() ->
  [{setup, fun test_util:port_setup/0,
    fun test_util:port_teardown/1,
    [fun() ->
         P = test_util:get_thing(),
         ?assert(is_port(P)),
         erlang:unlink(P) end]}].

destroy_test_() ->
  [{setup, fun test_util:port_setup/0,
    fun test_util:null_teardown/1,
    [fun() ->
         P = test_util:get_thing(),
         ?assertMatch(true, js_driver:destroy(P)),
         ?assertError(badarg, js:define(P, <<"var x = 100;">>)),
         erlang:unlink(P) end]}].
