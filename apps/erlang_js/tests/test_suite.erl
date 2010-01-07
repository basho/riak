-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, driver_tests},
   {module, eval_tests},
   {module, json_tests}].
