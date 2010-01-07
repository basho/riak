-module(json_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    [?assertMatch(ok, js_json:test())].

nested_proplists_test() ->
    [?assertMatch(<<"{\"foo\":{\"bar\":1}}">>, list_to_binary(js_json:encode([{<<"foo">>, [{<<"bar">>, 1}]}]))),
     ?assertMatch(<<"[123,\"abc\",{\"foo\":\"bar\"}]">>, list_to_binary(js_json:encode([123, <<"abc">>, [{<<"foo">>, <<"bar">>}]])))].
