#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name riak_test@127.0.0.1
main([Ebin]) ->
    code:add_path(Ebin),
    code:add_paths(filelib:wildcard("deps/*/ebin", Ebin)),
    code:add_paths(filelib:wildcard("deps/*/deps/*/ebin", Ebin)),
    eunit:test({application, riak}, [verbose]);
main(_) ->
    io:format("usage: run_tests.escript RIAK_EBIN_DIRECTORY~n"),
    halt(1).
