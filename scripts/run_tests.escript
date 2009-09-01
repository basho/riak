#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name riak_test@127.0.0.1
main([Ebin]) ->
    code:add_path(Ebin),
    code:add_paths(filelib:wildcard("deps/*/ebin", Ebin)),
    code:add_paths(filelib:wildcard("deps/*/deps/*/ebin", Ebin)),

    {ok, [{application, riak, App}]} =
        file:consult(filename:join([Ebin, "riak.app"])),
    {ok, NonTestRe} = re:compile("_tests$"),
    Modules = lists:filter(
                fun(M) ->
                        nomatch == re:run(atom_to_list(M), NonTestRe)
                end,
                proplists:get_value(modules, App)),
                        
    eunit:test(Modules, [verbose]);
main(_) ->
    io:format("usage: run_tests.escript RIAK_EBIN_DIRECTORY~n"),
    halt(1).
