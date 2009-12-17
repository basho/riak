#!/usr/bin/env escript
%% -*- mode: erlang -*-
-export([main/1]).

%% External API

main([Name]) ->
    main([Name, "."]);
main([Name, Dest]) ->
    ensure(),
    DestDir = filename:absname(Dest),
    ok = mochiweb_skel:skelcopy(DestDir, Name);
main(_) ->
    usage().

%% Internal API

ensure() ->
    code:add_patha(filename:join(filename:dirname(escript:script_name()),
                                 "../ebin")).

usage() ->
    io:format("usage: ~s name [destdir]~n",
              [filename:basename(escript:script_name())]),
    halt(1).


