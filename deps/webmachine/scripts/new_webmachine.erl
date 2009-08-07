#!/usr/bin/env escript
%% -*- mode: erlang -*-
-export([main/1]).

%% External API

main([Name]) ->
    case Name of
        "." ++ _Rest -> usage();
        "~" ++ _Rest -> usage();
        "/" ++ _Rest -> usage();
        _Any         -> main([Name, "."])
    end;
main([Name, Dest]) ->
    ensure(),
    DestDir = filename:absname(Dest),
    ok = webmachine_skel:skelcopy(DestDir, Name);
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


