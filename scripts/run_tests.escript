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
    crypto:start(),
    setup_mock_ring(),
    Modules = lists:filter(
                fun(M) ->
                        nomatch == re:run(atom_to_list(M), NonTestRe)
                end,
                proplists:get_value(modules, App)),

    start_cover(Modules),
    eunit:test(Modules, [verbose]),
    analyze_cover(Modules);
main(_) ->
    io:format("usage: run_tests.escript RIAK_EBIN_DIRECTORY~n"),
    halt(1).

start_cover(Modules) ->
    {ok, _Cover} = cover:start(),
    io:format("Cover compiling...~n"),
    Compiled = [ M || {ok, M} <- [ cover:compile(
                                     filename:join(["src",atom_to_list(M)]))
                                   || M <- Modules ] ],
    case length(Modules) == length(Compiled) of
        true -> ok;
        false ->
            io:format("Warning: the following modules were not"
                      " cover-compiled:~n   ~p~n", [Compiled])
    end.

analyze_cover(Modules) ->
    io:format("Anazlyzing cover...~n"),
    CoverBase = filename:join(["test", "cover"]),
    ok = filelib:ensure_dir(filename:join([CoverBase, "fake"])),
    Coverages = lists:foldl(
                  fun(M, Acc) ->
                          [analyze_module(CoverBase, M)|Acc]
                  end,
                  [], Modules),
    IndexFilename = filename:join([CoverBase, "index.html"]),
    {ok, Index} = file:open(IndexFilename, [write]),
    {LineTotal, CoverTotal} = 
        lists:foldl(fun({_,_,Lines,Covered}, {LineAcc, CovAcc}) ->
                            {LineAcc+Lines, CovAcc+Covered}
                    end, {0,0}, Coverages),
    file:write(Index,
               "<html><head><title>Riak Coverage</title></head>\n"
               "<body><h1>Riak Coverage</h1><ul>\n"),
    file:write(Index,
               io_lib:format("<h2>Total: ~.2f%</h2>\n",
                             [percentage(CoverTotal, LineTotal)])),
    [ file:write(Index,
                 io_lib:format(
                   "<li><a href=\"~s\">~p</a>: ~.2f%</li>~n",
                   [Filename, Module, percentage(Covered, Lines)]))
      || {Filename, Module, Lines, Covered} <- Coverages ],
    file:write(Index,"</ul></body></html>"),
    file:close(Index),
    io:format("Cover analysis in ~s~n", [IndexFilename]).

analyze_module(CoverBase, Module) ->
    {ok, Filename} = 
        cover:analyze_to_file(
          Module,
          filename:join(CoverBase, atom_to_list(Module)++".COVER.html"),
          [html]),
    Lines = count_lines(Filename, "[[:digit:]]\.\.|"),
    Covered = count_lines(Filename, "[[:space:]]0\.\.|"),
    {filename:basename(Filename), Module, Lines, Lines-Covered}.

count_lines(Filename, Pattern) ->
    {ok, [Lines],_} = io_lib:fread(
                        "~d",
                        os:cmd(io_lib:format("grep -e \"~s\" ~s | wc -l",
                                             [Pattern, Filename]))),
    Lines.

percentage(_, 0) -> 1000.0;
percentage(Part, Total) ->
    (Part/Total)*100.

setup_mock_ring() ->
    Ring0 = lists:foldl(fun(_,R) ->
                               riak_ring:transfer_node(
                                 hd(riak_ring:my_indices(R)),
                                 othernode@otherhost, R) end,
                       riak_ring:fresh(16,node()),[1,2,3,4,5,6]),
    Ring = lists:foldl(fun(_,R) ->
                               riak_ring:transfer_node(
                                 hd(riak_ring:my_indices(R)),
                                 othernode2@otherhost2, R) end,
                       Ring0,[1,2,3,4,5,6]),
    ets:new(nodelocal_ring, [protected, named_table]),
    ets:insert(nodelocal_ring, {ring, Ring}).
