#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name riak_demo@127.0.0.1 -pa ebin -setcookie xyzzy
main([ConfigFile]) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            run_demo(proplists:get_value(riak_nodename, Config),
                     proplists:get_value(riak_hostname, Config),
                     proplists:get_value(riak_cookie, Config));
        Error ->
            io:format("Error: could not open config file: ~p~n", [Error]),
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: riak_demo.escript CONFIG_FILENAME~n"),
    halt(1).

run_demo(NodeName, Hostname, Cookie) when is_atom(NodeName),
                                          is_list(Hostname),
                                          is_atom(Cookie) ->
    Node = list_to_atom(atom_to_list(NodeName)++"@"++Hostname),
    io:format("Attempting to connect to ~p with cookie ~p...~n",
              [Node, Cookie]),
    erlang:set_cookie(Node, Cookie),
    case riak:client_connect(Node) of
        {ok, Client} ->
            io:format("Connected successfully~n"),
            continue_demo(Client);
        Error ->
            io:format("Error: failed to connect to Riak cluster: ~p", [Error])
    end;
run_demo(NodeName, Hostname, Cookie) ->
    io:format("Error: invalid configuration file:~n"),
    if is_list(NodeName) -> ok;
       true -> io:format("  riak_nodename must be an atom (ex. 'riak')~n")
    end,
    if is_list(Hostname) -> ok;
       true -> io:format("  riak_hostname must be a list (ex. \"127.0.0.1\")~n")
    end,
    if is_atom(Cookie) -> ok;
       true -> io:format("  riak_cookie must be an atom (ex. 'riak_cookie')~n")
    end,
    usage().

continue_demo(Client) ->
    io:format("Looking for pre-existing object at {<<\"riak_demo\">>, <<\"demo\">>}...~n"),
    WrittenValue =
        case Client:get(<<"riak_demo">>, <<"demo">>, 1) of
            {error, notfound} ->
                io:format("  No pre-existing object found, creating new~n"),
                demo_write(Client, riak_object:new(<<"riak_demo">>, <<"demo">>, undefined));
            {ok, Object} ->
                io:format("  Pre-existing object found, modifying~n"),
                demo_write(Client, Object);
            Error ->
                io:format("Error: request for {<<\"riak_demo\">>, <<\"demo\">>} failed: ~p~n",
                          [Error]),
                halt(1)
        end,
    demo_read(Client, WrittenValue),
    io:format("SUCCESS~n").

demo_write(Client, Object0) ->
    Now = calendar:universal_time(),
    Object = riak_object:update_value(Object0, Now),
    io:format("Storing object with new value...~n"),
    case Client:put(Object, 1) of
        ok ->
            io:format("  Written successfully~n"),
            Now;
        Error ->
            io:format("Error: write request failed: ~p~n", [Error]),
            halt(1)
    end.

demo_read(Client, WrittenValue) ->
    io:format("Fetching object at {<<\"riak_demo\">>, <<\"demo\">>}...~n"),
    case Client:get(<<"riak_demo">>, <<"demo">>, 1) of
        {ok, Object} ->
            io:format("  Fetched successfully~n"),
            case lists:member(WrittenValue, riak_object:get_values(Object)) of
                true ->
                    io:format("  Object contained correct value~n");
                false ->
                    io:format("Error: Object did not contain correct value~n"),
                    halt(1)
            end;
        Error ->
            io:format("Error: fetch request failed: ~p~n", [Error]),
            halt(1)
    end.
