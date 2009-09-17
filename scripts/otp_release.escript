#!/usr/bin/env escript
%% -*- erlang -*-
%% This script checks the Erlang/OTP release version, and
%% exits with a failure code if that version is less than 13.
main([]) ->
    [$R|Vs] = erlang:system_info(otp_release),
    {ok, [V], _} = io_lib:fread("~d", Vs),
    if 13 =< V ->
            ok;
       true ->
            io:format("Error: Riak requires OTP release >= R13B"
                      " (detected R~b)~n", [V]),
            halt(1)
    end.
