#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(3),
    etap:is(protobuffs:encode(1, 1, uint32), <<8,1>>, "1, 1, unit32"),
    etap:is(protobuffs:encode(2, 1, uint32), <<16,1>>, "2, 1, unit32"),
    etap:is(protobuffs:encode(3, 1, uint32), <<24,1>>, "3, 1, unit32"),
    etap:end_tests().
