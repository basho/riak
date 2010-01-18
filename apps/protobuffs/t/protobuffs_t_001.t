#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(8),
    etap_can:loaded_ok(protobuffs, "module 'protobuffs' loaded"),
    etap_can:can_ok(protobuffs, encode),
    etap_can:can_ok(protobuffs, encode, 3),
    etap_can:can_ok(protobuffs, decode),
    etap_can:can_ok(protobuffs, decode, 2),
    etap_can:loaded_ok(protobuffs_compile, "module 'protobuffs_compile' loaded"),
    etap_can:can_ok(protobuffs_compile, scan_file),
    etap_can:can_ok(protobuffs_compile, scan_file, 1),
    etap:end_tests().
