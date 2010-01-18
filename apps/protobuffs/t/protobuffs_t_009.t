#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("t/hasdefault.proto"), ok, "hasdefault.proto created"),

	case catch hasdefault_pb:encode_person(#person{}) of
		{'EXIT', {error, {required_field_is_undefined,1,string}}} ->
			etap:ok(true, "Required field is undefined");
		_ ->
			etap:ok(false, "Required field is undefined")
	end,
	
	ok = file:delete("hasdefault_pb.hrl"),
	ok = file:delete("hasdefault_pb.beam"),

    etap:end_tests().
