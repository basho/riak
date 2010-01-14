#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("t/simple.proto"), ok, "simple.proto compiled"),
	
    Person = #person{
        name = "Nick",
        address = "Mountain View",
        phone_number = "+1 (000) 555-1234",
        age = 25,
		location = #location{region="CA", country="US"}
    },

	Bin = simple_pb:encode_person(Person),

	etap:is(simple_pb:decode_person(Bin), Person, "Encoded and decoded person"),
	
	ok = file:delete("simple_pb.hrl"),
	ok = file:delete("simple_pb.beam"),
	
    etap:end_tests().
