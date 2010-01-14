#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("t/hasdefault.proto"), ok, "hasdefault.proto created"),

	Person = #person {
		name = "Nick",
		address = "Mountain View",
		location = #location{region = "Lyon", country = "France"}	
	},
	
    DefaultPerson = #person{
    	name = "Nick",
    	address = "Mountain View",
        phone_number = "+1 (000) 000-0000",
		age = 25,
		location = #location{region = "Lyon", country = "France"}
    },

	Bin = hasdefault_pb:encode_person(Person),
	
	etap:is(hasdefault_pb:decode_person(Bin), DefaultPerson, "Default person encoded and decoded"),

	ok = file:delete("hasdefault_pb.hrl"),
	ok = file:delete("hasdefault_pb.beam"),

    etap:end_tests().
