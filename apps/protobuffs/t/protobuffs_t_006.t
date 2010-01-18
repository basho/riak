#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(3),
    etap:is(protobuffs_compile:scan_file("t/simple.proto"), ok, "simple.proto compiled"),

	Fields1 = [
		{1, "California", string},
		{2, "USA", string}
	],

    LocationBinData = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Fields1]),

    Fields2 = [
        {1, "Nick", string},
        {2, "Mountain View", string},
        {3, "+1 (000) 555-1234", string},
        {4, 25, int32},
        {5, LocationBinData, bytes}
    ],

	PersonBinData = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Fields2]),

	Location = #location{region="California", country="USA"},
	
    Person = #person{
        name = "Nick",
        address = "Mountain View",
        phone_number = "+1 (000) 555-1234",
        age = 25,
        location = Location
    },

	etap:is(simple_pb:encode_location(Location), LocationBinData, "Encoded locations match"),
	etap:is(simple_pb:encode_person(Person), PersonBinData, "Encoded persons match"),

	ok = file:delete("simple_pb.hrl"),
	ok = file:delete("simple_pb.beam"),
	
    etap:end_tests().
