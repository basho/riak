#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, hobbies, locations}).

main(_) ->
    etap:plan(5),
    etap:is(protobuffs_compile:scan_file("t/repeater.proto"), ok, "repeater.proto compiled"),

	Fields1 = [
		{1, "Lyon", string},
		{2, "France", string}
	],
	
	Fields2 = [
		{1, "Reykjavik", string},
		{2, "Iceland", string}
	],
	
	LocationBinData1 = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Fields1]),
	LocationBinData2 = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Fields2]),
	
	Fields3 = [
        {1, "Nick", string},
        {2, "Mountain View", string},
        {3, "+1 (000) 555-1234", string},
        {4, 25, int32},
		{5, "paddling", string},
		{5, "floating", string},
        {6, LocationBinData1, bytes},
		{6, LocationBinData2, bytes}
    ],

	PersonBinData1 = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Fields3]),

    Person = #person{
        name = "Nick",
        address = "Mountain View",
        phone_number = "+1 (000) 555-1234",
        age = 25,
		hobbies = ["paddling", "floating"],
        locations = 
			[#location{region = "Lyon", country = "France"},
			 #location{region = "Reykjavik", country = "Iceland"}]
    },
	
	PersonBinData2 = repeater_pb:encode_person(Person),

	Person1 = repeater_pb:decode_person(PersonBinData1),
	Person2 = repeater_pb:decode_person(PersonBinData2),
	
	etap:is(PersonBinData1, PersonBinData2, "Person binaries match"),

	etap:is(Person, Person1, "Encoded and decoded persons match"),
	
	etap:is(Person1, Person2, "Encoded and decoded persons match"),
	
	Person3 = #person {
		name = "Nick",
        address = "Mountain View",
        phone_number = "+1 (000) 555-1234",
        age = 25,
		hobbies = ["paddling", "floating"]
	},
	
	etap:is(repeater_pb:decode_person(repeater_pb:encode_person(Person3)), Person3, "Encoded and decoded person without locations"),

	ok = file:delete("repeater_pb.hrl"),
	ok = file:delete("repeater_pb.beam"),
	
    etap:end_tests().
