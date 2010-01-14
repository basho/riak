#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -noshell

main(_) ->
    etap:plan(18),
    Tests = [
        {8, uint32},
        {16, uint32},
        {24, uint32},
        {1, fixed32},
        {1, enum},
        {200, enum},
        {9933, int64},
        {-391, sint64},
        {5, fixed32},
        {-5, sfixed32},
        {30, fixed64},
        {500, sfixed64},
        {"Whirlwind tickles.", string, <<"Whirlwind tickles.">>},
        {"", string, <<>>},
        {<<"It's a secret to everyone.">>, string},
        {<<4,8,15,16,23,42>>, bytes},
        {3.141592025756836, float},
        {1.00000000000000022204460492503130808472633361816406, double}
    ],
    lists:foreach(
        fun(Test) ->
            {Value, Type, Expected} = case Test of
                {A, B} -> {A, B, A};
                _ -> Test
            end,
            Decode = protobuffs:decode(iolist_to_binary(protobuffs:encode(1, Value, Type)), Type),
            etap:is(
                protobuffs:decode(iolist_to_binary(protobuffs:encode(1, Value, Type)), Type),
                {{1, Expected}, <<>>},
                "encode/decode test -- random " ++ atom_to_list(Type)
            )
        end,
        Tests
    ),
    etap:end_tests().
