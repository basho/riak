%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc A protcol buffers encoding and decoding module.
-module(protobuffs).
-export([encode/3, read_field_num_and_wire_type/1, decode/2, decode_value/3]).
-compile(export_all).

-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).

%% @spec encode(FieldID, Value, Type) -> Result
%%       FieldID = integer()
%%       Value = any()
%%       Type = bool | enum | int32 | uint32 | int64 | unit64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = list()
%% @doc Encode an Erlang data structure into a Protocol Buffers value.
encode(FieldID, Value, Type) ->
    iolist_to_binary(encode_internal(FieldID, Value, Type)).
    
%% @hidden
encode_internal(FieldID, false, bool) ->
    encode_internal(FieldID, 0, int32);
encode_internal(FieldID, true, bool) ->
    encode_internal(FieldID, 1, int32);
encode_internal(FieldID, Integer, enum) ->
    encode_internal(FieldID, Integer, uint32);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer < 0 ->
    encode_internal(FieldID, Integer, int64);
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_internal(FieldID, Integer + (1 bsl 64), uint64);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint32) when Integer band 16#ffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);    
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint64) when Integer band 16#ffffffffffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, bool) when Integer band 1 =:= 1 ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, sint32) when Integer >= -16#80000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint32) when Integer >= 0, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, sint64) when Integer >= 0, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, fixed32) when Integer band 16#ffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, sfixed32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, fixed64) when Integer band 16#ffffffffffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, Integer, sfixed64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, String, string) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), string);
encode_internal(FieldID, String, string) when is_binary(String) ->
    encode_internal(FieldID, String, bytes);
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), bytes);
encode_internal(FieldID, Bytes, bytes) when is_binary(Bytes) ->
    [encode_field_tag(FieldID, ?TYPE_STRING), encode_varint(size(Bytes)), Bytes];
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), bytes);
encode_internal(FieldID, Float, float) when is_integer(Float) ->
    encode_internal(FieldID, Float + 0.0, float);
encode_internal(FieldID, Float, float) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Float:32/little-float>>];
encode_internal(FieldID, Float, double) when is_integer(Float) ->
    encode_internal(FieldID, Float + 0.0, double);
encode_internal(FieldID, Float, double) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Float:64/little-float>>].

read_field_num_and_wire_type(Bytes) ->
    {Tag, Rest} = decode_varint(Bytes),
    FieldID = Tag bsr 3,
    WireType = Tag band 7,
    {{FieldID, WireType}, Rest}.
    
%% @spec decode(Bytes, ExpectedType) -> Result
%%       Bytes = binary()
%%       ExpectedType = bool | enum | int32 | uint32 | int64 | unit64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = {{integer(), any()}, binary()}
decode(Bytes, ExpectedType) ->
    {{FieldID, WireType}, Rest} = read_field_num_and_wire_type(Bytes),
    {Value, Rest1} = decode_value(Rest, WireType, ExpectedType),
    {{FieldID, Value}, Rest1}.

%% @hidden
decode_value(Bytes, ?TYPE_VARINT, ExpectedType) ->
    {Value, Rest} = decode_varint(Bytes),
    {typecast(Value, ExpectedType), Rest};
decode_value(Bytes, ?TYPE_STRING, ExpectedType) when ExpectedType =:= string; ExpectedType =:= bytes ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(<<Value:64/little-unsigned-integer, Rest/binary>>, ?TYPE_64BIT, fixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, _:32, Rest/binary>>, ?TYPE_64BIT, fixed32) ->
    {Value, Rest};
decode_value(<<Value:64/little-signed-integer, Rest/binary>>, ?TYPE_64BIT, sfixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, _:32, Rest/binary>>, ?TYPE_64BIT, sfixed32) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= fixed32; Type =:= fixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= sfixed32; Type =:= sfixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-float, Rest/binary>>, ?TYPE_32BIT, float) ->
    {Value + 0.0, Rest};
decode_value(<<Value:64/little-float, Rest/binary>>, ?TYPE_64BIT, double) ->
    {Value + 0.0, Rest};
decode_value(_, WireType, ExpectedType) ->
    exit({error, {unexpected_value, WireType, ExpectedType}}).

%% @hidden
typecast(Value, SignedType) when SignedType =:= int32; SignedType =:= int64 ->
    if
        Value band 16#8000000000000000 =/= 0 -> Value - 16#10000000000000000;
        true -> Value
    end;
typecast(Value, SignedType) when SignedType =:= sint32; SignedType =:= sint64 ->
    (Value bsr 1) bxor (-(Value band 1));
typecast(Value, _) ->
    Value.

%% @hidden
encode_field_tag(FieldID, FieldType) when FieldID band 16#3fffffff =:= FieldID ->
    encode_varint((FieldID bsl 3) bor FieldType).

%% @hidden
encode_varint_field(FieldID, Integer) ->
    [encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)].

%% @hidden
encode_varint(I) ->
    encode_varint(I, []).

%% @hidden
encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).

%% @hidden
decode_varint(Bytes) ->
    decode_varint(Bytes, []).
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).
