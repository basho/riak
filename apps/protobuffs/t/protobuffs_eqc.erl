%%% erlc -o ebin t/*.erl -pa ebin
%%% erl -name eqc_pb -pa ebin
%%% eqc_gen:sample(protobuffs_eqc:protobuff_data()).
%%% eqc:quickcheck(protobuffs_eqc:prop_encode_decode1()).
%%% eqc:quickcheck(protobuffs_eqc:prop_encode_decode2()).
%%% eqc:quickcheck(protobuffs_eqc:prop_encode_decode3()).
%%%
%%% File    : protobuffs_eqc.erl
%%% Author  :  <thomas@QUVIQ-THOMAS>
%%% Description : QuickCheck specification used in class for
%%%               protobuffs-0.2
%%% Created : 27 Apr 2009 by  <thomas@QUVIQ-THOMAS>
-module(protobuffs_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(Mach_Eps, 1.1920928955078125e-7).
-define(NotYetImplemented(Cond,Prop), ?IMPLIES(not (Cond),Prop)).

%% Properties

prop_encode_decode1() ->
    ?FORALL({FieldNum,Data,Type}, protobuff_data(),
        collect(Type,
            begin
                {{N, RData}, <<>>} = protobuffs:decode(protobuffs:encode(FieldNum, Data, Type), Type),
                FieldNum =:= N andalso 
                (compare(Data, RData) orelse foreign_type(Type, Data, RData))  
            end)).
    
prop_encode_decode2() ->
    ?FORALL({FieldNum,Data,Type}, fault_rate(5,10,protobuff_data()),
        case catch protobuffs:encode(FieldNum,Data,Type) of
            {'EXIT', _} ->
                not in_range(Data,Type);
            Bin ->
                {{N, RData}, <<>>} = protobuffs:decode(Bin, Type),
                in_range(Data,Type) andalso
                FieldNum =:= N andalso
                (compare(Data,RData) orelse foreign_type(Type,Data,RData))
        end).

prop_encode_decode3() ->
    ?FORALL(Many, protobuff_many(),
        begin
            Sorted = lists:keysort(1, Many),
            IOList = [protobuffs:encode(FNum,Data,Type) || {FNum,Data,Type} <- Sorted],
            Bin = iolist_to_binary(IOList),
            {Decoded0,_} = lists:foldl(
                fun({_,_,Type}, {Acc, Bin1}) ->
                    {Val, Rest} = protobuffs:decode(Bin1, Type),
                    {[Val|Acc], Rest}
                end, {[],Bin}, Sorted),
            Decoded = lists:reverse(Decoded0),
            lists:foldl(
                fun (_, false) -> false;
                    (I, true) ->                        
                        {FNum1, Data1, Type1} = lists:nth(I, Sorted),
                        {FNum2, Data2} = lists:nth(I, Decoded),
                        (FNum1 =:= FNum2 andalso
                        (compare(Data1,Data2) orelse foreign_type(Type1,Data1,Data2)))
                end, true, lists:seq(1, length(Sorted))) 
        end).

prop_encode_decode4() ->
    ?FORALL({ProtoName, Msgs}, protobuff_msgs(),   
        begin
            code:delete(list_to_atom(ProtoName)),
            code:purge(list_to_atom(ProtoName)),
            protobuffs_compile:output(ProtoName, Msgs),
            ?FORALL(Set, protobuff_set(Defs),
                begin
                    Record = build_record_from_defs(MsgName, Defs, Set),
                    case catch apply(list_to_atom(ProtoName), list_to_atom("encode_" ++ MsgName), [Record]) of
                        {'EXIT', {error, {required_field_is_undefined, FNum, _}}} ->
                            {value, {_, required, _, _, _, _}} = lists:keysearch(FNum, 1, Defs),
                            true;
                        Bin when is_binary(Bin) ->
                            Record1 = apply(list_to_atom(ProtoName), list_to_atom("decode_" ++ MsgName), [Bin]),
                            lists:foldl(
                                fun ({A,B}, true) ->
                                        io:format("compare ~p and ~p~n", [A,B]),
                                        compare(A,B);
                                    (_, false) ->
                                        false
                                end, true, lists:zip(tuple_to_list(Record), tuple_to_list(Record1)))
                    end
                end)
        end).
    
%% Data generators

protobuff_msgs() ->
    {string(), list({msg_name(), protobuff_defs()})}.
    
protobuff_many() ->
    list(protobuff_data()).
    
protobuff_set(Defs) ->
    [begin
        {FNum, Name, field_value(Tag, Type)}
     end || {FNum, Tag, Type, Name, _, _Default} <- Defs].
    
protobuff_data() ->
    fault({field_num(), int(80), oneof([int32,uint32,int64,uint64,sint32,sint64])},
        oneof([
            {field_num(), int(32), int32},
            {field_num(), uint(32), uint32},
            {field_num(), int(64), int64},
            {field_num(), uint(64), uint64},
            {field_num(), bool(), bool},
            {field_num(), sint(32), sint32},
            {field_num(), sint(64), sint64},
            {field_num(), real(), float},
            {field_num(), real(), double},
            {field_num(), list(char()), string},
            {field_num(), binary(), bytes}
        ])
    ).
    
protobuff_defs() ->
    ?SUCHTHAT(D,orderedlist(protobuff_def()),length(D) > 0).
    
protobuff_def() ->
    oneof([
        {field_num(), tag(), "int32",   field_name(), number, oneof([none, int(32)])},
        {field_num(), tag(), "uint32",  field_name(), number, oneof([none, uint(32)])},
        {field_num(), tag(), "int64",   field_name(), number, oneof([none, int(64)])},
        {field_num(), tag(), "uint64",  field_name(), number, oneof([none, uint(64)])},
        {field_num(), tag(), "bool",    field_name(), number, oneof([none, bool()])},
        {field_num(), tag(), "sint32",  field_name(), number, oneof([none, sint(32)])},
        {field_num(), tag(), "sint64",  field_name(), number, oneof([none, sint(64)])},
        {field_num(), tag(), "float",   field_name(), number, oneof([none, real()])},
        {field_num(), tag(), "double",  field_name(), number, oneof([none, real()])},
        {field_num(), tag(), "string",  field_name(), number, oneof([none, list(char())])},
        {field_num(), tag(), "bytes",   field_name(), number, oneof([none, binary()])}
    ]).
    
field_value(repeated, "int32") -> oneof([undefined, list(int(32))]);
field_value(repeated, "uint32") -> oneof([undefined, list(uint(32))]);
field_value(repeated, "int64") -> oneof([undefined, list(int(64))]);
field_value(repeated, "uint64") -> oneof([undefined, list(uint(64))]);
field_value(repeated, "bool") -> oneof([undefined, list(bool())]);
field_value(repeated, "sint32") -> oneof([undefined, list(sint(32))]);
field_value(repeated, "sint64") -> oneof([undefined, list(sint(64))]);
field_value(repeated, "float") -> oneof([undefined, list(real())]);
field_value(repeated, "double") -> oneof([undefined, list(real())]);
field_value(repeated, "string") -> oneof([undefined, list(list(char()))]);
field_value(repeated, "bytes") -> oneof([undefined, list(binary())]);
field_value(_, "int32") -> oneof([undefined, int(32)]);
field_value(_, "uint32") -> oneof([undefined, uint(32)]);
field_value(_, "int64") -> oneof([undefined, int(64)]);
field_value(_, "uint64") -> oneof([undefined, uint(64)]);
field_value(_, "bool") -> oneof([undefined, bool()]);
field_value(_, "sint32") -> oneof([undefined, sint(32)]);
field_value(_, "sint64") -> oneof([undefined, sint(64)]);
field_value(_, "float") -> oneof([undefined, real()]);
field_value(_, "double") -> oneof([undefined, real()]);
field_value(_, "string") -> oneof([undefined, list(char())]);
field_value(_, "bytes") -> oneof([undefined, binary()]).
    
field_num() ->
    ?SUCHTHAT(N,nat(),N>0).
    
tag() ->
    oneof([optional, required, repeated]).
    
field_name() ->
   ?SUCHTHAT(N,string(),length(N)>0).
    
msg_name() ->
    ?SUCHTHAT(N,string(),length(N)>0).
    
string() ->
    list(oneof([choose(97,122), choose(65,90)])).
    
%% Internal Functions

foreign_type(bool,false,0) ->
    true;
foreign_type(bool,true,1) ->
    true;
foreign_type(_,_,_) ->
    false.

prop_varint() ->
    ?FORALL(Base,oneof([32,64]),
        ?FORALL(I,int(Base),
            begin
                {Bits,Data} = decompose(protobuffs:encode_varint(I)),
                right_bits(Bits) andalso 
                concatenate(Data) == I
            end
        )
    ).

build_record_from_defs(MsgName, Defs, Set) ->
    lists:foldl(
        fun({FNum, _Tag, _Type, _Name, _, _Default}, Acc) ->
            {value, {_,_,Value}} = lists:keysearch(FNum, 1, Set),
            erlang:append_element(Acc, Value)
        end, {list_to_atom(MsgName)}, Defs).

%% Bits are in reverse order: First bit should be zero, rest should be 1 
right_bits([0|Rest]) ->
    lists:all(fun(B) -> B==1 end,Rest).

int(Base) ->
    ?LET(I,uint(Base),
        begin 
            << N:Base/signed >> = <<I:Base>>, N 
        end
    ).

uint(Base) ->
    oneof([ choose(0,exp(B)) || B<-lists:seq(1,Base)]).

sint(Base) ->
    int(Base).

exp(1) ->
    2;
exp(N) ->
    2*exp(N-1).

decompose(<<Bit:1,Data:7>>) ->
    {[Bit],<<Data:7>>};
decompose(<<Bit:1,Data:7,Rest/binary>>) ->
    {Bs,Ds} = decompose(Rest),
    {Bs++[Bit],<<Ds/bitstring,Data:7>>}.

concatenate(Bin) ->
    S = bit_size(Bin),
    << N:S >> = Bin,
    N.

in_range(Int,int32) ->
    fitbits(Int,32);
in_range(Int,sint32) ->
    fitbits(abs(Int),31);
in_range(Int,uint32) ->
    fitbits(Int,32);
in_range(Int,int64) ->
    fitbits(Int,64);
in_range(Int,sint64) ->
    fitbits(abs(Int),63);
in_range(Int,uint64) ->
    fitbits(Int,64);
in_range(Float,float) ->
    fitbits(Float,32);
in_range(Float,double) ->
    fitbits(Float,64);
in_range(_,string) ->
    true;
in_range(_,bytes) ->
    true;
in_range(false,bool) ->
    true;
in_range(true,bool) ->
    true.

compare(Float1, Float2) when is_float(Float1), is_float(Float2) ->
    (abs(Float1 - Float2) =< ?Mach_Eps);
compare(String, Binary) when is_list(String), is_binary(Binary) ->
    String =:= binary_to_list(Binary);
compare(A,A) -> true;
compare(_,_) -> false.

fitbits(Float,32) when is_float(Float) -> true;
fitbits(Float,64) when is_float(Float) -> true;
fitbits(Int,Bits) ->
    RestBits = 80-Bits,
    << NoFit:RestBits, _:Bits >> = <<Int:80>>,
    NoFit == 0.
