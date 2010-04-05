%% -------------------------------------------------------------------
%%
%% riak_kv_pb: protocol buffer utility functions
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc protocol buffer utilities

-module(riak_kv_pb).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("riakclient_pb.hrl").
-compile([export_all]).

%% Names of riak_object metadata fields
-define(MD_CTYPE,    <<"content-type">>).
-define(MD_CHARSET,  <<"charset">>).
-define(MD_ENCODING, <<"content-encoding">>).
-define(MD_VTAG,     <<"X-Riak-VTag">>).
-define(MD_LINKS,    <<"Links">>).
-define(MD_LASTMOD,  <<"X-Riak-Last-Modified">>).
-define(MD_USERMETA, <<"X-Riak-Meta">>).

%% Names of PB fields in bucket properties
-define(PB_PROPS,   <<"props">>).
-define(PB_KEYS,    <<"keys">>).
-define(PB_LINKFUN, <<"linkfun">>).
-define(PB_MOD,     <<"mod">>).
-define(PB_FUN,     <<"fun">>).
-define(PB_CHASH,   <<"chash_keyfun">>).
-define(PB_JSFUN,    <<"jsfun">>).
-define(PB_JSANON,   <<"jsanon">>).
-define(PB_JSBUCKET, <<"bucket">>).
-define(PB_JSKEY,    <<"key">>).
-define(PB_ALLOW_MULT, <<"allow_mult">>).

%% Create an iolist of msg code and protocol buffer message
encode(Msg) when is_atom(Msg) ->
    [msg_code(Msg)];
encode(Msg) when is_tuple(Msg) ->
    MsgType = element(1, Msg),
    [msg_code(MsgType) | riakclient_pb:iolist(MsgType, Msg)].
 
%% Decode a protocol buffer message given its type - if no bytes
%% return the atom for the message code
decode(MsgCode, <<>>) ->
    msg_type(MsgCode);
decode(MsgCode, MsgData) ->
    riakclient_pb:decode(msg_type(MsgCode), MsgData).

msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbhelloreq;
msg_type(2) -> rpbhelloresp;
msg_type(3) -> rpbpingreq;
msg_type(4) -> rpbpingresp;
msg_type(5) -> rpbgetreq;
msg_type(6) -> rpbgetresp;
msg_type(7) -> rpbputreq;
msg_type(8) -> rpbputresp;
msg_type(9) -> rpbdelreq;
msg_type(10) -> rpbdelresp;
msg_type(11) -> rpbgetbucketpropsreq;
msg_type(12) -> rpbgetbucketpropsresp;
msg_type(13) -> rpbsetbucketpropsreq;
msg_type(14) -> rpbsetbucketpropsresp;
msg_type(15) -> rpblistbucketsreq;
msg_type(16) -> rpblistbucketsresp;
msg_type(17) -> rpblistkeysreq;
msg_type(18) -> rpblistkeysresp;
msg_type(19) -> rpbmapredreq;
msg_type(20) -> rpbmapredresp;
msg_type(_) -> undefined.
    
msg_code(rpberrorresp) -> 0;
msg_code(rpbhelloreq)  -> 1;
msg_code(rpbhelloresp) -> 2;
msg_code(rpbpingreq)   -> 3;
msg_code(rpbpingresp)  -> 4;
msg_code(rpbgetreq)    -> 5;
msg_code(rpbgetresp)   -> 6;
msg_code(rpbputreq)    -> 7;
msg_code(rpbputresp)   -> 8;
msg_code(rpbdelreq)    -> 9;
msg_code(rpbdelresp)   -> 10;
msg_code(rpbgetbucketpropsreq)   -> 11;
msg_code(rpbgetbucketpropsresp)  -> 12;
msg_code(rpbsetbucketpropsreq)   -> 13;
msg_code(rpbsetbucketpropsresp)  -> 14;
msg_code(rpblistbucketsreq)      -> 15;
msg_code(rpblistbucketsresp)     -> 16;
msg_code(rpblistkeysreq)         -> 17;
msg_code(rpblistkeysresp)        -> 18;
msg_code(rpbmapredreq)           -> 19;
msg_code(rpbmapredresp)          -> 20.
    

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================
    
%% Convert a vector clock to erlang
erlify_rpbvc(undefined) ->
    vclock:fresh();
erlify_rpbvc(PbVc) ->
    binary_to_term(zlib:unzip(PbVc)).

%% Convert a vector clock to protocol buffers
pbify_rpbvc(Vc) ->
    zlib:zip(term_to_binary(Vc)).

%% Convert options to protocol buffers
pbify_rpboptions(undefined) ->
    undefined;
pbify_rpboptions(Options) ->
   pbify_rpboptions(Options, #rpboptions{}).

pbify_rpboptions([], RpbOpts) ->
    RpbOpts;
pbify_rpboptions([return_body|Rest], RpbOpts) ->
    pbify_rpboptions(Rest, RpbOpts#rpboptions{return_body = true}).

%% Convert a list of {MetaData,Value} pairs to protocol buffers
pbify_rpbcontents([], Acc) ->
    lists:reverse(Acc);
pbify_rpbcontents([Content | Rest], Acc) ->
    pbify_rpbcontents(Rest, [pbify_rpbcontent(Content) | Acc]).

%% Convert a metadata/value pair into an #rpbcontent{} record    
pbify_rpbcontent({Metadata, Value}) ->
    {PbContent, LeftOver} = 
        dict:fold(fun pbify_rpbcontent_entry/3, {#rpbcontent{value = Value}, []}, Metadata),
    case LeftOver of
        [] ->
            PbLeftOver = undefined;
        LeftOver ->
            PbLeftOver = pbify_rpbterm({struct, LeftOver})
    end,
    PbContent#rpbcontent{metadata = PbLeftOver}.

%% Convert the metadata dictionary entries to protocol buffers
pbify_rpbcontent_entry(?MD_CTYPE, ContentType, {PbContent, LeftOver}) when is_list(ContentType) -> 
    {PbContent#rpbcontent{content_type = ContentType}, LeftOver};
pbify_rpbcontent_entry(?MD_CHARSET, Charset, {PbContent, LeftOver}) when is_list(Charset) ->
    {PbContent#rpbcontent{charset = Charset}, LeftOver};
pbify_rpbcontent_entry(?MD_ENCODING, Encoding, {PbContent, LeftOver}) when is_list(Encoding) ->
    {PbContent#rpbcontent{content_encoding = Encoding}, LeftOver};
pbify_rpbcontent_entry(?MD_VTAG, Vtag, {PbContent, LeftOver}) when is_list(Vtag) ->
    {PbContent#rpbcontent{vtag = Vtag}, LeftOver};
pbify_rpbcontent_entry(?MD_LINKS, Links, {PbContent, LeftOver}) when is_list(Links) ->
    {PbContent#rpbcontent{links = [pbify_rpblink(E) || E <- Links]}, LeftOver};
pbify_rpbcontent_entry(?MD_LASTMOD, {MS,S,US}, {PbContent, LeftOver}) -> 
    {PbContent#rpbcontent{last_mod = 1000000*MS+S, last_mod_usecs = US}, LeftOver};
pbify_rpbcontent_entry(?MD_USERMETA, UserMeta, {PbContent, LeftOver}) when is_list(UserMeta) ->
    {PbContent#rpbcontent{usermeta = [pbify_rpbpair(E) || E <- UserMeta]}, LeftOver};
pbify_rpbcontent_entry(Key, Value, {PbContent, LeftOver}) ->
    {PbContent, [{Key, Value} | LeftOver]}.

%% Convert an rpccontent pb message to an erlang {MetaData,Value} tuple
erlify_rpbcontent(PbC) ->
    case PbC#rpbcontent.metadata of
        undefined ->
            ErlMd0 = dict:new();
        
        PbMd ->
            {struct, MdList} = erlify_rpbterm(PbMd), 
            ErlMd0 = dict:from_list(MdList)
    end,
    case PbC#rpbcontent.content_type of
        undefined ->
            ErlMd1 = ErlMd0;
        ContentType ->
            ErlMd1 = dict:store(?MD_CTYPE, binary_to_list(ContentType), ErlMd0)
    end,
    case PbC#rpbcontent.charset of
        undefined ->
            ErlMd2 = ErlMd1;
        Charset ->
            ErlMd2 = dict:store(?MD_CHARSET, binary_to_list(Charset), ErlMd1)
    end,
    case PbC#rpbcontent.content_encoding of
        undefined ->
            ErlMd3 = ErlMd2;
        Encoding ->
            ErlMd3 = dict:store(?MD_ENCODING, binary_to_list(Encoding), ErlMd2)
    end,
    case PbC#rpbcontent.vtag of
        undefined ->
            ErlMd4 = ErlMd3;
        Vtag ->
            ErlMd4 = dict:store(?MD_VTAG, binary_to_list(Vtag), ErlMd3)
    end,
    case PbC#rpbcontent.links of
        undefined ->
            ErlMd5 = ErlMd4;
        PbLinks ->
            Links = [erlify_rpblink(E) || E <- PbLinks],
            ErlMd5 = dict:store(?MD_LINKS, Links, ErlMd4)
    end,
    case PbC#rpbcontent.last_mod of
        undefined ->
            ErlMd6 = ErlMd5;
        LastMod ->
            case PbC#rpbcontent.last_mod_usecs of
                undefined ->
                    Usec = 0;
                Usec ->
                    Usec
            end,
            Msec = LastMod div 1000000,
            Sec = LastMod rem 1000000,
            ErlMd6 = dict:store(?MD_LASTMOD, {Msec,Sec,Usec}, ErlMd5)
    end,
    case PbC#rpbcontent.usermeta of
        undefined ->
            ErlMd = ErlMd6;
        PbUserMeta ->
            UserMeta = [erlify_rpbpair(E) || E <- PbUserMeta],
            ErlMd = dict:store(?MD_USERMETA, UserMeta, ErlMd6)
    end,

    {ErlMd, PbC#rpbcontent.value}.
    

%% Convert {K,V} tuple to protocol buffers
pbify_rpbpair({K,V}) ->
    #rpbpair{key = K, value = V}.

%% Convert RpbPair PB message to erlang {K,V} tuple
erlify_rpbpair(#rpbpair{key = K, value = V}) ->
    {binary_to_list(K), binary_to_list(V)}.
    
%% Covnert erlang link tuple to RpbLink PB message
pbify_rpblink({{B,K},T}) ->
    #rpblink{bucket = B, key = K, tag = T}.

%% Convert RpbLink PB message to erlang link tuple
erlify_rpblink(#rpblink{bucket = B, key = K, tag = T}) ->
    {{B,K},T}.

%% Protocol bufferify bucket properties
%%
pbify_bucket_props(Props) ->
    pbify_rpbterm({struct, [pbify_bucket_prop(Prop) || Prop <- Props]}).


pbify_bucket_prop({linkfun, {modfun, Module, Function}}) ->
    {?PB_LINKFUN, {struct, [{?PB_MOD, to_binary(Module)},
                             {?PB_FUN, to_binary(Function)}]}};
pbify_bucket_prop({linkfun, {qfun, _}}) ->
    {?PB_LINKFUN, <<"qfun">>};
pbify_bucket_prop({linkfun, {jsfun, Name}}) ->
    {?PB_LINKFUN, {struct, [{?PB_JSFUN, to_binary(Name)}]}};
pbify_bucket_prop({linkfun, {jsanon, {Bucket, Key}}}) ->
    {?PB_LINKFUN, {struct, [{?PB_JSANON,
                               {struct, [{?PB_JSBUCKET, Bucket},
                                         {?PB_JSKEY, Key}]}}]}};
pbify_bucket_prop({linkfun, {jsanon, Source}}) ->
    {?PB_LINKFUN, {struct, [{?PB_JSANON, Source}]}};
pbify_bucket_prop({chash_keyfun, {Module, Function}}) ->
    {?PB_CHASH, {struct, [{?PB_MOD, to_binary(Module)},
                          {?PB_FUN, to_binary(Function)}]}};
pbify_bucket_prop({Prop, Value}) ->
    {list_to_binary(atom_to_list(Prop)), Value}.


%% Erlify bucket properties
%%
erlify_bucket_props(RpbProps) ->
    {struct, Props} = erlify_rpbterm(RpbProps),
    [erlify_bucket_prop(Prop) || Prop <- Props].

%% @spec erlify_bucket_prop({Property::binary(), jsonpropvalue()}) ->
%%          {Property::atom(), erlpropvalue()}
%% @doc The reverse of pbify_bucket_prop/1.  Converts PB representation
%%      of bucket properties to their Erlang form.
erlify_bucket_prop({?PB_LINKFUN, {struct, Props}}) ->
    case {proplists:get_value(?PB_MOD, Props),
          proplists:get_value(?PB_FUN, Props)} of
        {Mod, Fun} when is_binary(Mod), is_binary(Fun) ->
            {linkfun, {modfun,
                       list_to_existing_atom(binary_to_list(Mod)),
                       list_to_existing_atom(binary_to_list(Fun))}};
        {undefined, undefined} ->
            case proplists:get_value(?PB_JSFUN, Props) of
                Name when is_binary(Name) ->
                    {linkfun, {jsfun, Name}};
                undefined ->
                    case proplists:get_value(?PB_JSANON, Props) of
                        {struct, Bkey} ->
                            Bucket = proplists:get_value(?PB_JSBUCKET, Bkey),
                            Key = proplists:get_value(?PB_JSKEY, Bkey),
                            %% bomb if malformed
                            true = is_binary(Bucket) andalso is_binary(Key),
                            {linkfun, {jsanon, {Bucket, Key}}};
                        Source when is_binary(Source) ->
                            {linkfun, {jsanon, Source}}
                    end
            end
    end;
erlify_bucket_prop({?PB_CHASH, {struct, Props}}) ->
    {chash_keyfun, {list_to_existing_atom(
                      binary_to_list(
                        proplists:get_value(?PB_MOD, Props))),
                    list_to_existing_atom(
                      binary_to_list(
                        proplists:get_value(?PB_FUN, Props)))}};
erlify_bucket_prop({?PB_ALLOW_MULT, Value}) ->
    {allow_mult, any_to_bool(Value)};
erlify_bucket_prop({Prop, Value}) ->
    {list_to_existing_atom(binary_to_list(Prop)), Value}.

%% Convert RpbMapRedInput message to erlang tuple
erlify_mapred_input(#rpbmapredinput{bucket = Bucket, key = Key, data = undefined}) ->
    {Bucket, Key};
erlify_mapred_input(#rpbmapredinput{bucket = Bucket, key = Key, data = PbData}) ->
    {Bucket, Key, erlify_rpbterm(PbData)}.

%% Convert list of RpbMapRedPhase to list of erlang phase tuples
erlify_mapred_query(PbQuery) ->
    erlify_mapred_phases(PbQuery, []).

erlify_mapred_phases([], ErlPhases) ->
    {ok, lists:reverse(ErlPhases)};
erlify_mapred_phases([#rpbmapredphase{type = <<"link">>, bucket = B, tag = T} | Rest], 
                     ErlPhases) ->
    case {B, T} of
        {undefined, _} ->
            {error, "link missing bucket"};
        {_, undefined} ->
            {error, "link missing tag"};
        _ ->
            erlify_mapred_phases(Rest, [{link, B, T} | ErlPhases])
    end;
erlify_mapred_phases([#rpbmapredphase{type = <<"map">>, keep = PbKeep, arg = PbArg}=PbPhase | Rest],
                     ErlPhases) ->
    case erlify_mapred_funterm(PbPhase) of
        {ok, FunTerm} ->
            Arg = erlify_rpbterm(PbArg),
            erlify_mapred_phases(Rest, [{map, FunTerm, Arg, any_to_bool(PbKeep)} | ErlPhases]);
        X ->
            X
    end;
erlify_mapred_phases([#rpbmapredphase{type = <<"reduce">>, keep = PbKeep, arg = PbArg}=PbPhase | 
                      Rest], ErlPhases) ->
    case erlify_mapred_funterm(PbPhase) of
        {ok, FunTerm} ->
            Arg = erlify_rpbterm(PbArg),
            erlify_mapred_phases(Rest, [{reduce, FunTerm, Arg, any_to_bool(PbKeep)} | ErlPhases]);
        X ->
            X
    end;
erlify_mapred_phases([#rpbmapredphase{type = Type} | _Rest], _ErlPhases) ->
    {error, {unknown_phase_type, Type}}.

%% Build the FunTerm for a phase from an RpbMapRedPhase message
erlify_mapred_funterm(Phase) ->
    case Phase of
        #rpbmapredphase{language = <<"javascript">>, source = undefined, bucket = B, key = K,
                       function = undefined} when 
              B =/= undefined, K =/= undefined ->
            {ok, {jsanon, {B, K}}};
        #rpbmapredphase{language = <<"javascript">>, source = Source,
                        bucket = undefined, key = undefined} when Source =/= undefined ->
            {ok, {jsanon, Source}};
        #rpbmapredphase{language = <<"javascript">>, source = undefined,
                        bucket = undefined, key = undefined, function = Function} when 
              function =/= undefined ->
            {ok, {jsfun, Function}};
        #rpbmapredphase{language = <<"erlang">>, source = undefined,
                        module = Module, function = Function} when Module =/= undefined,
                                                                   Function =/= undefined ->
            try
                {ok, {modfun, list_to_existing_atom(binary_to_list(Module)),
                              list_to_existing_atom(binary_to_list(Function))}}
            catch
                error:badarg ->
                    {error, "nonexistant module/function name"}
            end;
        #rpbmapredphase{language = <<"erlang">>, source = undefined,
                        module = undefined, function = Function} when Function =/= undefined ->
            try
                F = binary_to_term(Function),
                true = is_function(F),
                {ok, {qfun, F}}
            catch
                error:badarg ->
                    {error, "nonexistant module/function name"};
                error:{batmatch,_} ->
                    {error, "not an external binary encoded function"}
            end;
        _ ->
            {error, "cannot parse function term"}
    end.
         
%% Convert erlang term to RpbTerm message tree
%% JSON-like generic mapping for sending general terms across the PB interface
%% Uses the same {array, [term()]} and {object, [{term(),term()}]} convention
%% as mochijson.

-define(RPB_TERM_INTEGER, 1).
-define(RPB_TERM_BOOLEAN, 2).
-define(RPB_TERM_STRING, 3).
-define(RPB_TERM_OBJECT, 5).
-define(RPB_TERM_ARRAY, 6).
 
pbify_rpbterm(T) when is_atom(T) ->
    #rpbterm{type = ?RPB_TERM_STRING, string_value = list_to_binary(atom_to_list(T))};
pbify_rpbterm(T) when is_list(T) ->
    %% treat lists as lists - encode as a binary if you want a string
    #rpbterm{type = ?RPB_TERM_ARRAY, array_values = [pbify_rpbterm(E) || E <- T]};
pbify_rpbterm(T) when is_binary(T) ->
    #rpbterm{type = ?RPB_TERM_STRING, string_value = T};
pbify_rpbterm(T) when is_integer(T) ->
    #rpbterm{type = ?RPB_TERM_INTEGER, int_value = T};
pbify_rpbterm({array, L}) when is_list(L) ->
    #rpbterm{type = ?RPB_TERM_ARRAY, array_values = [pbify_rpbterm(E) || E <- L]};
pbify_rpbterm({struct, L}) ->
    E = [#rpbobjectentry{name = to_binary(Name), value = pbify_rpbterm(Value)} ||
            {Name, Value} <- L],
    #rpbterm{type = ?RPB_TERM_OBJECT, object_entries = E}.

%% Convert RpbTerm message to a term()
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_INTEGER, int_value = Int}) when Int =/= undefined ->
    Int;
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_BOOLEAN, int_value = Int}) when Int =/= undefined ->
    case Int of
        0 ->
            false;
        _ ->
            true
    end;
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_STRING, string_value = Str}) when Str =/= undefined ->
    binary_to_list(Str);
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_OBJECT, object_entries = undefined}) ->
    {struct, []};
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_OBJECT, object_entries = List}) ->
    {struct, [{Name, erlify_rpbterm(Value)} || 
                  #rpbobjectentry{name=Name, value=Value} <- List]};
erlify_rpbterm(#rpbterm{type = ?RPB_TERM_ARRAY, array_values = List}) ->
    {array, [erlify_rpbterm(E) || E <- List]};
erlify_rpbterm(undefined) ->
    undefined.

%% Make sure an atom/string/binary is definitely a binary
to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

%% Try and convert to true/false atoms
any_to_bool(V) when is_list(V) ->
    (V == "1") orelse (V == "true") orelse (V == "TRUE");
any_to_bool(V) when is_binary(V) ->
    any_to_bool(binary_to_list(V));
any_to_bool(V) when is_integer(V) ->
    V /= 0;
any_to_bool(V) when is_boolean(V) ->
    V.


%% ===================================================================
%% Unit Tests
%% ===================================================================
-ifdef(TEST).

content_encode_decode_test() ->
    MetaData = dict:from_list(
                 [{?MD_CTYPE, "ctype"},
                  {?MD_CHARSET, "charset"},
                  {?MD_ENCODING, "encoding"},
                  {?MD_VTAG, "vtag"},
                  {?MD_LINKS, [{{<<"b1">>, <<"k1">>}, <<"v1">>},
                               {{<<"b2">>, <<"k2">>}, <<"v2">>}
                              ]},
                  {?MD_LASTMOD, {1, 2, 3}},
                  {?MD_USERMETA, [{"X-Riak-Meta-MyMetaData1","here it is"},
                                  {"X-Riak-Meta-MoreMd", "have some more"}
                                 ]}
                 ]),
    Value = <<"test value">>,
    {MetaData2, Value2} = erlify_rpbcontent(
                          riakclient_pb:decode_rpbcontent(
                            riakclient_pb:encode_rpbcontent(
                              pbify_rpbcontent({MetaData, Value})))),
    MdSame = (dict:to_list(MetaData) =:= dict:to_list(MetaData2)),
    MdSame = true,
    Value = Value2.

-endif.
  

