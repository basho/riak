%% -------------------------------------------------------------------
%%
%% riak_kv_bitcask_backend: Bitcask Driver for Riak
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

-module(riak_kv_bitcask_backend).
-author('Andy Gross <andy@basho.com>').
-author('Dave Smith <dizzyd@basho.com>').

-export([start/2,
         stop/1,
         get/2,
         put/3,
         delete/2,
         list/1,
         list_bucket/2,
         fold/3,
         drop/1,
         is_empty/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(Partition, Config) ->
    BitcaskRoot = filename:join([proplists:get_value(data_root, Config),
                                 "bitcask",
                                 integer_to_list(Partition)]),
    case bitcask:open(BitcaskRoot, [{read_write, true}]) of
        {ok, State} ->
            %% We need to store the state of bitcask in the process dictionary
            %% as each request mutates it.
            erlang:put(?MODULE, State),
            {ok, BitcaskRoot};
        {error, Reason} ->
            {error, Reason}
    end.


stop(_BitcaskRoot) ->
    bitcask:close(erlang:get(?MODULE)).


get(_BitcaskRoot, BKey) ->
    State0 = erlang:get(?MODULE),
    Key = term_to_binary(BKey),
    case bitcask:get(State0, Key) of
        {ok, Value, State} ->
            erlang:put(?MODULE, State),
            {ok, Value};
        {not_found, State} ->
            erlang:put(?MODULE, State),
            {error, notfound};
        {error, Reason} ->
            {error, Reason}
    end.

put(_BitcaskRoot, BKey, Val) ->
    State0 = erlang:get(?MODULE),
    Key = term_to_binary(BKey),
    case bitcask:put(State0, Key, Val) of
        {ok, State} ->
            erlang:put(?MODULE, State),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

delete(_BitcaskRoot, BKey) ->
    State0 = erlang:get(?MODULE),
    case bitcask:delete(State0, term_to_binary(BKey)) of
        {ok, State} ->
            erlang:put(?MODULE, State),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

list(_BitcaskRoot) ->
    State0 = erlang:get(?MODULE),
    case bitcask:list_keys(State0) of
        KeyList when is_list(KeyList) ->
            [binary_to_term(K) || K <- KeyList];
        Other ->
            Other
    end.

list_bucket(_BitcaskRoot, {filter, Bucket, Fun}) ->
    [K || {B, K} <- ?MODULE:list(none),
          B =:= Bucket,
          Fun(K)];
list_bucket(_BitcaskRoot, '_') ->
    [B || {B, _K} <- ?MODULE:list(none)];
list_bucket(_BitcaskRoot, Bucket) ->
    [K || {B, K} <- ?MODULE:list(none), B =:= Bucket].


fold(_BitcaskRoot, Fun0, Acc0) ->
    %% When folding across the bitcask, the bucket/key tuple must
    %% be decoded. The intermediate binary_to_term call handles this
    %% and yields the expected fun({B, K}, Value, Acc)
    bitcask:fold(erlang:get(?MODULE),
                 fun(K, V, Acc) ->
                         Fun0(binary_to_term(K), V, Acc)
                 end,
                 Acc0).

drop(_) -> ok.

is_empty(_BitcaskRoot) ->
    %% Determining if a bitcask is empty requires us to find at least
    %% one value that is NOT a tombstone. Accomplish this by doing a fold
    %% that forcibly bails on the very first k/v encountered.
    F = fun(_K, _V, _Acc0) ->
                throw(found_one_value)
        end,
    case catch(bitcask:fold(erlang:get(?MODULE), F, undefined)) of
        found_one_value ->
            false;
        _ ->
            true
    end.




%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

simple_test() ->
    ?assertCmd("rm -rf test/bitcask-backend"),
    Config = [{data_root, "test/bitcask-backend"}],
    riak_kv_test_util:standard_backend_test(?MODULE, Config).

-endif.
