%% -------------------------------------------------------------------
%%
%% riak_mapred_term: Term parsing for mapreduce
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

%% @doc Term parsing for mapreduce

-module(riak_kv_mapred_term).

-export([parse_request/1]).

-define(DEFAULT_TIMEOUT, 60000).

%%
%% Parse a map/reduce request encoded as a property list
%% [{'inputs', Inputs},
%%  {'query', Query},
%%  {'timeout', Timeout}].
%%
parse_request(BinReq) ->
    try
        Req = binary_to_term(BinReq),

        Timeout = proplists:get_value(timeout, Req, ?DEFAULT_TIMEOUT),
        Inputs = proplists:get_value(inputs, Req, undefined),
        Query = proplists:get_value('query', Req, undefined),

        case {valid_inputs(Inputs), valid_query(Query)} of
            {ok, ok} ->
                {ok, Inputs, Query, Timeout};
            {{error, Reason}, _} ->
                {error, {'inputs', Reason}};
            {_, {error, Reason}} ->
                {error, {'query', Reason}}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% Return ok if inputs are valid, {error, Reason} if not
valid_inputs(Bucket) when is_binary(Bucket) ->    
    ok;
valid_inputs(Targets) when is_list(Targets) ->
    valid_input_targets(Targets);
valid_inputs(Invalid) ->
    {error, {"Inputs must be a binary bucket or a list of target tuples:", Invalid}}.

valid_input_targets([]) ->
    ok;
valid_input_targets([{B,K}|Rest]) when is_binary(B), is_binary(K) ->
    valid_input_targets(Rest);
valid_input_targets([{{B,K},_KeyData}|Rest]) when is_binary(B), is_binary(K) ->
    valid_input_targets(Rest);
valid_input_targets(Invalid) ->
     {error, {"Inputs target tuples must be {B,K} or {{B,K},KeyData}:", Invalid}}.

%% Return ok if query are valid, {error, Reason} if not.  Not very strong validation
%% done here as riak_kv_mapred_query will check this.
valid_query(Query) when is_list(Query) ->
    ok;
valid_query(Invalid) ->
    {error, {"Query takes a list of step tuples", Invalid}}.
