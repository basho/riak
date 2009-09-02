%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

%% @doc riak_osmos_backend is a storage backend that makes use of
%%      the Dukes of Erl "osmos" file storage system.
%%      http://dukesoferl.blogspot.com/2009/07/osmos.html
%%      http://code.google.com/p/osmos/
%%
%% Configuration parameters:
%%
%%  riak_osmos_backend_root:
%%       Directory in which to keep osmos files
%%       Required - riak will exit if this option is not set                    
%%
%%  riak_osmost_backend_block_size:
%%       "Block size" parameter, as documented in osmos's docs
%%       osmos_table_format:new/3
%%       Optional - defaulted to 2048
%%
%% Notes:
%%  - Support for the 'delete' operation is strange in osmos.  As such,
%%    delete is implemented here as writing an empty binary for a key's
%%    value.  This is safe because no riak_object will ever serialize
%%    to an empty binary.
%%  - The osmos documentation is not explicit about it, but opening two
%%    tables in the same directory will cause osmos to become confused.
%%    To solve this, each partition opens its table in a unique subdirectory
%%    under riak_osmos_backend_root.
-module(riak_osmos_backend).

-export([start/1,stop/1,get/2,put/3,list/1,list_bucket/2,delete/2]).
-include_lib("eunit/include/eunit.hrl").
-record(state, {table}).

%% @spec start(Partition :: integer()) ->
%%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(Partition) ->
    ConfigRoot = riak:get_app_env(riak_osmos_backend_root),
    if ConfigRoot =:= undefined ->
            riak:stop("riak_osmos_backend_root unset, failing.");
       true -> ok
    end,

    %% put each table in its own directory, named after the partition
    %% osmos requires this - each table must have its own directory
    TableRoot = filename:join([ConfigRoot, integer_to_list(Partition)]),
    case filelib:ensure_dir(TableRoot) of
        ok -> ok;
        _Error ->
            riak:stop("riak_osmos_backend could not ensure"
                      " the existence of its root directory")
    end,

    case application:start(osmos) of
        ok -> ok;
        {error,{already_started,osmos}} -> ok
    end,

    BlockSize = riak:get_app_env(riak_osmos_backend_block_size, 2048),
    Format = osmos_table_format:new(binary, binary_replace, BlockSize),

    Ready = case osmos:open(Partition, 
                            [{directory, TableRoot},
                             {format, Format}]) of
                {ok, Partition} ->
                    ok;
                {error, Reason} ->
                    riak:stop("osmos:open failed"),
                    {error, Reason}
            end,
    {Ready, #state{table=Partition}}.

%% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(#state{table=Table}) ->
    osmos:close(Table).

%% get(state(), Key :: binary()) ->
%%   {ok, Val :: binary()} | {error, Reason :: term()}
%% key must be 160b
get(#state{table=Table}, BKey) ->
    case osmos:read(Table, term_to_binary(BKey)) of
        {ok, <<>>}  -> {error, notfound}; %% sentinal for delete
        {ok, Value} -> {ok, Value};
        not_found   -> {error, notfound}
    end.

%% put(state(), Key :: binary(), Val :: binary()) ->
%%   ok | {error, Reason :: term()}
%% key must be 160b
put(#state{table=Table},BKey,Val) ->       
    osmos:write(Table, term_to_binary(BKey), Val).

%% delete(state(), Key :: binary()) ->
%%   ok | {error, Reason :: term()}
%% key must be 160b
delete(#state{table=Table}, BKey) ->
    osmos:write(Table, term_to_binary(BKey), <<>>). %% sentinal for delete

-define(SELECT_CHUNK, 1000).

%% list(state()) -> [Key :: binary()]
list(#state{table=Table}) ->
    accum(Table, fun(K,_) -> {true, binary_to_term(K)} end).

%% list_bucket(state(), Bucket :: atom()) -> [Key :: binary()]
list_bucket(#state{table=Table}, Bucket) ->
    accum(Table,
          fun(K,_) ->
                  case binary_to_term(K) of
                      {Bucket, Key} -> {true, Key};
                      _             -> false
                  end
          end).

%% @spec accum(osmos_table(), function()) -> [term()]
%% @doc map across the rows in the osmos table, and return
%%      terms R for which Fun returns {true, R}
%% Explanation of osmos:select_range params:
%%   The three functions are, in order, LessLo, LessHi, and Select.
%%   We are trying to select *all* keys, not a limited range, thus,
%%   LessLo always returns 'false' to say "all keys are not less than
%%   our desired lower bound" and LessHi always returns 'true' to say
%%   "all keys are less than our desired upper bound".
%%   Select's only function is to throw away the keys that have been
%%   deleted (i.e. that have the delete sentinal stored as their value).
accum(Table, Fun) ->
    accum2(Table, Fun,
           osmos:select_range(Table,
                              fun(_) -> false end,
                              fun(_) -> true end,
                              fun(_,V) -> V /= <<>> end,
                              ?SELECT_CHUNK),
           []).
                          
%% simple accumulator to exhaust select_range's continuation should
%% there be more than SELECT_CHUNK keys to return
accum2(_, _, {ok, [], _}, Acc) -> lists:append(Acc);
accum2(_, _, {error, _}, Acc)  -> lists:append(Acc);
accum2(Table, Fun, {ok, NewList, Continue}, Acc) ->
    accum2(Table, Fun,
           osmos:select_continue(Table, Continue, ?SELECT_CHUNK),
           [[ A || {true, A} <- [ Fun(K,V) || {K,V} <- NewList]]|Acc]).

%%
%% Test
%%

simple_test() ->
    application:set_env(riak, riak_osmos_backend_root,
                        "test/osmos-backend"),
    ?assertCmd("rm -rf test/osmos-backend"),
    {ok, S} = riak_osmos_backend:start(42),
    ok = riak_osmos_backend:put(S,<<"k1">>,<<"v1">>),
    ok = riak_osmos_backend:put(S,<<"k2">>,<<"v2">>),
    {ok,<<"v2">>} = riak_osmos_backend:get(S,<<"k2">>),
    {error, notfound} = riak_osmos_backend:get(S, <<"k3">>),
    [<<"k1">>,<<"k2">>] = lists:sort(riak_osmos_backend:list(S)),
    ok = riak_osmos_backend:delete(S,<<"k2">>),
    [<<"k1">>] = riak_osmos_backend:list(S),
    ok = riak_osmos_backend:stop(S).
