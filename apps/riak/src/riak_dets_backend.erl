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

%% @doc riak_dets_backend is a Riak storage backend using dets.

-module(riak_dets_backend).

-include_lib("eunit/include/eunit.hrl").
-export([start/2,stop/1,get/2,put/3,list/1,list_bucket/2,delete/2]).

% @type state() = term().
-record(state, {table}).

% @spec start(Partition :: integer(), Config :: proplist()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(Partition, Config) ->
    ConfigRoot = proplists:get_value(riak_dets_backend_root, Config),
    if ConfigRoot =:= undefined ->
            riak:stop("riak_dets_backend_root unset, failing.~n");
       true -> ok
    end,

    TablePath = filename:join([ConfigRoot, integer_to_list(Partition)]),
    case filelib:ensure_dir(TablePath) of
        ok -> ok;
        _Error ->
            riak:stop("riak_dets_backend could not ensure"
                      " the existence of its root directory")
    end,

    DetsName = list_to_atom(integer_to_list(Partition)),
    case dets:open_file(DetsName, [{file, TablePath}, 
                                   {min_no_slots, 8192},
                                   {max_no_slots, 16777216}]) of
        {ok, DetsName} ->
            {ok, #state{table=DetsName}};
        {error, Reason}  ->
            riak:stop("dets:open_file failed"),
            {error, Reason}
    end.

% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(#state{table=T}) -> dets:close(T).

% get(state(), Key :: binary()) ->
%   {ok, Val :: binary()} | {error, Reason :: term()}
% key must be 160b
get(#state{table=T}, BKey) ->
    case dets:lookup(T, BKey) of
        [] -> {error, notfound};
        [{BKey,Val}] -> {ok, Val};
        {error, Err} -> {error, Err}
    end.

% put(state(), Key :: binary(), Val :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
put(#state{table=T},BKey,Val) -> dets:insert(T, {BKey,Val}).

% delete(state(), Key :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
delete(#state{table=T}, BKey) -> dets:delete(T, BKey).

% list(state()) -> [Key :: binary()]
list(#state{table=T}) ->
    MList = dets:match(T,{'$1','_'}),
    list(MList,[]).
list([],Acc) -> Acc;
list([[K]|Rest],Acc) -> list(Rest,[K|Acc]).

list_bucket(#state{table=T}, {filter, Bucket, Fun}) ->
    MList = lists:filter(Fun, dets:match(T,{{Bucket,'$1'},'_'})),
    list(MList,[]);
list_bucket(#state{table=T}, Bucket) ->
    case Bucket of
        '_' -> MatchSpec = {{'$1','_'},'_'};
        _ -> MatchSpec = {{Bucket,'$1'},'_'}
    end,
    MList = dets:match(T,MatchSpec),
    list(MList,[]).

%%
%% Test
%%

% @private
simple_test() ->
    ?assertCmd("rm -rf test/dets-backend"),
    Config = [{riak_dets_backend_root, "test/dets-backend"}],
    riak_test_util:standard_backend_test(riak_dets_backend, Config).
