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

% @doc riak_fs_backend is a simple filesystem storage system.

-module(riak_fs_backend).
-export([start/1,stop/1,get/2,put/3,list/1,list_bucket/2,delete/2]).
-include_lib("eunit/include/eunit.hrl").
% @type state() = term().
-record(state, {dir}).

% @spec start(Partition :: integer()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(Partition) ->
    PartitionName = integer_to_list(Partition),
    ConfigRoot = riak:get_app_env(riak_fs_backend_root),
    if
        ConfigRoot =:= undefined ->
            riak:stop("riak_fs_backend_root unset, failing.");
        true -> ok
    end,
    Dir = filename:join([ConfigRoot,PartitionName]),
    {filelib:ensure_dir(Dir), #state{dir=Dir}}.

% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(_State) -> ok.

% get(state(), Key :: binary()) ->
%   {ok, Val :: binary()} | {error, Reason :: term()}
% key must be 160b
get(State, BKey) ->
    File = location(State,BKey),
    case filelib:is_file(File) of
        false -> {error, notfound};
        true -> file:read_file(File)
    end.

% put(state(), Key :: binary(), Val :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
put(State,BKey,Val) ->       
    File = location(State,BKey),
    case filelib:ensure_dir(File) of
        ok -> file:write_file(File,Val);
        X -> X
    end.

% delete(state(), Key :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
delete(State, BKey) ->
    File = location(State,BKey),
    case file:delete(File) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Err} -> {error, Err}
    end.

% list(state()) -> [Key :: binary()]
list(State) ->
    % this is slow slow slow
    %                                              B,N,N,N,K
    [location_to_bkey(X) || X <- filelib:wildcard("*/*/*/*/*",
                                                  State#state.dir)].

list_bucket(State, Bucket) ->
    B64 = encode_bucket(Bucket),
    L = length(State#state.dir),
    [ K || {_,K} <- [ location_to_bkey(lists:nthtail(L, X)) ||
                        X <- filelib:wildcard(
                               filename:join([State#state.dir,
                                              B64,"*/*/*/*"])) ]].

location(State, {Bucket, Key}) ->
    B64 = encode_bucket(Bucket),
    K64 = encode_key(Key),
    [N1,N2,N3] = nest(K64),
    filename:join([State#state.dir, B64, N1, N2, N3, K64]).

location_to_bkey(Path) ->
    [B64,_,_,_,K64] = string:tokens(Path, "/"),
    {decode_bucket(B64), decode_key(K64)}.

encode_bucket(Bucket) ->
    clean(base64:encode_to_string(atom_to_list(Bucket))).

decode_bucket(B64) ->
    list_to_atom(base64:decode_to_string(dirty(B64))).

encode_key(Key) ->
    clean(base64:encode_to_string(Key)).

decode_key(K64) ->
    base64:decode(dirty(K64)).

clean(Str64) ->
    lists:map(fun($=) -> $-;
                 ($+) -> $_;
                 ($/) -> $,;
                 (C)  -> C
              end,
              Str64).

dirty(Str64) ->
    lists:map(fun($-) -> $=;
                 ($_) -> $+;
                 ($,) -> $/;
                 (C)  -> C
              end,
              Str64).

nest(Key) -> nest(lists:reverse(string:substr(Key, 1, 6)), 3, []).
nest(_, 0, Parts) -> Parts;
nest([Nb,Na|Rest],N,Acc) ->
    nest(Rest, N-1, [[Na,Nb]|Acc]);
nest([Na],N,Acc) ->
    nest([],N-1,[[Na]|Acc]);
nest([],N,Acc) ->
    nest([],N-1,["0"|Acc]).

%%
%% Test
%%

simple_test() ->
    application:set_env(riak, riak_fs_backend_root,
                        "test/fs-backend"),
    ?assertCmd("rm -rf test/fs-backend"),
    riak_test_util:standard_backend_test(riak_fs_backend).

dirty_clean_test() ->
    Dirty = "abc=+/def",
    Clean = clean(Dirty),
    [ ?assertNot(lists:member(C, Clean)) || C <- "=+/" ],
    ?assertEqual(Dirty, dirty(Clean)).

nest_test() ->
    ?assertEqual(["ab","cd","ef"],nest("abcdefg")),
    ?assertEqual(["ab","cd","ef"],nest("abcdef")),
    ?assertEqual(["a","bc","de"], nest("abcde")),
    ?assertEqual(["0","ab","cd"], nest("abcd")),
    ?assertEqual(["0","a","bc"],  nest("abc")),
    ?assertEqual(["0","0","ab"],  nest("ab")),
    ?assertEqual(["0","0","a"],   nest("a")),
    ?assertEqual(["0","0","0"],   nest([])).
