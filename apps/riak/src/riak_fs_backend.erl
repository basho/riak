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
-export([start/2,stop/1,get/2,put/3,list/1,list_bucket/2,delete/2]).
%-export([fold/3, drop/1, is_empty/1]).

-include_lib("eunit/include/eunit.hrl").
% @type state() = term().
-record(state, {dir}).

%% @spec start(Partition :: integer(), Config :: proplist()) ->
%%          {ok, state()} | {{error, Reason :: term()}, state()}
%% @doc Start this backend.  'riak_fs_backend_root' must be set in
%%      Riak's application environment.  It must be set to a string
%%      representing the base directory where this backend should
%%      store its files.
start(Partition, Config) ->
    PartitionName = integer_to_list(Partition),
    ConfigRoot = proplists:get_value(riak_fs_backend_root, Config),
    if
        ConfigRoot =:= undefined ->
            riak:stop("riak_fs_backend_root unset, failing.");
        true -> ok
    end,
    Dir = filename:join([ConfigRoot,PartitionName]),
    {filelib:ensure_dir(Dir), #state{dir=Dir}}.

%% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(_State) -> ok.

%% @spec get(state(), BKey :: riak_object:bkey()) ->
%%         {ok, Val :: binary()} | {error, Reason :: term()}
%% @doc Get the object stored at the given bucket/key pair
get(State, BKey) ->
    File = location(State,BKey),
    case filelib:is_file(File) of
        false -> {error, notfound};
        true -> file:read_file(File)
    end.

%% @spec atomic_write(File :: string(), Val :: binary()) ->
%%       ok | {error, Reason :: term()}
%% @doc store a atomic value to disk. Write to temp file and rename to
%%       normal path.
atomic_write(File, Val) ->
    FakeFile = File ++ ".tmpwrite",
    case file:write_file(FakeFile, Val) of
        ok ->
            file:rename(FakeFile, File);
        X -> X
    end.

%% @spec put(state(), BKey :: riak_object:bkey(), Val :: binary()) ->
%%         ok | {error, Reason :: term()}
%% @doc Store Val under Bkey
put(State,BKey,Val) ->       
    File = location(State,BKey),
    case filelib:ensure_dir(File) of
        ok -> atomic_write(File, Val);
        X -> X
    end.

%% @spec delete(state(), BKey :: riak_object:bkey()) ->
%%          ok | {error, Reason :: term()}
%% @doc Delete the object stored at BKey
delete(State, BKey) ->
    File = location(State,BKey),
    case file:delete(File) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Err} -> {error, Err}
    end.

%% @spec list(state()) -> [{Bucket :: riak_object:bucket(),
%%                          Key :: riak_object:key()}]
%% @doc Get a list of all bucket/key pairs stored by this backend
list(State) ->
    % this is slow slow slow
    %                                              B,N,N,N,K
    [location_to_bkey(X) || X <- filelib:wildcard("*/*/*/*/*",
                                                  State#state.dir)].


%% @spec list_bucket(state(), riak_object:bucket()) ->
%%           [riak_object:key()]
%% @doc Get a list of the keys in a bucket
list_bucket(State, Bucket) ->
    case Bucket of
        '_' ->
            lists:usort(lists:map(fun({B, _}) -> B end, list(State)));
        {filter, B, Fun} ->
            [ hd(K) || K <-
                lists:filter(Fun,
                    [ EV || EV <- lists:map(fun(K) ->
                                                case K of
                                                    {B, Key} -> [Key];
                                                    _ -> []
                                                end
                                            end, list(State)),
                            EV /= [] ]) ];
        _ ->
            B64 = encode_bucket(Bucket),
            L = length(State#state.dir),
            [ K || {_,K} <- [ location_to_bkey(lists:nthtail(L, X)) ||
                                X <- filelib:wildcard(
                                       filename:join([State#state.dir,
                                                      B64,"*/*/*/*"])) ]]
    end.

%% @spec location(state(), {riak_object:bucket(), riak_object:key()})
%%          -> string()
%% @doc produce the file-path at which the object for the given Bucket
%%      and Key should be stored
location(State, {Bucket, Key}) ->
    B64 = encode_bucket(Bucket),
    K64 = encode_key(Key),
    [N1,N2,N3] = nest(K64),
    filename:join([State#state.dir, B64, N1, N2, N3, K64]).

%% @spec location_to_bkey(string()) ->
%%           {riak_object:bucket(), riak_object:key()}
%% @doc reconstruct a Riak bucket/key pair, given the location at
%%      which its object is stored on-disk
location_to_bkey(Path) ->
    [B64,_,_,_,K64] = string:tokens(Path, "/"),
    {decode_bucket(B64), decode_key(K64)}.

%% @spec encode_bucket(binary()) -> string()
%% @doc make a filename out of a Riak bucket
encode_bucket(Bucket) ->
    clean(base64:encode_to_string(Bucket)).

%% @spec decode_bucket(string()) -> binary()
%% @doc reconstruct a Riak bucket, given a filename
%% @see encode_bucket/1
decode_bucket(B64) ->
    base64:decode(dirty(B64)).

%% @spec encode_key(binary()) -> string()
%% @doc make a filename out of a Riak object key
encode_key(Key) ->
    clean(base64:encode_to_string(Key)).

%% @spec decode_key(string()) -> binary()
%% @doc reconstruct a Riak object key, given a filename
%% @see encode_key/1
decode_key(K64) ->
    base64:decode(dirty(K64)).

%% @spec clean(string()) -> string()
%% @doc remove characters from base64 encoding, which may
%%      cause trouble with filenames
clean(Str64) ->
    lists:map(fun($=) -> $-;
                 ($+) -> $_;
                 ($/) -> $,;
                 (C)  -> C
              end,
              Str64).

%% @spec dirty(string()) -> string()
%% @doc replace filename-troublesome base64 characters
%% @see clean/1
dirty(Str64) ->
    lists:map(fun($-) -> $=;
                 ($_) -> $+;
                 ($,) -> $/;
                 (C)  -> C
              end,
              Str64).

%% @spec nest(string()) -> [string()]
%% @doc create a directory nesting, to keep the number of
%%      files in a directory smaller
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
   ?assertCmd("rm -rf test/fs-backend"),
   Config = [{riak_fs_backend_root, "test/fs-backend"}],
   riak_test_util:standard_backend_test(riak_fs_backend, Config).

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
