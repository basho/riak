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
-export([start/1,stop/1,get/2,put/3,list/1,delete/2]).
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
get(State, Key) ->
    File = location(State,Key),
    case filelib:is_file(File) of
        false -> {error, notfound};
        true -> file:read_file(File)
    end.

% put(state(), Key :: binary(), Val :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
put(State,Key,Val) ->       
    File = location(State,Key),
    case filelib:ensure_dir(File) of
        ok -> file:write_file(File,Val);
        X -> X
    end.

% delete(state(), Key :: binary()) ->
%   ok | {error, Reason :: term()}
% key must be 160b
delete(State, Key) ->
    File = location(State,Key),
    case file:delete(File) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Err} -> {error, Err}
    end.

% list(state()) -> [Key :: binary()]
list(State) ->
    % this is slow slow slow
    [location_to_key(X) || X <- filelib:wildcard("*/*/*/*/*/*/*/*/*/*",
                                                 State#state.dir)].

location_to_key(Path) ->
    % Path is a list of strings giving the path beneath Dir to a file
    list_to_binary([list_to_integer(A) || A <- 
      lists:append([string:tokens(X,".") || X <- string:tokens(Path,"/")])]).

% location(state(), Key :: binary()) -> Path :: string()
location(State, Key) ->
    location(Key,State#state.dir,
             [integer_to_list(X) || X <- binary_to_list(Key)]).
location(_Key,Dir,[A|[B|[]]]) ->
    filename:join([Dir,A++"."++B]);
location(Key,Dir,[A|[B|Rest]]) ->
    location(Key,filename:join([Dir,A++"."++B]),Rest).
