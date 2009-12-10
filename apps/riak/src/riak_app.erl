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

%% @doc Bootstrapping the Riak application.

-module(riak_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    %% Append user-provided code paths 
    case riak:get_app_env(add_paths) of
        List when is_list(List) ->
            ok = code:add_paths(List);
        _ ->
            ok
    end,

    %% Make sure default_bucket_props is setup properly
    DefaultBucketProps = riak:get_app_env(default_bucket_props),
    if
        DefaultBucketProps =:= undefined ->
            set_bucket_params([]);
        is_list(DefaultBucketProps) ->
            set_bucket_params(DefaultBucketProps);
        true ->
            error_logger:error_msg("default_bucket_props is not a list: ~p\n", [DefaultBucketProps]),
            throw({error, invalid_default_bucket_props})
    end,
    
    %% Check the storage backend
    StorageBackend = riak:get_app_env(storage_backend),
    case code:ensure_loaded(StorageBackend) of
        {error,nofile} ->
            error_logger:error_msg("storage_backend ~p is non-loadable.\n", [StorageBackend]),
            throw({error, invalid_storage_backend});
        _ ->
            ok
    end,

    %% Validate that the ring state directory exists
    RingStateDir = riak:get_app_env(ring_state_dir),
    case filelib:is_directory(RingStateDir) of
        true ->
            ok;
        false ->
            error_lgoger:error_msg("Ring state directory ~p does not exist.\n", [RingStateDir]),
            throw({error, invalid_ring_state_dir})
    end,

    %% Make sure required modules are available
    %% TODO: Is this really necessary?!
    check_deps(),

    %% Spin up supervisor
    io:format("Plain args: ~p\n", [init:get_plain_arguments()]),
    case riak_sup:start_link() of
        {ok, Pid} ->
            %% App is basically running. Now we need to either initialize a new
            %% ring (noop), join a new ring or rejoin existing ring (default)
            case ring_args(init:get_plain_arguments()) of
                new_ring ->
                    error_logger:info_msg("Starting new ring\n"),
                    ok;
                {join_ring, Node} ->
                    error_logger:info_msg("Attempting to join ring at ~p\n", [Node]),
                    riak_connect:send_ring(Node, node());
                rejoin ->
                    riak_ring_manager:prune_ringfiles(),
                    riak_ring_manager:set_my_ring(
                      riak_ring_manager:read_ringfile(
                        riak_ring_manager:find_latest_ringfile()))
            end,
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.
            
            

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak.
stop(_State) ->
    ok.


set_bucket_params(In) ->
    application:set_env(
      riak, default_bucket_props,
      lists:ukeymerge(1,
                      lists:keysort(1, lists:keydelete(name, 1, In)),
                      lists:keysort(1, riak_bucket:defaults()))).


%%
%% Scan init:get_plain_arguments/0 looking for what to do with
%% ring initialization
%%
ring_args([]) ->
    rejoin;
ring_args(["join", Node | _Rest]) ->
    {join, list_to_atom(Node)};
ring_args(["bootstrap" | _Rest]) ->
    new_ring;
ring_args([_ | Rest]) ->
    ring_args(Rest).


check_deps() ->
    % explicit list of external modules we should fail-fast on missing
    Deps = [vclock, chash, merkerl],
    Fails = [Fail || {Fail, {error,_}} <-
             [{Dep, code:ensure_loaded(Dep)} || Dep <- Deps]],
    case Fails of
        [] ->
            ok;
        _ ->
            throw({error, {missing_modules, Fails}})
    end.
