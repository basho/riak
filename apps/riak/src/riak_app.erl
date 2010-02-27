%% -------------------------------------------------------------------
%%
%% riak_app: application startup for Riak
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

%% @doc application startup for Riak

-module(riak_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->

    %% Look at the epoch and generating an error message if it doesn't match up to our expectations
    check_epoch(),
    
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
    case filelib:is_dir(RingStateDir) of
        true ->
            ok;
        false ->
            error_logger:error_msg("Ring state directory ~p does not exist.\n", [RingStateDir]),
            throw({error, invalid_ring_state_dir})
    end,

    %% Make sure required modules are available
    %% TODO: Is this really necessary?!
    check_deps(),

    %% Spin up supervisor
    case riak_sup:start_link() of
        {ok, Pid} ->
            %% App is running; search for latest ring file and initialize with it
            riak_ring_manager:prune_ringfiles(),
            case riak_ring_manager:find_latest_ringfile() of
                {ok, Ring} ->
                    riak_ring_manager:set_my_ring(riak_ring_manager:read_ringfile(Ring));
                {error, not_found} ->
                    error_logger:warning_msg("No ring file available.\n");
                {error, Reason} ->
                    error_logger:error_msg("Failed to load ring file: ~p\n", [Reason]),
                    throw({error, Reason})
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


%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).

%% @spec check_epoch() -> ok
%% @doc 
check_epoch() ->
    %% doc for erlang:now/0 says return value is platform-dependent
    %% -> let's emit an error if this platform doesn't think the epoch
    %%    is Jan 1, 1970
    {MSec, Sec, _} = erlang:now(),
    GSec = calendar:datetime_to_gregorian_seconds(
             calendar:universal_time()),
    case GSec - ((MSec*1000000)+Sec) of
        N when (N < ?SEC_TO_EPOCH+5 andalso N > ?SEC_TO_EPOCH-5);
        (N < -?SEC_TO_EPOCH+5 andalso N > -?SEC_TO_EPOCH-5) ->
            %% if epoch is within 10 sec of expected, accept it
            ok;
        N ->
            Epoch = calendar:gregorian_seconds_to_datetime(N),
            error_logger:error_msg("Riak expects your system's epoch to be Jan 1, 1970,~n"
                                   "but your system says the epoch is ~p~n", [Epoch]),
            ok
    end.
