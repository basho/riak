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
            {error, invalid_default_bucket_props}
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

    riak_sup:start_link().

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
