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
-export([read_config/0, read_config/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    case riak:get_app_env(no_config) of
        true -> nop; % promising to set all env variables some other way
        _ -> read_config()
    end,
    register(riak_app, self()),
    erlang:set_cookie(node(), riak:get_app_env(riak_cookie)),
    riak_sup:start_link().

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak.
stop(_State) -> ok.


%% @spec read_config() -> ok
%% @doc Read the riak erlenv configuration file and set environment variables.
read_config() -> read_config(riak:get_app_env(configpath)).
%% @spec read_config(ConfigPath :: list()) -> ok
%% @doc Read the riak erlenv configuration file with filename ConfigPath
%%      and set environment variables.
read_config(ConfigPath) ->
    ConfigPairs = 
        case file:consult(ConfigPath) of
            {ok, Terms} -> Terms;
            {error, Reason} ->
                error_logger:info_msg("Failed to read config from: ~p (~p)~n",
                                      [ConfigPath, Reason]),
                []
	end,
    set_erlenv(ConfigPairs),
    check_erlenv(ConfigPath),
    [code:add_path(Path) || Path <- riak:get_app_env(add_paths)],
    [application:start(App) || App <- riak:get_app_env(start_apps)],
    StorageBackend = riak:get_app_env(storage_backend),
    case code:ensure_loaded(StorageBackend) of
        {error,nofile} ->
            riak:stop(lists:flatten(io_lib:format(
                        "storage_backend ~p in ~p non-loadable, failing.",
                        [StorageBackend, ConfigPath])));
        _ ->
            ok
    end,
    ok.

%% @private
set_erlenv([]) ->
    ok;
%% @private
set_erlenv([{K, V}|T]) when is_atom(K) ->
    application:set_env(riak, K, V),
    error_logger:info_msg("set env variable ~p:~p~n",[K,V]),
    set_erlenv(T).

%% @private
check_erlenv(ConfigPath) ->
    % verify presence and correctness of some necessary configuration fields
    % set some defaults for undefined fields
    [ClusterName,RingStateDir,RingCreationSize,
     WantsClaimFun,ChooseClaimFun,GossipInterval,
     DoorbellPort,StorageBackend,RiakCookie,RiakAddPaths,
     RiakNodeName,RiakHostName,RiakHeartCommand,
     DefaultBucketProps,RiakStartApps] =
        [riak:get_app_env(X) || X <- 
           [cluster_name,ring_state_dir,ring_creation_size,
            wants_claim_fun,choose_claim_fun,gossip_interval,
            doorbell_port,storage_backend,riak_cookie,add_paths,
            riak_nodename,riak_hostname,riak_heart_command,
            default_bucket_props,start_apps]],
    if
        ClusterName =:= undefined ->
            riak:stop(io_lib:format(
                        "cluster_name unset in ~p, failing.",[ConfigPath]));
        true -> ok
    end,
    if
        RingStateDir =:= undefined ->
            error_logger:info_msg(
              "ring_state_dir unset in ~p, setting to priv/ringstate~n",
              [ConfigPath]),
            application:set_env(riak, ring_state_dir, "priv/ringstate");
        true -> ok
    end,
    if
        RingCreationSize =:= undefined ->
            error_logger:info_msg(
              "ring_creation_size unset in ~p, setting to 1024~n",
              [ConfigPath]),
            application:set_env(riak, ring_creation_size, 1024);
        is_integer(RingCreationSize) =:= false ->
            riak:stop(io_lib:format(
                "ring_creation_size set to ~p in ~p, must be integer",
                          [RingCreationSize,ConfigPath]));
        true -> ok
    end,
    if
        WantsClaimFun =:= undefined ->
            application:set_env(riak, wants_claim_fun,
                                {riak_claim, default_wants_claim});
        true -> ok
    end,
    if
        ChooseClaimFun =:= undefined ->
            application:set_env(riak, choose_claim_fun,
                                {riak_claim, default_choose_claim});
        true -> ok
    end,
    if
        GossipInterval =:= undefined ->
            error_logger:info_msg(
              "gossip_interval unset in ~p, setting to 60000~n",
              [ConfigPath]),
            application:set_env(riak, gossip_interval, 60000);
        is_integer(GossipInterval) =:= false ->
            riak:stop(io_lib:format(
                "gossip_interval set to ~p in ~p, must be integer",
                          [GossipInterval,ConfigPath]));
        true -> ok
    end,
    if
        DoorbellPort =:= undefined ->
            io_lib:format(
                  "doorbell_port unset in ~p, no UDP service.",[ConfigPath]);
        is_integer(DoorbellPort) =:= false ->
            riak:stop(io_lib:format(
                "doorbell_port set to ~p in ~p, must be integer",
                          [DoorbellPort,ConfigPath]));
        true -> ok
    end,
    if
        StorageBackend =:= undefined ->
            nop; % but you had better know what you're doing!
        not is_atom(StorageBackend) ->
            riak:stop(io_lib:format(
                    "storage_backend in ~p non-atom, failing.",[ConfigPath]));
        true -> ok
    end,
    if
        RiakCookie =:= undefined ->
            riak:stop(io_lib:format(
                       "riak_cookie unset in ~p, failing.",[ConfigPath]));
        not is_atom(RiakCookie) ->
            riak:stop(io_lib:format(
                       "riak_cookie in ~p non-atom, failing.",[ConfigPath]));
        true -> ok
    end,
    if
        RiakAddPaths =:= undefined ->
            error_logger:info_msg(
              "add_paths unset in ~p, setting to []~n",
              [ConfigPath]),
            application:set_env(riak, add_paths, []);
        not is_list(RiakAddPaths) ->
            riak:stop(io_lib:format(
                       "add_paths in ~p non-list, failing.",[ConfigPath]));
        true -> ok
    end,
    if
        % this one has to stop hard, as we test it before starting the app
        RiakNodeName =:= undefined ->
            io:format("no_riak_nodename_"),
            init:stop();
        not is_atom(RiakNodeName) ->
            io:format("no_riak_nodename_"),
            init:stop();
        true -> ok
    end,
    if
        % this one has to stop hard, as we test it before starting the app
        RiakHostName =:= undefined ->
            io:format("no_riak_hostname_"),
            init:stop();
        not is_list(RiakHostName) ->
            io:format("no_riak_hostname_"),
            init:stop();
        true -> ok
    end,
    if
        RiakHeartCommand =:= undefined ->
            application:set_env(riak, riak_heart_command, "fail_to_restart");
        not is_list(RiakHeartCommand) ->
            riak:stop(io_lib:format(
                 "riak_heart_command in ~p non-list, failing.",[ConfigPath]));
        true -> ok
    end,
    if
        DefaultBucketProps =:= undefined ->
            set_bucket_params([]);
        is_list(DefaultBucketProps) ->
            set_bucket_params(DefaultBucketProps);
        true ->
            riak:stop(io_lib:format(
                        "default_bucket_props in ~p non-list, failing.",
                        [ConfigPath]))
    end,
    if 
        RiakStartApps =:= undefined ->
            error_logger:info_msg(
              "start_apps unset in ~p, setting to []~n",
              [ConfigPath]),
            application:set_env(riak, start_apps, []);
        not is_list(RiakStartApps) ->
            riak:stop(io_lib:format(
                        "start_apps in ~p non-list, failing.",[ConfigPath]));
        true -> ok
    end,
    ok.

set_bucket_params(In) ->
    application:set_env(
      riak, default_bucket_props,
      lists:ukeymerge(1,
                      lists:keysort(1, lists:keydelete(name, 1, In)),
                      lists:keysort(1, riak_bucket:defaults()))).
