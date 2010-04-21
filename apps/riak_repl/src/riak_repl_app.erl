-module(riak_repl_app).
-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak_repl.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    ReplHook = {struct, [{<<"mod">>, <<"riak_repl_logger">>},
                         {<<"fun">>, <<"postcommit">>}]},
    {ok, DefaultBucketProps} = application:get_env(riak_core, 
                                                   default_bucket_props),
    application:set_env(riak_core, default_bucket_props, 
                        proplists:delete(postcommit, DefaultBucketProps)),
    riak_core_bucket:append_bucket_defaults([{postcommit, [ReplHook]}]),
    riak_repl_sup:start_link().

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak_repl.
stop(_State) -> ok.


