%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the stickynotes application.

-module(stickynotes_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,   
    {ok, RiakConfig} = file:consult("riak-config.erlenv"),
    setup_buckets(RiakConfig),
    External = lists:flatten(
                 io_lib:format("http://~s:~b/jiak/", [Ip, 8000])),
    Internal = lists:flatten(
                 io_lib:format("http://~s:~b/~s/",
                               [proplists:get_value(riak_web_ip, RiakConfig),
                                proplists:get_value(riak_web_port, RiakConfig),
                                proplists:get_value(jiak_name, RiakConfig)])),
    ProxyRes = case stickynotes:get_app_env(use_ibrowse) of
                   true -> jiak_proxy_ibrowse;
                   _    -> jiak_proxy_inets
               end,
    Dispatch = [{["jiak",'*'], ProxyRes, {External, Internal}},
                {['*'], stickynotes_resource, ["priv/www/"]}],
    WebConfig = [{ip, Ip},
		 {port, 8000},
                 {log_dir, "priv/log"},
		 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% @spec setup_buckets(proplist()) -> ok
%% @doc notes and groups know how to deal with Riak siblings,
%%      so we'll turn on allow_mult for them
setup_buckets(RiakConfig) ->
    code:add_path("../../ebin"), %% riak ebin for jiak module
    case jiak:client_connect(
           proplists:get_value(riak_hostname, RiakConfig),
           proplists:get_value(doorbell_port, RiakConfig),
           proplists:get_value(riak_cookie, RiakConfig)) of
        {ok, C} ->
            C:set_bucket(<<"notes">>, [{bucket_mod, notes},{allow_mult, true}]),
            C:set_bucket(<<"groups">>, [{bucket_mod, groups},{allow_mult, true}]);
        Error ->
            error_logger:error_msg(
              "Unable to connect to riak cluster at ~p:~p"
              " with cookie ~p~nError: ~p",
              [proplists:get_value(riak_hostname, RiakConfig),
               proplists:get_value(doorbell_port, RiakConfig),
               proplists:get_value(riak_cookie, RiakConfig),
               Error])
    end.
