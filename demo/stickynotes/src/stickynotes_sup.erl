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
    setup_buckets(),
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,   
    JiakOptions = [{jiak_name, "jiak"},
                   {jiak_buckets, [notes,groups]}
                   |[{K, stickynotes:get_app_env(K)}
                     || K <- [riak_ip, riak_port, riak_cookie]]],
    Dispatch = [{["jiak",bucket], jiak_resource,
                 [{key_type, container}|JiakOptions]},
                {["jiak",bucket,key], jiak_resource,
                 [{key_type, item}|JiakOptions]},
                {["jiak",bucket,key,'*'], jaywalker_resource,
                 JiakOptions},
                {['*'], stickynotes_resource, ["priv/www/"]}],
    WebConfig = [
		 {ip, Ip},
		 {port, 8000},
                 {log_dir, "priv/log"},
		 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% @spec setup_buckets() -> ok
%% @doc notes and groups buckets need to have their linkfuns set
%%      before Jiak link walking will work properly on them
setup_buckets() ->
    case jiak:client_connect(
           stickynotes:get_app_env(riak_ip),
           stickynotes:get_app_env(riak_port),
           stickynotes:get_app_env(riak_cookie)) of
        {ok, C} ->
            C:set_bucket(notes, jiak:default_jiak_bucket_props()),
            C:set_bucket(groups, jiak:default_jiak_bucket_props());
        Error ->
            error_logger:error_msg(
              "Unable to connect to riak cluster at ~p:~p~n"
              "Error: ~p", [Error])
    end.
