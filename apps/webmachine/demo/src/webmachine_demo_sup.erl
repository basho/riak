%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies

%% @doc Supervisor for the webmachine_demo application.

-module(webmachine_demo_sup).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch_map() ->
    [{["demo", '*'], webmachine_demo_resource, []},
     {["fs", '*'], demo_fs_resource, [{root, "/tmp/fs"}]}
    ].

%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    WebConfig = [
		 {ip, Ip},
                 {backlog, 1000},
		 {port, 8000},
                 {log_dir, "priv/log"},
		 {dispatch, dispatch_map()}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
