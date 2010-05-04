-module(riak_repl_sup).
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
    Processes = [{riak_repl_events,
                  {riak_repl_events, start_link, []},
                  permanent, 5000, worker, dynamic},
                 {riak_repl_config,
                  {riak_repl_config, start_link, []},
                  permanent, 5000, worker, [riak_repl_config]},
                 {riak_repl_sink,
                  {riak_repl_sink, start_link, []},
                  permanent, 5000, worker, [riak_repl_sink]},
                 {riak_repl_leader,
                  {riak_repl_leader, start_link, []},
                  permanent, 5000, worker, [riak_repl_leader]},
                 {riak_repl_connector,
                  {riak_repl_connector, start_link, []},
                  permanent, 5000, worker, [riak_repl_connector]}],
%                 {riak_repl_logger,
%                  {riak_repl_logger, start_link, []},
%                  permanent, 5000, worker, [riak_repl_logger]},
%                 {riak_repl_server,
%                  {riak_repl_server, start_link, []},
%                  permanent, 5000, worker, [riak_repl_server]}],
    {ok, {{one_for_one, 9, 10}, Processes}}.
