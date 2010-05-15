%% Ccopyright 2007-2009 Basho Technologies, Inc. All Rights Reserved.
-module(riak_repl_connector_sup).
-author('Andy Gross <andy@basho.com>').
-behaviour(supervisor).
-export([start_link/0, init/1, stop/1]).
-export([start_connector/3]).

start_connector(IP, Port, Site) ->
    error_logger:info_msg("Starting connector for site ~p (~s:~p)~n",
                          [Site, IP, Port]),
    supervisor:start_child(?MODULE, [IP, Port, Site]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok, 
     {{simple_one_for_one, 10, 10}, 
      [{undefined,
        {riak_repl_connector, start_link, []},
        temporary, brutal_kill, worker, [riak_repl_connector]}]}}.
