%% Riak EnterpriseDS
%% Copyright 2007-2009 Basho Technologies, Inc. All Rights Reserved.
-module(riak_repl_server_sup).
-author('Andy Gross <andy@basho.com>').
-behaviour(supervisor).
-include("riak_repl.hrl").
-export([start_link/0, init/1, stop/1]).
-export([start_server/2]).

start_server(Socket, Sitename) ->
    supervisor:start_child(?MODULE, [Socket, Sitename]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok, 
     {{simple_one_for_one, 10, 10}, 
      [{undefined,
        {riak_repl_tcp_server, start, []},
        temporary, brutal_kill, worker, [riak_repl_tcp_server]}]}}.
