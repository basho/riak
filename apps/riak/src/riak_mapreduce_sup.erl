-module(riak_mapreduce_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_mapreduce_fsm/5]).

%% Supervisor callbacks
-export([init/1]).

new_mapreduce_fsm(Node, ReqId, Query, Timeout, Requestor) ->
    case supervisor:start_child({?MODULE, Node},  [riak_mapreduce_fsm, [ReqId, Query,
                                                                        Timeout, Requestor], []]) of
        {ok, Pid} ->
            {ok, {ReqId, Pid}};
        Error ->
            Error
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    Process = {undefined,
               {gen_fsm, start_link, []},
               temporary, brutal_kill, worker, dynamic},
    {ok, {SupFlags, [Process]}}.
