-module(riak_phase_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_reduce_phase/4, new_map_phase/4]).
-export([new_map_executor/4]).

%% Supervisor callbacks
-export([init/1]).

new_reduce_phase(_Ring, QTerm, NextFSM, Requestor) ->
    start_child(riak_reduce_phase_fsm, [QTerm, NextFSM, Requestor]).

new_map_phase(Ring, QTerm, NextFSM, Requestor) ->
    start_child(riak_map_phase_fsm, [Ring, QTerm, NextFSM, Requestor]).

new_map_executor(Ring, Input, QTerm, Requestor) ->
    start_child(riak_map_executor, [Ring, Input, QTerm, Requestor]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    Process = {undefined,
               {gen_fsm, start_link, []},
               transient, brutal_kill, worker, dynamic},
    {ok, {SupFlags, [Process]}}.

%% Internal functions
start_child(ModName, Args) ->
    case supervisor:start_child(?MODULE, [ModName, Args, []]) of
        {ok, Pid} ->
            MRef = erlang:monitor(process, Pid),
            {ok, MRef, Pid};
        Error ->
            Error
    end.
