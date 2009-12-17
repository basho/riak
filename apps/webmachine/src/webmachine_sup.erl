%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Supervisor for the webmachine application.

-module(webmachine_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0, start_logger/1]).
-export([start_perf_logger/1]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_logger(BaseDir) ->
    case application:get_env(webmachine, webmachine_logger_module) of
        {ok, LoggerModule} ->
            ChildSpec = 
                {webmachine_logger,
                 {LoggerModule, start_link, [BaseDir]},
                 permanent, 5000, worker, dynamic},
            supervisor:start_child(?MODULE, ChildSpec);
        _ -> nop
    end.

start_perf_logger(BaseDir) ->
    ChildSpec = 
	{webmachine_perf_logger,
	 {webmachine_perf_logger, start_link, [BaseDir]},
	 permanent, 5000, worker, [webmachine_perf_logger]},
    supervisor:start_child(?MODULE, ChildSpec).

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
    Processes = [],
    {ok, {{one_for_one, 9, 10}, Processes}}.
