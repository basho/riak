%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(rt_worker_sup).

-behavior(supervisor).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Node, Backend, Vsn), { 
    list_to_atom(atom_to_list(Node) ++ "_loader_" ++ integer_to_list(Id)), 
    {   Mod, 
        start_link, 
        [list_to_atom(atom_to_list(Node) ++ "_loader_" ++ integer_to_list(Id)), Node, Backend, Vsn]}, 
        permanent, 5000, worker, [Mod]}).

-export([init/1]).
-export([start_link/1]).

start_link(Props) ->
    supervisor:start_link(?MODULE, Props).

init(Props) ->
    WorkersPerNode = proplists:get_value(concurrent, Props),
    Node = proplists:get_value(node, Props),
    Backend = proplists:get_value(backend, Props),
    Vsn = proplists:get_value(version, Props),

    ChildSpecs = [
        ?CHILD(Num, loaded_upgrade_worker_sup, Node, Backend, Vsn)
    || Num <- lists:seq(1, WorkersPerNode)],

    lager:info("Starting ~p workers to ~p", [WorkersPerNode, Node]),

    {ok, {{one_for_one, 1000, 60}, ChildSpecs}}.
