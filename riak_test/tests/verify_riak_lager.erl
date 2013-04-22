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
-module(verify_riak_lager).

-behavior(riak_test).
-export([confirm/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(UNIX_RW_R__R__, 8#100644).

confirm() ->
    lager:info("Staring a node"),
    Nodes = [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),
    
    lager:info("Stopping that node"),
    rt:stop(Node),
    
    rt:start(Node),
    lager:info("Checking for log files"),
    
    {ok, LagerHandlers} = rt:rpc_get_env(Node, [{lager, handlers}]),
    
    Files = [element(1, Backend) || Backend <- proplists:get_value(lager_file_backend, LagerHandlers)],
    
    lager:info("Checking for files: ~p", [Files]),
    [?assert(rpc:call(Node, filelib, is_file, [File])) || File <- Files],
    
    FileInfos = [ FileInfo || {ok, FileInfo} <- [rpc:call(Node, file, read_file_info, [File]) || File <- Files]],
    
    [?assertEqual(?UNIX_RW_R__R__, ?UNIX_RW_R__R__ band FileInfo#file_info.mode) || FileInfo <- FileInfos],
    pass.
    