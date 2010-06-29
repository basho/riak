%% -------------------------------------------------------------------
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
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
-module(process_proxy).
-export([start_link/2, init/1, stop/1]).

start_link(RegName, ProxyTo) ->
    proc_lib:start_link(?MODULE, init, [[self(), RegName, ProxyTo]]).

init([ParentPid, RegName, ProxyTo]) ->
    erlang:register(RegName, self()),
    proc_lib:init_ack(ParentPid, {ok, self()}),
    loop(ProxyTo).

stop(Name) ->
    Name ! stop.

loop(ProxyTo) ->
    receive
        stop ->
            exit(normal);
        M ->
            ProxyTo ! M,
            loop(ProxyTo)
    end.
