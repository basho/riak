%% -------------------------------------------------------------------
%%
%% cause_bdp - helper module used by verify_busy_dist_port
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

-module(cause_bdp).
-compile(export_all).

spam_nodes(TargetNodes) ->
        [[spawn(?MODULE, spam, [N]) || _ <- lists:seq(1,10*1000)] || N <- TargetNodes].

spam(Node) ->
    timer:sleep(random:uniform(1500)), 
    catch rpc:call(Node, erlang, whereis, [rex]).
