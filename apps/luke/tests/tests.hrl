%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-define(TWO_PHASE_FLOW, [{simple_phase, none, []},
                         {simple_phase, accumulate, []}]).

-define(TWO_ASYNC_FLOW, [{async_phase, none, []},
                         {async_phase, accumulate, []}]).

-define(MAP_FLOW, [{map_phase, accumulate, []}]).
-define(MAP_DBL_FLOW, [{map_phase, accumulate, []},
                       {map_phase, accumulate, []}]).
-define(MAPRED_FLOW, [{map_phase, none, []},
                      {reduce_phase, {converge, 3}, []}]).
-define(MAPRED_FLOW1, [{map_phase, none, []},
                       {reduce_phase, {converge, 1}, []}]).
