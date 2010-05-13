%% -------------------------------------------------------------------
%%
%% riak_core: Core Riak Application
%%
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

-module(riak_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Validate that the ring state directory exists
    RingStateDir = app_helper:get_env(riak_core, ring_state_dir),
    case filelib:ensure_dir(filename:join(RingStateDir, "dummy")) of
        ok ->
            ok;
        {error, RingReason} ->
            error_logger:error_msg(
              "Ring state directory ~p does not exist, "
              "and could not be created. (reason: ~p)\n",
              [RingStateDir, RingReason]),
            throw({error, invalid_ring_state_dir})
    end,

    %% Spin up the supervisor; prune ring files as necessary
    case riak_core_sup:start_link() of
        {ok, Pid} ->
            %% App is running; search for latest ring file and initialize with it
            riak_core_ring_manager:prune_ringfiles(),
            case riak_core_ring_manager:find_latest_ringfile() of
                {ok, RingFile} ->
                    Ring = riak_core_ring_manager:read_ringfile(RingFile),
                    riak_core_ring_manager:set_my_ring(Ring);
                {error, not_found} ->
                    error_logger:warning_msg("No ring file available.\n");
                {error, Reason} ->
                    error_logger:error_msg("Failed to load ring file: ~p\n",
                                           [Reason]),
                    throw({error, Reason})
            end,
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
