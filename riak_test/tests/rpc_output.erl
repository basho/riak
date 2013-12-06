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
-module(rpc_output).

-behavior(riak_test).
-export([confirm/0]).

-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-behavior(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
         
confirm() ->
    gen_event:add_handler(lager_event, ?MODULE, []),
    io:put_chars("This is an io:put_chars/1 call"),
    io:format("This is an io:format/1 call"),
    io:format("This is an io:format/~w call", [2]),
    lager:info("This is a lager message"),
    {ok, {LogId, Failures}} = gen_event:delete_handler(lager_event, ?MODULE, []),
    ?assertEqual(5, LogId),
    ?assertEqual([], Failures).

-record(state, {level = debug, verbose = true, log_id = 1, failures = []}).

init(_) -> {ok, #state{}}.
handle_event({log, _Dest, _Level, {_Date, _Time}, [_LevelStr, _Location, Message]},
             State) ->
    check_log_message(lists:flatten(Message), State);
handle_event({log, _Level, {_Date, _Time}, [_LevelStr, _Location, Message]}, State) ->
    check_log_message(lists:flatten(Message), State);
handle_event(_, State) ->
    {ok, State}.
    
handle_call(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{log_id = LogId, failures = Failures}) ->
    {ok, {LogId, Failures}}.
    
check_log_message(Message, #state{log_id = LogId, failures = Failures} = State) ->
    try
        case LogId of
            1 -> ?assertEqual(Message, "This is an io:put_chars/1 call");
            2 -> ?assertEqual(Message, "This is an io:format/1 call");
            3 -> ?assertEqual(Message, "This is an io:format/2 call");
            4 -> ?assertEqual(Message, "This is a lager message");
            _ -> ?assert(false)
        end,
        {ok, State#state{log_id = LogId + 1}}
    catch
        _:Reason -> {ok, State#state{log_id = LogId + 1, failures = [Reason|Failures]}}
    end.            