%% -------------------------------------------------------------------
%%
%% riak_local_logger: logging in basic text file form
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

%% @doc logging in basic text file form

-module(riak_local_logger).
-behavior(gen_server2).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    case riak:get_app_env(riak_local_logfile) of
        undefined ->
            {ok, no_logger};
        FN ->
            {ok, CWD} = file:get_cwd(),
            LogFN = filename:join([CWD,FN]),
            ok = filelib:ensure_dir(LogFN),
            file:open(LogFN, [raw, append, delayed_write])
    end.

%% @private
handle_cast(_EventEntry, no_logger) -> {noreply,no_logger};
handle_cast({event, EventEntry}, FD) ->
    file:write(FD, [fmtnow()]),
    file:write(FD, io_lib:format(": ~p~n",[EventEntry])),
    {noreply,FD}.

%% @private
handle_call(_, _From, State) -> {noreply, State}.

%% @private
handle_info(_, State) -> {ok, State}.

%% @private
terminate(swap, State)  -> {?MODULE, State};
terminate(_Reason,_State)  -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

month(1) ->  "Jan";
month(2) ->  "Feb";
month(3) ->  "Mar";
month(4) ->  "Apr";
month(5) ->  "May";
month(6) ->  "Jun";
month(7) ->  "Jul";
month(8) ->  "Aug";
month(9) ->  "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(Time),
    zone((DiffSecs/3600)*100).

zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).

fmtnow() ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
                  [Date,month(Month),Year, Hour, Min, Sec, zone()]).

