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

-module(riak_event_logger).
-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).
-export([code_change/3]).

%% @private
init({Arg,_}) ->
    case Arg of
        "stdout" -> {ok, "stdout"};
        _ ->
            {ok, CWD} = file:get_cwd(),
            LogFN = filename:join([CWD,Arg]),
            ok = filelib:ensure_dir(LogFN),
            io:format("Writing event log to ~p~n",[LogFN]),
            file:open(LogFN, [raw, append, delayed_write])
    end.

%% @private
handle_event(Event, "stdout") ->
    io:format("~s",[fmtnow()]),
    io:format(": ~p~n",[Event]),
    {ok, "stdout"};
handle_event(Event, FD) ->
    file:write(FD, [fmtnow()]),
    file:write(FD, io_lib:format(": ~p~n",[Event])),
    {ok, FD}.

%% @private
handle_call(_, State) -> {ok, no_call_support, State}.

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

