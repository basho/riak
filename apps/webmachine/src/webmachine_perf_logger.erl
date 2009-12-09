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

-module(webmachine_perf_logger).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([log/1, refresh/0]).
-include("webmachine_logger.hrl").
-record(state, {hourstamp, filename, handle}).

alog_path(BaseDir) ->
    filename:join(BaseDir, "perf.log").

start_link(BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BaseDir], []).

init([BaseDir]) ->
    defer_refresh(),
    FileName = alog_path(BaseDir),
    DateHour = datehour(),
    filelib:ensure_dir(FileName),
    Handle = log_open(FileName, DateHour),
    {ok, #state{filename=FileName, handle=Handle, hourstamp=DateHour}}.

refresh() ->
    refresh(now()).

refresh(Time) ->
    gen_server:cast(?MODULE, {refresh, Time}).

log(#wm_log_data{}=D) ->
    gen_server:call(?MODULE, {log, D}).

handle_call({log, LogData}, _From, State) ->
    NewState = maybe_rotate(State, now()),
    Msg = format_req(LogData),
    log_write(NewState#state.handle, Msg),
    {reply, ok, NewState}.

handle_cast({refresh, Time}, State) ->
    {noreply, maybe_rotate(State, Time)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log_open(FileName, DateHour) ->
    LogName = FileName ++ suffix(DateHour),
    io:format("opening log file: ~p~n", [LogName]),
    {ok, FD} = file:open(LogName, [read, write, raw]),
    {ok, Location} = file:position(FD, eof),
    fix_log(FD, Location),
    file:truncate(FD),
    {?MODULE, LogName, FD}.

log_write({?MODULE, _Name, FD}, IoData) ->
    file:write(FD, lists:flatten(IoData)).
    

log_close({?MODULE, Name, FD}) ->
    io:format("~p: closing log file: ~p~n", [?MODULE, Name]),
    file:close(FD).

maybe_rotate(State, Time) ->
    ThisHour = datehour(Time),
    if ThisHour == State#state.hourstamp ->
	    State;
       true ->
	    defer_refresh(),
	    log_close(State#state.handle),
	    Handle = log_open(State#state.filename, ThisHour),
	    State#state{hourstamp=ThisHour, handle=Handle}
    end.    

format_req(#wm_log_data{resource_module=Mod,
			start_time=StartTime,
			method=Method, 
			peer=Peer, 
			path=Path,
			version=Version,
			response_code=ResponseCode,
			response_length=ResponseLength,
		        end_time=EndTime,
		        finish_time=FinishTime}) ->
    Time = fmtnow(),
    Status = integer_to_list(ResponseCode),
    Length = integer_to_list(ResponseLength),
    TTPD = webmachine_util:now_diff_milliseconds(EndTime, StartTime),
    TTPS = webmachine_util:now_diff_milliseconds(FinishTime, EndTime),
    fmt_plog(Time, Peer, atom_to_list(Method), Path, Version,
	     Status, Length, atom_to_list(Mod), integer_to_list(TTPD),
	     integer_to_list(TTPS)).

%% Seek backwards to the last valid log entry
fix_log(_FD, 0) ->
    ok;
fix_log(FD, 1) ->
    {ok, 0} = file:position(FD, 0),
    ok;
fix_log(FD, Location) ->
    case file:pread(FD, Location - 1, 1) of
	{ok, [$\n | _]} ->
	    ok;
	{ok, _} ->
	    fix_log(FD, Location - 1)
    end.

defer_refresh() ->
    {_, {_, M, S}} = calendar:universal_time(),
    Time = 1000 * (3600 - ((M * 60) + S)),
    timer:apply_after(Time, ?MODULE, refresh, []).

datehour() ->
    datehour(now()).

datehour(Now) ->
    {{Y, M, D}, {H, _, _}} = calendar:now_to_universal_time(Now),
    {Y, M, D, H}.

zeropad_str(NumStr, Zeros) when Zeros > 0 ->
    zeropad_str([$0 | NumStr], Zeros - 1);
zeropad_str(NumStr, _) ->
    NumStr.

zeropad(Num, MinLength) ->
    NumStr = integer_to_list(Num),
    zeropad_str(NumStr, MinLength - length(NumStr)).

suffix({Y, M, D, H}) ->
    YS = zeropad(Y, 4),
    MS = zeropad(M, 2),
    DS = zeropad(D, 2),
    HS = zeropad(H, 2),
    lists:flatten([$., YS, $_, MS, $_, DS, $_, HS]).

fmt_plog(Time, Ip,  Method, Path, {VM,Vm}, Status, Length, Mod, TTPD, TTPS) ->
    [fmt_ip(Ip), " - ", [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, " " , Mod, " ", TTPD, " ", TTPS, $\n].

month(1) ->
    "Jan";
month(2) ->
    "Feb";
month(3) ->
    "Mar";
month(4) ->
    "Apr";
month(5) ->
    "May";
month(6) ->
    "Jun";
month(7) ->
    "Jul";
month(8) ->
    "Aug";
month(9) ->
    "Sep";
month(10) ->
    "Oct";
month(11) ->
    "Nov";
month(12) ->
    "Dec".
zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(Time),
    zone((DiffSecs/3600)*100).

%% Ugly reformatting code to get times like +0000 and -1300

zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).

fmt_ip(IP) when is_tuple(IP) ->
    inet_parse:ntoa(IP);
fmt_ip(undefined) ->
    "0.0.0.0";
fmt_ip(HostName) ->
    HostName.

fmtnow() ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
		  [Date,month(Month),Year, Hour, Min, Sec, zone()]).
