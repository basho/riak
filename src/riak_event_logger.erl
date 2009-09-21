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

%% @doc 
%% riak_event_logger is an example of how to connect to a 
%% running Riak cluster to receive events.

-module(riak_event_logger).
-behavior(gen_server).

-define (RECONNECT_INTERVAL, 200).
-define (SERVER, ?MODULE).
-record (state, {node, pid, fd}).
-export ([start/2, start_link/2]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @private
start(Node, Filename) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Node, Filename], []).

start_link(Node, Filename) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Node, Filename], []).

%% @private
init([Node, Filename]) -> 
    % If this gen_server dies and is supervised, then it will
    % be restarted under a new pid. If this happens, then we will
    % lose our connection to the Riak cluster. So, send a keepalive
    % every few seconds to reconnect.
    timer:apply_interval(?RECONNECT_INTERVAL, gen_server, call, [?SERVER, connect]),
    
    % Open the file, get the file descriptor....
    {ok, FD} = case Filename of
        _ when Filename == "stdout" orelse Filename == "" -> 
            {ok, stdout};
        _ ->
            {ok, CWD} = file:get_cwd(),
            LogFN = filename:join([CWD,Filename]),
            ok = filelib:ensure_dir(LogFN),
            io:format("Writing event log to ~p~n",[LogFN]),
            file:open(LogFN, [raw, append, delayed_write])
    end,
    
    State = #state {
        node = Node,
        fd = FD
    },
    {ok, State}.
    
%% @private
%% Check if we need to reconnect to the Riak cluster.
handle_call(connect, _From, State) ->
    PidHasChanged = State#state.pid /= self(),
    case PidHasChanged of
        true ->  register_for_events(State);
        false -> ignore
    end,
    {reply, ok, State#state { pid=self() }};
    
handle_call(_, _, State) -> {ok, State}.

handle_cast(_, State) -> {noreply, State}.

%% @private
%% Got an incoming event. Write it to a file or to the console.
handle_info({event, Event}, State) ->
    case State#state.fd of
        stdout ->
            io:format("~s",[fmtnow()]),
            io:format(": ~p~n",[Event]);
        FD ->
            file:write(FD, [fmtnow()]),
            file:write(FD, io_lib:format(": ~p~n",[Event]))
    end,
    {noreply, State};
    
handle_info(_, State) -> {noreply, State}.

%% @private
terminate(swap, State)  -> {?MODULE, State};
terminate(_Reason,_State)  -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

register_for_events(State) ->
    % Get the client...
    {ok, C} = riak:client_connect(State#state.node),

    % Attach the eventer...
    Desc = io_lib:format("~s (~s)", [?SERVER, node()]),
    C:add_event_handler(self(), Desc, {'_', '_', '_', '_'}, []).


%%% DATE FUNCTIONS %%%

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

