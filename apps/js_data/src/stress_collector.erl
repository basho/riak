-module(stress_collector).

-behaviour(gen_server).

%% API
-export([start/1, log/2, test_complete/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {fd, counter}).

log(Success, Error) ->
    gen_server:cast(?SERVER, {log, Success, Error}).

test_complete() ->
    gen_server:cast(?SERVER, test_complete).

start(FileName) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [FileName], []).

init([FileName]) ->
    {ok, Fd} = file:open(FileName, [write]),
    file:write(Fd, io_lib:format("TestId,Success,Error~n", [])),
    {ok, #state{fd=Fd, counter=1}}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(test_complete, #state{fd=Fd}=State) ->
    file:close(Fd),
    {stop, normal, State};
handle_cast({log, SuccessTime, ErrorTime}, #state{fd=Fd, counter=Counter}=State) ->
    file:write(Fd, io_lib:format("~s,~s,~s~n", [integer_to_list(Counter), integer_to_list(SuccessTime),
                                                integer_to_list(ErrorTime)])),
    {noreply, State#state{counter=Counter + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
