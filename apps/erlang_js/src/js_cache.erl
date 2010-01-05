-module(js_cache).

-behaviour(gen_server).

%% API
-export([start_link/0, store/2, delete/1, fetch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cache=gb_trees:empty()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(Key, Value) ->
  gen_server:cast(?SERVER, {store, Key, Value}).

delete(Key) ->
  gen_server:cast(?SERVER, {delete, Key}).

fetch(Key) ->
  gen_server:call(?SERVER, {fetch, Key}).

init([]) ->
  {ok, #state{}}.

handle_call({fetch, Key}, _From, #state{cache=Cache}=State) ->
  Result = case gb_trees:lookup(Key, Cache) of
             {value, Value} ->
               Value;
             Other ->
               Other
           end,
  {reply, Result, State};

handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

handle_cast({store, Key, Value}, #state{cache=Cache}=State) ->
  {noreply, State#state{cache=gb_trees:enter(Key, Value, Cache)}};

handle_cast({delete, Key}, #state{cache=Cache}=State) ->
  {noreply, State#state{cache=gb_trees:delete(Key, Cache)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
