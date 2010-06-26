-module(riak_core_handoff_manager).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([add_exclusion/2, get_handoff_lock/1, get_exclusions/1]).
-export([release_handoff_lock/2]).
-record(state, {excl}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{excl=ordsets:new()}}.

add_exclusion(Module, Index) ->
    gen_server:cast(?MODULE, {add_exclusion, {Module, Index}}).

get_exclusions(Module) ->
    gen_server:call(?MODULE, {get_exclusions, Module}).

get_handoff_lock(LockId) ->
    TokenCount = app_helper:get_env(riak_core, handoff_concurrency, 4),
    get_handoff_lock(LockId, TokenCount).

get_handoff_lock(_LockId, 0) ->
    {error, max_concurrency};
get_handoff_lock(LockId, Count) ->
    case global:set_lock({{handoff_token, Count}, {node(), LockId}}, [node()], 0) of
        true ->
            {ok, {handoff_token, Count}};
        false ->
            get_handoff_lock(LockId, Count-1)
    end.    

release_handoff_lock(LockId, Token) ->
    global:del_lock({Token, {node(), LockId}}, [node()]).
    
handle_call({get_exclusions, Module}, _From, State=#state{excl=Excl}) ->
    Reply =  [I || {M, I} <- ordsets:to_list(Excl), M =:= Module],
    {reply, {ok, Reply}, State}.

handle_cast({add_exclusion, {Mod, Idx}}, State=#state{excl=Excl}) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    riak_core_ring_events:ring_update(Ring),
    {noreply, State#state{excl=ordsets:add_element({Mod, Idx}, Excl)}}.    

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

