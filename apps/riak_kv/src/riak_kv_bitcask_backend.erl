-module(riak_kv_bitcask_backend).
-behaviour(gen_server).
-author('Andy Gross <andy@basho.com').

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/2, stop/1, get/2, put/3, list/1, list_bucket/2, delete/2]).
-export([drop/1, is_empty/1, fold/3]).
-record(state, {partition, bcstate}).

start(Partition, Config) ->
    gen_server:start_link(?MODULE, [Partition, Config], []).

get(Pid, BKey) ->
    gen_server:call(Pid, {get, BKey}).

put(Pid, BKey, Val) ->
    gen_server:call(Pid, {put, BKey, Val}).    

list(Pid) ->
    gen_server:call(Pid, list).

list_bucket(Pid, Bucket) ->
    gen_server:call(Pid, {list_bucket, Bucket}).

delete(Pid, BKey) ->
    gen_server:call(Pid, {delete, BKey}).

drop(Pid) ->
    gen_server:call(Pid, drop).

is_empty(Pid) ->
    gen_server:call(Pid, is_empty).

fold(Pid, Fun, InitAcc) ->
    gen_server:call(Pid, {fold, Fun, InitAcc}).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Partition, Config]) ->
    BackendRoot = proplists:get_value(riak_kv_bitcask_backend_root, Config),
    if BackendRoot =:= undefined ->
            riak:stop("riak_kv_bitcask_backend_root undefined, failing.~n");
       true -> ok
    end,
    BackendDir = filename:join([BackendRoot, integer_to_list(Partition)]),
    case filelib:ensure_dir(BackendDir) of
        ok -> ok;
        _Error ->
            riak:stop("riak_kv_bitcask_backend could not ensure"
                      " the existence of its root directory")
    end,
    {ok, BCState} = bitcask:open(BackendDir, [{read_write, true}]),
    {ok, #state{partition=Partition, bcstate=BCState}}.

handle_call(stop, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({put, BKey, Val}, _From, State=#state{bcstate=BCState}) ->
    {Reply, NewBCState} = do_put(BCState, BKey, Val),
    {reply, Reply, State#state{bcstate=NewBCState}};
handle_call({get, BKey}, _From, State=#state{bcstate=BCState}) ->
    {Reply, NewBCState} = do_get(BCState, BKey),
    {reply, Reply, State#state{bcstate=NewBCState}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_put(BCState, BKey, Val) ->
    Key = term_to_binary(BKey),
    bitcask:put(BCState, Key, Val).

do_get(BCState, BKey) ->
    Key = term_to_binary(BKey),
    case bitcask:get(BCState, Key) of
        {ok, Value, NewBCState} ->
            {{ok, Value}, NewBCState};
        {not_found, NewBCState} ->
            {{error, notfound}, NewBCState}
    end.
