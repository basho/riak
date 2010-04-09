-module(riak_kv_bitcask_backend).
-author('Andy Gross <andy@basho.com').

-export([start/2, stop/1, get/2, put/3, list/1, list_bucket/2, delete/2]).
-export([drop/1, is_empty/1, fold/3]).
-record(state, {partition, bcstate}).

start(Partition, Config) ->
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
    erlang:put(?MODULE, #state{partition=Partition, bcstate=BCState}),
    {ok, none}.    

get(_, BKey) ->
    State = erlang:get(?MODULE),
    Key = term_to_binary(BKey),
    case bitcask:get(State#state.bcstate, Key) of
        {ok, Value, NewBCState} ->
            erlang:put(?MODULE, State#state{bcstate=NewBCState}),
            {ok, Value};
        {not_found, NewBCState} ->
            erlang:put(?MODULE, State#state{bcstate=NewBCState}),
            {error, notfound}
    end.

put(_, BKey, Val) ->
    State = erlang:get(?MODULE),
    Key = term_to_binary(BKey),
    {Reply, NewBCState} = bitcask:put(State#state.bcstate, Key, Val),
    erlang:put(?MODULE, State#state{bcstate=NewBCState}),
    Reply.
    
list(_) -> 
    State = erlang:get(?MODULE),
    case bitcask:list_keys(State#state.bcstate) of
        [KeyList] ->
            [binary_to_term(K) || K <- KeyList];
        Other -> Other
    end.

list_bucket(_, Bucket) ->  [{B,K} || {B,K} <- ?MODULE:list(none), B =:= Bucket].

delete(_, BKey) -> 
    State = erlang:get(?MODULE),
    {Reply, NewBCState} = bitcask:delete(State#state.bcstate, term_to_binary(BKey)),
    erlang:put(?MODULE, State#state{bcstate=NewBCState}),
    Reply.

drop(_) -> ok.

is_empty(_) -> false.

fold(_, _Fun, _InitAcc) -> [].

stop(_) -> 
    State = erlang:get(?MODULE),
    bitcask:close(State#state.bcstate).





