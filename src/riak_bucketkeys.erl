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

%% @doc Management of keylists for Riak buckets.
-module(riak_bucketkeys).
-behavior(gen_server2).

-export([put_key/2,
         del_key/2,
         get_keys/1]).
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(BUCK, ' bucketkeys').
-define(QSIZE, 1000).
-define(NFRAGS, 1024).
-define(FLUSH_INTERVAL, 1000).
-record(state, {ops, ring, tref}).

% State.ops is a dict, keys are bucketname atoms, vals are lists of operations,
% where operations are tuples of {Op::[add|del], riak_object:key()}

%% @spec put_key(riak_object:bucket(), riak_object:key()) -> ok
%% @doc Add Key to the keylist for Bucket.
put_key(Bucket,Key) -> 
    gen_server2:cast(?MODULE,{add,Bucket,Key}).

%% @spec del_key(riak_object:bucket(), riak_object:key()) -> ok
%% @doc Remove Key from the keylist for Bucket.
del_key(Bucket,Key) -> 
    gen_server2:cast(?MODULE,{del,Bucket,Key}).

%% @private
start_link() -> 
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], 
                           [{fullsweep_after, 0}]).

%% @private
init(_) -> 
    {ok, TRef} = timer:send_interval(1000, timeout),
    {ok, #state{ops=dict:new(),ring=undefined, tref=TRef}}.

handle_call(_,_,State) -> {reply,no_call_support,State}.

handle_cast({OpType,Bucket,Key},State=#state{ops=Ops}) ->
    BucketFrag = list_to_binary(
                   lists:flatten(
                     io_lib:format("~s-~b", 
                                   [Bucket, 
                                    erlang:phash2(Key) rem ?NFRAGS]))),
    NewState = ensure_ring(State),
    OpList = case dict:find(BucketFrag, Ops) of
        error ->   [{OpType,Key}];
        {ok, L} -> [{OpType,Key}|L]
    end,
    case length(OpList) > ?QSIZE of
        true ->
            do_write_all(dict:store(BucketFrag, OpList, Ops), 
                         NewState#state.ring),
            {noreply, NewState#state{ops=dict:new()}};
        false ->
            {noreply,NewState#state{ops=dict:store(BucketFrag,OpList,Ops)}}
    end.

%% @private
sort_contents([], Acc) ->
    [V || {_, V} <- lists:sort(Acc)];
sort_contents([{M,V}|T], Acc) ->
    LM = 
        case dict:find(<<"X-Riak-Last-Modified">>, M) of
            {ok, Val} -> Val;
            error -> httpd_util:rfc1123_date()
        end,
    sort_contents(
      T,
      [{calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(LM)), V}|Acc]
     ).


%% @private
replay_changes([], Set) -> Set;
replay_changes([{add,K}|T], Set) -> replay_changes(T, sets:add_element(K, Set));
replay_changes([{del,K}|T], Set) -> replay_changes(T, sets:del_element(K, Set)).

%% @private
do_write_all(Ops, Ring) ->
    [do_write_bucket(BucketOps, Ring) || BucketOps <- dict:to_list(Ops)].

%% @private
do_write_bucket({BucketName,OpList}, Ring) ->
    Obj = get_keysobj(BucketName, Ring),
    NewSet = merge_keysobj(Obj, OpList),
    NewV = {NewSet, OpList},
    NewObj = riak_object:update_value(Obj,NewV),
    put_keysobj(NewObj, Ring).

%% @private
get_keysobj(Bucket, Ring) ->
    fix_bucket(Ring),
    case gen_server2:call({riak_api,node()},
                          {get,?BUCK,Bucket,1,120000}) of
        {error, notfound} ->
            riak_object:new(?BUCK,Bucket,{sets:new(), []});
        {error, Err} -> {error, Err};
        {ok,Obj} -> Obj
    end.

%% @private
get_all_keyfrags(Bucket, Ring) ->
    fix_bucket(Ring),
    [get_keysobj(Frag, Ring) || Frag <- all_frags(Bucket)].

%% @private
merge_keysobj(KeysObj, NewReplays) ->
    Sorted = sort_contents(riak_object:get_contents(KeysObj), []),
    {Sets, Replays0} = lists:unzip(Sorted),
    UnionSet = sets:union(Sets), 
    AllReplays = lists:flatten(Replays0 ++ lists:reverse(NewReplays)),
    replay_changes(AllReplays, UnionSet).

%% @private
put_keysobj(KeysObj, Ring) ->
    fix_bucket(Ring),
    {ok, C} = riak:local_client(),
    C:put(KeysObj, 1, 1, 120000).

%% @spec get_keys(Bucket::atom()) -> 
%%               {ok, [riak_object:key()]} | {error, Reason::term()}
%% @doc Return the keylist for Bucket.
get_keys(Bucket) ->            
    % this one will cause a put if merge is needed
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    AllFrags = get_all_keyfrags(Bucket, Ring),
    FragErrs = [F || F <- AllFrags, element(1, F) =:= error],
    case FragErrs of
        [] ->
            {ok, merge_frags(AllFrags, Ring, sets:new())};
        [E|_] -> E
    end.
    
%% @private
merge_frags([], _Ring, Acc) ->
    sets:to_list(Acc);
merge_frags([F|T], Ring, Acc) ->
    Contents = riak_object:get_values(F),
    FSet = 
        case length(Contents) of
            1 ->
                merge_keysobj(F, []);
            _ ->
                NewSet = merge_keysobj(F, []),
                NewV = {NewSet, []},
                NewObj = riak_object:update_value(F, NewV),
                spawn(fun() -> put_keysobj(NewObj, Ring) end),
                NewSet
        end,
    merge_frags(T, Ring, sets:union([FSet, Acc])).

%% @private                    
fix_bucket(Ring) ->
    Bucket = riak_bucket:get_bucket(?BUCK, Ring),
    Change = case proplists:get_value(n_val,Bucket) of
        5 -> case proplists:get_value(allow_mult,Bucket) of
                 true -> false;
                 _ -> true
             end;
        _ -> true
    end,
    case Change of
        false -> nop;
        true ->
            riak_bucket:set_bucket(?BUCK,
                            [{n_val,5},{allow_mult,true},{has_links,false}])
    end.

%% @private 
all_frags(Bucket) when is_atom(Bucket) ->
    [list_to_binary(F) || F <- [atom_to_list(Bucket) ++ "-" ++ X || X <- [integer_to_list(I) || I <- lists:seq(0, ?NFRAGS-1)]]].
%% @private
ensure_ring(State=#state{ring=undefined}) ->
    riak_ring_manager:subscribe(self()),
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    State#state{ring=Ring};
ensure_ring(State) -> State.

%% @private
terminate(_,#state{tref=TRef}) -> timer:cancel(TRef), ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
handle_info({set_ring, Ring}, State) -> {noreply, State#state{ring=Ring}};
handle_info(timeout, State=#state{ops=Ops}) ->
    NewState = ensure_ring(State),
    do_write_all(Ops, NewState#state.ring),
    {noreply, NewState#state{ops=dict:new()}};
handle_info(_,State) -> {stop,badmsg,State}.
