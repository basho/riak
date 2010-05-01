-module(get_fsm_qc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-define(RING_KEY, riak_ring).
-define(DEFAULT_BUCKET_PROPS,
        [{allow_mult, false},
         {chash_keyfun, {riak_core_util, chash_std_keyfun}}]).

%% Generators

longer_list(K, G) ->
    ?SIZED(Size, resize(trunc(K*Size), list(resize(Size, G)))).

non_empty(G) ->
    ?SUCHTHAT(X, G, X /= [] andalso X /= <<>>).

largenat() ->
    ?LET(X, largeint(), abs(X)).

n(Max) ->
    choose(1, Max).

bkey() ->
    %%TODO: "make this nastier"
    {binary(6),  %% bucket
     binary(6)}. %% key

pow(_, 0) -> 1;
pow(A, N) -> A * pow(A, N - 1).

make_power_of_two(Q) -> make_power_of_two(Q, 1).

make_power_of_two(Q, P) when P >= Q -> P;
make_power_of_two(Q, P) -> make_power_of_two(Q, P*2).

all_distinct(Xs) ->
    lists:sort(Xs) =:= lists:usort(Xs).

num_partitions() ->
    %% TODO: use some unfortunate parition counts (1, 50, etc.)
    % elements([4, 16, 64]).
    ?LET(N, choose(0, 6), pow(2, N)).

ring(Partitions) ->
    riak_core_ring:fresh(Partitions, node()).

vclock() ->
    ?LET(VclockSym, vclock_sym(), eval(VclockSym)).

vclock_sym() ->
    ?LAZY(
       oneof([
              {call, vclock, fresh, []},
              ?LETSHRINK([Clock], [vclock_sym()],
                         {call, ?MODULE, increment,
                          [noshrink(binary(4)), nat(), Clock]})
              ])).

increment(Actor, Count, Vclock) ->
    lists:foldl(
      fun vclock:increment/2,
      Vclock,
      lists:duplicate(Count, Actor)).

riak_object() ->
    ?LET({{Bucket, Key}, Vclock, Value},
         {bkey(), vclock(), binary()},
         riak_object:set_vclock(
           riak_object:new(Bucket, Key, Value),
           Vclock)).

build_riak_obj(B,K,Vc,Val,notombstone) ->
    riak_object:set_contents(
        riak_object:set_vclock(
            riak_object:new(B,K,Val),
                Vc),
        [{dict:from_list([{<<"X-Riak-Last-Modified">>,now()}]), Val}]);
build_riak_obj(B,K,Vc,Val,tombstone) ->
    Obj = build_riak_obj(B,K,Vc,Val,notombstone),
    add_tombstone(Obj).
    
add_tombstone(Obj) ->
    [{M,V}] = riak_object:get_contents(Obj),
    NewM = dict:store(<<"X-Riak-Deleted">>, true, M),
    riak_object:set_contents(Obj, [{NewM, V}]).
                
maybe_tombstone() ->
    weighted_default({2, notombstone}, {1, tombstone}).

%% Generate 5 riak objects with the same bkey
%% 
riak_objects() ->
    ?LET({{Bucket,Key},AncestorVclock,Tombstones}, 
         {noshrink(bkey()),vclock(),vector(5, maybe_tombstone())},
    begin
        BrotherVclock  = vclock:increment(<<"bro!">>, AncestorVclock),
        OtherBroVclock = vclock:increment(<<"bro2">>, AncestorVclock),
        SisterVclock   = vclock:increment(<<"sis!">>, AncestorVclock),
        CurrentVclock  = vclock:merge([BrotherVclock,SisterVclock,OtherBroVclock]),
        Clocks = [{ancestor, AncestorVclock, <<"ancestor">>},
                  {brother,  BrotherVclock, <<"brother">>},
                  {sister,   SisterVclock, <<"sister">>},
                  {otherbrother, OtherBroVclock, <<"otherbrother">>},
                  {current,  CurrentVclock, <<"current">>}],
        [ {Lineage, build_riak_obj(Bucket, Key, Vclock, Value, Tombstone)}
            || {{Lineage, Vclock, Value}, Tombstone} <- lists:zip(Clocks, Tombstones) ]
    end).

%%
%%         ancestor
%%       /     |    \
%%  brother   sister otherbrother
%%       \     |    /
%%         current
%%    
lineage() ->
    elements([current, ancestor, brother, sister, otherbrother]).

merge(ancestor, Lineage) -> Lineage;
merge(Lineage, ancestor) -> Lineage;
merge(_, current)        -> current;
merge(current, _)        -> current;
merge(otherbrother, _)   -> otherbrother;
merge(_, otherbrother)   -> otherbrother;
merge(sister, _)         -> sister;
merge(_, sister)         -> sister;
merge(brother, _)        -> brother;
merge(_, brother)        -> brother.

merge([Lin]) ->
    Lin;
merge([Lin|Lins]) ->
    merge(Lin, merge(Lins)).

merge_heads([]) ->
    [];
merge_heads([Lin|Lins]) ->
    merge_heads(Lin, merge_heads(Lins)).

merge_heads(Lin, Lins) ->
    Subsumes = fun(Lin0, Lin1) ->
            is_descendant(Lin0, Lin1) orelse
            Lin0 == Lin1
        end,
    case lists:any(fun(Lin1) -> Subsumes(Lin1, Lin) end, Lins) of
        true  -> Lins;
        false ->
            [Lin|lists:filter(fun(Lin1) -> not Subsumes(Lin, Lin1) end, Lins)]
    end.

is_descendant(Lin, Lin) ->
    false;
is_descendant(current, _) ->
    true;
is_descendant(_, ancestor) ->
    true;
is_descendant(_, _) ->
    false.

is_sibling(Lin, Lin) ->
    false;
is_sibling(Lin1, Lin2) ->
    not is_descendant(Lin1, Lin2) andalso
    not is_descendant(Lin2, Lin1).

partval() ->
    Shrink = fun(G) -> ?SHRINK(G, [{ok, current}]) end,
    frequency([{2,{ok, lineage()}},
               {1,Shrink(notfound)},
               {1,Shrink(timeout)},
               {1,Shrink(error)}]).

partvals() ->
    non_empty(longer_list(2, partval())).

start_mock_servers() ->
    case whereis(riak_kv_vnode_master) of
        undefined -> ok;
        Pid       ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_pid(Pid)
    end,
    get_fsm_qc_vnode_master:start_link(),
    application:load(riak_core),
    application:start(crypto),
    ok.

node_status() ->
    frequency([{1, ?SHRINK(down, [up])},
               {9, up}]).

cycle(N, Xs=[_|_]) when N >= 0 ->
    cycle(Xs, N, Xs).

cycle(_Zs, 0, _Xs) ->
    [];
cycle(Zs, N, [X|Xs]) ->
    [X|cycle(Zs, N - 1, Xs)];
cycle(Zs, N, []) ->
    cycle(Zs, N, Zs).

reassign_nodes(Status, Ring) ->
    Ids = [ I || {I, _} <- riak_core_ring:all_owners(Ring) ],
    lists:foldl(
        fun({down, Id}, R) ->
                riak_core_ring:transfer_node(Id, 'dummy@nohost', R);
           (_, R) -> R
        end, Ring, lists:zip(Status, Ids)).

prop_len() ->
    ?FORALL({R, Ps}, {choose(1, 10), partvals()},
        collect({R, length(Ps)}, true)
    ).

prop_basic_get() ->
    ?FORALL({RSeed,NQdiff,Objects,ReqId,PartVals,NodeStatus0},
            {largenat(),choose(0,4096),
             riak_objects(), noshrink(largeint()),
             partvals(),non_empty(longer_list(10, node_status()))},
    begin
        N = length(PartVals),
        R = (RSeed rem N) + 1,
        Q = make_power_of_two(N + NQdiff),
        NodeStatus = cycle(Q, NodeStatus0),
        Ring = reassign_nodes(NodeStatus,
                              riak_core_ring:fresh(Q, node())),

        ok = gen_server:call(riak_kv_vnode_master,
                         {set_data, Objects, PartVals}),

        mochiglobal:put(?RING_KEY, Ring),

        application:set_env(riak_core,
                            default_bucket_props,
                            [{n_val, N}
                             |?DEFAULT_BUCKET_PROPS]),
    
        [{_,Object}|_] = Objects,
    
        {ok, GetPid} = riak_kv_get_fsm:start(ReqId,
                            riak_object:bucket(Object),
                            riak_object:key(Object),
                            R,
                            200,
                            self()),

        ok = wait_for_pid(GetPid),
        % Give read repairs and deletes a chance to go through
        timer:sleep(5),
        Res = wait_for_req_id(ReqId),
        History = get_fsm_qc_vnode_master:get_history(),
        RepairHistory = get_fsm_qc_vnode_master:get_repair_history(),
        Ok         = length([ ok || {_, {ok, _}} <- History ]),
        NotFound   = length([ ok || {_, notfound} <- History ]),
        NoReply    = length([ ok || {_, timeout}  <- History ]),
        Partitions = [ P || {{P,_}, _} <- History ],
        Deleted    = [ {Lin, case riak_kv_util:obj_not_deleted(Obj) == undefined of
                                 true  -> deleted;
                                 false -> present
                             end
                       }
                        || {Lin, Obj} <- Objects ],
        H          = [ V || {_, V} <- History ],
        ExpectedN  = lists:min([N, length([xx || up <- NodeStatus])]),
        Expected   = expect(Objects, H, N, R),
        ?WHENFAIL(
            begin
                io:format("Ring: ~p~nRepair: ~p~nHistory: ~p~n",
                          [Ring, RepairHistory, History]),
                io:format("Result: ~p~nExpected: ~p~nNode status: ~p~nDeleted objects: ~p~n",
                          [Res, Expected, NodeStatus, Deleted]),
                io:format("N: ~p~nR: ~p~nQ: ~p~n",
                          [N, R, Q]),
                io:format("H: ~p~nOk: ~p~nNotFound: ~p~nNoReply: ~p~n",
                          [H, Ok, NotFound, NoReply])
            end,
            conjunction(
                [{result, Res =:= Expected},
                 {n_value, equals(length(History), ExpectedN)},
                 {repair, check_repair(Objects, RepairHistory, History)},
                 {delete, check_delete(Objects, RepairHistory, History)},
                 {distinct, all_distinct(Partitions)}
                ]))
    end).

wait_for_req_id(ReqId) ->
    receive
        {ReqId, {ok, Reply1}} ->
            {ok, Reply1};
        {ReqId, Error1} ->
            Error1;
        Anything1 ->
            {anything, Anything1}
    after 400 ->
            timeout
    end.


test() ->
    test(100).

test(N) ->
    quickcheck(numtests(N, prop_basic_get())).

do_repair(_Heads, notfound) ->
    true;
do_repair(Heads, {ok, Lineage}) ->
    lists:any(fun(Head) ->
                is_descendant(Head, Lineage) orelse
                is_sibling(Head, Lineage)
              end, Heads);
do_repair(_Heads, _V) ->
    false.

expected_repairs(H) ->
    case [ Lineage || {_, {ok, Lineage}} <- H ] of
        []   -> [];
        Lins ->
            Heads = merge_heads(Lins),
            [ Part || {{Part, _Node}, V} <- H,
                      do_repair(Heads, V) ]
    end.

check_repair(Objects, RepairH, H) ->
    Actual = [ Part || {vnode_put, {Part, _}, _} <- RepairH ],
    Heads  = merge_heads([ Lineage || {_, {ok, Lineage}} <- H ]),
    
    AllDeleted = lists:all(fun({_, {ok, Lineage}}) ->
                                Obj = proplists:get_value(Lineage, Objects),
                                riak_kv_util:is_x_deleted(Obj);
                              (_) -> true
                           end, H),
    Expected = case AllDeleted of
            false -> expected_repairs(H);
            true  -> []  %% we don't expect read repair if everyone has a tombstone
        end,

    RepairObject  = (catch build_merged_object(Heads, Objects)),
    RepairObjects = [ Obj || {vnode_put, _, {_, _, Obj, _, _}} <- RepairH ],

    conjunction(
        [{puts, equals(lists:sort(Expected), lists:sort(Actual))},
         {sanity, equals(length(RepairObjects), length(Actual))},
         {right_object,
            ?WHENFAIL(io:format("RepairObject: ~p~n", [RepairObject]),
                lists:all(fun(Obj) -> Obj =:= RepairObject end,
                          RepairObjects))}
        ]).

check_delete(Objects, RepairH, H) ->
    Deletes  = [ Part || {vnode_del, {Part,_Node}, _} <- RepairH ],

    AllDeleted = lists:all(fun({{_,Node}, {ok, Lineage}}) ->
                                Obj = proplists:get_value(Lineage, Objects),
                                riak_kv_util:is_x_deleted(Obj) andalso Node == node();
                              ({{_,Node}, notfound}) -> Node == node();
                              ({_, error})    -> false;
                              ({_, timeout})  -> false
                           end, H),

    HasOk = lists:any(fun({_, {ok, _}}) -> true;
                         (_) -> false
                      end, H),

    Expected = case AllDeleted andalso HasOk of
        true  -> [ P || {{P, _N}, _} <- H ];  %% send deletes to notfound nodes as well
        false -> []
    end,

    equals(lists:sort(Expected), lists:sort(Deletes)).


build_merged_object(Heads, Objects) ->
    Lineage = merge(Heads),
    Object  = proplists:get_value(Lineage, Objects),
    Vclock  = vclock:merge(
                [ riak_object:vclock(proplists:get_value(Head, Objects))
                    || Head <- Heads ]),
   riak_object:set_vclock(Object, Vclock).

expect(Objects,History,N,R) ->
    case expect(History,N,R,0,0,0,[]) of
        {ok, Heads} ->
            case riak_kv_util:obj_not_deleted(build_merged_object(Heads, Objects)) of
                undefined -> {error, notfound};
                Obj       -> {ok, Obj}
            end;
        Err ->
            {error, Err}
    end.

notfound_or_error(0, Err) ->
    lists:duplicate(Err, error);
notfound_or_error(_NotFound, _Err) ->
    notfound.

expect(H, N, R, NotFounds, Oks, Errs, Heads) ->
    Pending = N - (NotFounds + Oks + Errs),
    if  Oks >= R ->                     % we made quorum
            {ok, Heads};
        (NotFounds + Errs)*2 > N orelse % basic quorum
        Pending + Oks < R ->            % no way we'll make quorum
            notfound_or_error(NotFounds, Errs);
        true ->
            case H of
                [] ->
                    timeout;
                [timeout|Rest] ->
                    expect(Rest, N, R, NotFounds, Oks, Errs, Heads);
                [notfound|Rest] ->
                    expect(Rest, N, R, NotFounds + 1, Oks, Errs, Heads);
                [error|Rest] ->
                    expect(Rest, N, R, NotFounds, Oks, Errs + 1, Heads);
                [{ok,Lineage}|Rest] ->
                    expect(Rest, N, R, NotFounds, Oks + 1, Errs,
                           merge_heads(Lineage, Heads))
            end
    end.

wait_for_pid(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN',Mref,process,_,_} ->
            ok
    after
        1000 ->
            {error, didnotexit}
    end.
    
    
eqc_test_() ->
    {spawn,
    {timeout, 20, ?_test(
        begin
            start_mock_servers(),
            ?assert(test(30))
        end)
    }}.

%% Call unused callback functions to clear them in the coverage
%% checker so the real code stands out.
coverage_test() ->
    riak_kv_test_util:call_unused_fsm_funs(riak_kv_get_fsm).
    
-endif. % EQC
