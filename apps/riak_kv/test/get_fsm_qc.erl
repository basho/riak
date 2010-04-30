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
                          [binary(4), nat(), Clock]})
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

partval() ->
    frequency([{2,ok},
               {1,?SHRINK(notfound, [ok])},
               {1,?SHRINK(timeout, [ok])},
               {1,?SHRINK(error, [ok])}]).

partvals(Partitions) ->
    vector(Partitions, partval()).

partvals() ->
    non_empty(longer_list(2, partval())).

start_mock_servers() ->
    get_fsm_qc_vnode_master:start_link(),
    application:load(riak_core),
    application:start(crypto).

prop_len() ->
    ?FORALL({R, Ps}, {choose(1, 10), partvals()},
        collect({R, length(Ps)}, true)
    ).

prop_basic_get() ->
    ?FORALL({RSeed,NQdiff,Object,ReqId,PartVals},
            {largenat(),choose(0,4096),
             noshrink(riak_object()), noshrink(largeint()),
             partvals()},
    begin
        N = length(PartVals),
        R = (RSeed rem N) + 1,
        Q = make_power_of_two(N + NQdiff),
        Ring = riak_core_ring:fresh(Q, node()),

        ok = gen_server:call(riak_kv_vnode_master,
                         {set_data, Object, PartVals}),

        mochiglobal:put(?RING_KEY, Ring),

        application:set_env(riak_core,
                            default_bucket_props,
                            [{n_val, N}
                             |?DEFAULT_BUCKET_PROPS]),
    
        {ok, GetPid} = riak_kv_get_fsm:start(ReqId,
                            riak_object:bucket(Object),
                            riak_object:key(Object),
                            R,
                            200,
                            self()),

        ok = wait_for_pid(GetPid),
        Res = wait_for_req_id(ReqId),
        History = get_fsm_qc_vnode_master:get_history(),
        RepairHistory = get_fsm_qc_vnode_master:get_repair_history(),
        Ok       = length([ ok || {_, {ok, _}} <- History ]),
        NotFound = length([ ok || {_, {error, notfound}} <- History ]),
        NoReply  = length([ ok || {_, {error, timeout}}  <- History ]),
        H        = lists:map(fun({_, {ok, _}})      -> ok;
                            ({_, {error, Err}}) -> Err end, History),
        Expected = expect(Object, H, N, R),
        ?WHENFAIL(
            begin
                io:format("History: ~p~nRepair: ~p~n",
                          [History, RepairHistory]),
                io:format("N: ~p~nR: ~p~nQ: ~p~nResult: ~p~nExpected: ~p~n",
                          [N, R, Q, Res, Expected]),
                io:format("H: ~p~nOk: ~p~nNotFound: ~p~nNoReply: ~p~n",
                          [H, Ok, NotFound, NoReply])
            end,
            conjunction(
                [{result, Res =:= Expected},
                 {n_value, equals(length(History), N)},
                 {repair, check_repair(RepairHistory, History)}
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

expected_repairs(H) ->
    case [ ok || {_, {ok, _}} <- H ] of
        [] -> [];
        _  -> [ Part || {Part, {error, notfound}} <- H ]
    end.

check_repair(RepairH, H) ->
    Expected = expected_repairs(H),
    Actual   = [ Part || {vnode_put, {Part, _}, _} <- RepairH ],
    Deletes  = lists:filter(fun({vnode_put, _, _}) -> false;
                               (_) -> true end, RepairH),
    conjunction(
        [{puts, equals(lists:sort(Expected), lists:sort(Actual))},
         {junk, equals(Deletes, [])}
        ]).

expect(Object,History,N,R) ->
    case expect(History,N,R,0,0,0) of
        ok  -> {ok, Object};
        Err -> {error, Err}
    end.

notfound_or_error(0, Err) ->
    lists:duplicate(Err, error);
notfound_or_error(_NotFound, _Err) ->
    notfound.

expect(H, N, R, NotFounds, Oks, Errs) ->
    Pending = N - (NotFounds + Oks + Errs),
    if  Oks >= R ->                     % we made quorum
            ok;
        (NotFounds + Errs)*2 > N orelse % basic quorum
        Pending + Oks < R ->            % no way we'll make quorum
            notfound_or_error(NotFounds, Errs);
        true ->
            case H of
                [] ->
                    timeout;
                [Res|Rest] ->
                    handle_result(Res, Rest, N, R, NotFounds, Oks, Errs)
            end
    end.

handle_result(timeout,Rest,N,R,NotFounds,Oks,Errs) ->
    expect(Rest,N,R,NotFounds,Oks,Errs);
handle_result(notfound,Rest,N,R,NotFounds,Oks,Errs) ->
    expect(Rest,N,R,NotFounds+1,Oks,Errs);
handle_result(error,Rest,N,R,NotFounds,Oks,Errs) ->
    expect(Rest,N,R,NotFounds,Oks,Errs+1);
handle_result(ok,Rest,N,R,NotFounds,Oks,Errs) ->
    expect(Rest,N,R,NotFounds,Oks+1,Errs).
    
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
