-module(get_fsm_qc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).
-define(RING_KEY, riak_ring).
-define(DEFAULT_BUCKET_PROPS,
        [{allow_mult, false},
         {chash_keyfun, {riak_core_util, chash_std_keyfun}}]).

%% Generators

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
    frequency([{1,ok},
               {1,?SHRINK(notfound, [ok])},
               {1,?SHRINK(timeout, [ok])}]).

partvals(Partitions) ->
    vector(Partitions, partval()).

start_mock_servers() ->
    get_fsm_qc_vnode_master:start(),
    application:load(riak_core),
    application:start(crypto).



%    ?FORALL({Partitions, Object, ReqId}, {num_partitions(), noshrink(riak_object()), noshrink(largeint())},
%    ?FORALL({N, Ring, PartVals}, {n(Partitions), ring(Partitions), partvals(Partitions)},
%    ?FORALL(R, n(N),

prop_basic_get() ->
    ?FORALL({R,RNdiff,NQdiff,Object,ReqId,PartVals},
            {choose(1,10),choose(0,10),choose(0,4096),
             noshrink(riak_object()), noshrink(largeint()),
             ?LET(M, choose(0,20), partvals(M))},
    begin
        N = R + RNdiff,
        Q = make_power_of_two(N + NQdiff),
        Ring = riak_core_ring:fresh(Q, node()),

        ok = gen_server:call(riak_kv_vnode_master,
                         {set_data, Object, PartVals}),

        mochiglobal:put(?RING_KEY, Ring),

        application:set_env(riak_core,
                            default_bucket_props,
                            [{n_val, N}
                             |?DEFAULT_BUCKET_PROPS]),
    
        riak_kv_get_fsm:start(ReqId,
                              riak_object:bucket(Object),
                              riak_object:key(Object),
                              R,
                              200,
                              self()),

        Res = wait_for_req_id(ReqId),
        History = get_fsm_qc_vnode_master:get_history(),
        Ok       = length([ ok || {_, {ok, _}} <- History ]),
        NotFound = length([ ok || {_, {error, notfound}} <- History ]),
        NoReply  = length([ ok || {_, {error, timeout}}  <- History ]),
        H        = lists:map(fun({_, {ok, _}})      -> ok;
                            ({_, {error, Err}}) -> Err end, History),
        Expected = expect(H, N, R),
        ?WHENFAIL(
            io:format("N: ~p~nR: ~p~nQ: ~p~nResult: ~p~nHistory: ~p~nOk: ~p~nNotFound: ~p~nNoReply: ~p~nExpected: ~p~n",
                      [N, R, Q, Res, H, Ok, NotFound, NoReply, Expected]),
            conjunction(
                [{result,
                    case Res of
                        {ok, Reply} ->
                            conjunction(
                                [{object, equals(Reply, Object)},
                                 {ok, Expected =:= ok}]);
                        {error, timeout} ->
                            Expected =:= timeout;
                        {error, notfound} ->
                            Expected =:= notfound;
                        {anything, _Anything} ->
                            false;
                        timeout ->
                            false
                    end},
                {n_value, equals(length(History), N)}
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

expect(History,N,R) ->
    expect(History,N,R,0,0).
    
expect([],N,R,NotFounds,_Oks) ->
    case NotFounds >= N-R+1 of
        true -> notfound;        
        false -> timeout
    end;
expect([timeout|Rest],N,R,NotFounds,Oks) ->
    expect(Rest,N,R,NotFounds,Oks);
expect([notfound|Rest],N,R,NotFounds,Oks) ->
    case (NotFounds + 1)*2 > N of
        true ->
            notfound;
        false ->
            expect(Rest,N,R,NotFounds+1,Oks)
    end;
expect([ok|Rest],N,R,NotFounds,Oks) ->
    case Oks+1 >= R of
        true ->
            ok;
        false ->
            expect(Rest,N,R,NotFounds,Oks+1)
    end.
