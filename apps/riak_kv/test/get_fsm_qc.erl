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

num_partitions() ->
    %% TODO: use some unfortunate parition counts (1, 50, etc.)
    elements([4, 16, 64]).

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

start_mock_servers() ->
    get_fsm_qc_vnode_master:start_link(),
    application:load(riak_core),
    application:start(crypto).

prop_basic_get() ->
    ?FORALL({Partitions, Object}, {num_partitions(), riak_object()},
    ?FORALL({N, Ring}, {n(Partitions), ring(Partitions)},
    ?FORALL(R, n(N),
    begin
        ok = gen_server:call(riak_kv_vnode_master,
                             {set_data, Object}),

        mochiglobal:put(?RING_KEY, Ring),

        application:set_env(riak_core,
                            default_bucket_props,
                            [{n_val, N}
                             |?DEFAULT_BUCKET_PROPS]),
        
        ReqId = make_ref(),
        riak_kv_get_fsm:start(ReqId,
                              riak_object:bucket(Object),
                              riak_object:key(Object),
                              R,
                              60000,
                              self()),

        receive
            {ReqId, {ok, Reply}} ->
                equals(Reply, Object);
            {ReqId, Error} ->
                ?WHENFAIL(io:format("Error: ~p~n", [Error]),
                          false);
            Anything ->
                ?WHENFAIL(io:format("Anything: ~p~n", [Anything]),
                          false)
        after 1000 ->
                ?WHENFAIL(io:format("Timeout!~n", []),
                          false)
        end
    end))).
