-module(riak_kv_get_fsm_intercepts).
-compile(export_all).
-include("intercept.hrl").

-define(M, riak_kv_get_fsm_orig).


%% @doc simulate slow puts by adding delay to the prepare state.
slow_prepare(Atom, State) ->  
    timer:sleep(1000),
    ?M:prepare_orig(Atom, State).
