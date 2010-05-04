-module(backend_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([initial_state/0, 
         initial_state_data/0,
         next_state_data/5,
         precondition/4, 
         postcondition/5]).

-export([stopped/3,
         running/3,
         init_backend/2,
         fold/2]).
-export([test/1, test/2, test/3, test/4, test/5]).
-export([prop_backend/4]).

-record(qcst, {c,  % Backend config
               s,  % Module state returned by Backend:start
               olds=sets:new(), % Old states after a stop
               d}).% Orddict of values stored

test(Backend) ->
    test(Backend, false).

test(Backend, Volatile) ->
    test(Backend, Volatile, []).

test(Backend, Volatile, Config) ->
    test(Backend, Volatile, Config, fun(_BeState,_Olds) -> ok end).

test(Backend, Volatile, Config, Cleanup) ->
    test(Backend, Volatile, Config, Cleanup, 30).

test(Backend, Volatile, Config, Cleanup, NumTests) ->
    eqc:quickcheck(eqc:numtests(NumTests, 
                                prop_backend(Backend, Volatile, Config, Cleanup))).

prop_backend(Backend, Volatile, Config, Cleanup) ->
    ?FORALL(Cmds, commands(?MODULE, {{stopped, Backend, Volatile}, initial_state_data(Config)}),
            aggregate(command_names(Cmds),
                      begin
                          {H,{_F,S},Res} = run_commands(?MODULE, Cmds),
                          Cleanup(S#qcst.s, sets:to_list(S#qcst.olds)),
                          ?WHENFAIL(
                             begin
                                 io:format("History: ~p\n", [H]),
                                 io:format("BE Config: ~p\nBE State: ~p\nD: ~p\n",
                                           [S#qcst.c, S#qcst.s, orddict:to_list(S#qcst.d)]),
                                 io:format("Result: ~p\n", [Res])
                             end,
                             Res == ok)
                      end)).


bucket() ->
    elements([<<"b1">>,<<"b2">>,<<"b3">>,<<"b4">>]).
   
key() ->
    elements([<<"k1">>,<<"k2">>,<<"k3">>,<<"k4">>]).

bkey() ->
    {bucket(),key()}.

val() ->
    binary().

initial_state() ->
    {stopped, riak_kv_ets_backend, true}.

initial_state_data() ->
    #qcst{d = orddict:new()}.

initial_state_data(Config) ->
    #qcst{c = Config, d = orddict:new()}.

next_state_data({running,Backend,Volatile},{stopped,Backend,Volatile},S,_R,
                {call,_M,stop,_}) ->
    S1 = S#qcst{s = undefined, olds = sets:add_element(S#qcst.s, S#qcst.olds)},
    case Volatile of
        true ->
            S1#qcst{d = orddict:new()};
        false ->
            S1
    end;
next_state_data({running,Backend,Volatile},{stopped,Backend,Volatile},S,_R,
                {call,_M,drop,_}) ->
    S#qcst{s=undefined, olds=sets:add_element(S#qcst.s, S#qcst.olds),
           d=orddict:new()};
next_state_data(_From,_To,S,BeState,{call,_M,init_backend,_}) ->
    S#qcst{s = BeState};
next_state_data(_From,_To,S,_R,{call,_M,put,[_S, Key, Val]}) ->
    S#qcst{d = orddict:store(Key, Val, S#qcst.d)};
next_state_data(_From,_To,S,_R,{call,_M,delete,[_S, Key]}) ->
    S#qcst{d = orddict:erase(Key, S#qcst.d)};
next_state_data(_From,_To,S,_R,_C) ->
    S.

stopped(Backend, Volatile, S) ->
    [{{running, Backend, Volatile}, {call,?MODULE,init_backend,[Backend,S#qcst.c]}}].

running(Backend, Volatile, S) ->
    [{history, {call,Backend,put,[S#qcst.s,bkey(),val()]}},
     {history, {call,Backend,get,[S#qcst.s,bkey()]}},
     {history, {call,Backend,delete,[S#qcst.s,bkey()]}},
     {history, {call,Backend,list,[S#qcst.s]}},
     {history, {call,?MODULE,fold,[Backend,S#qcst.s]}},
     {history, {call,Backend,is_empty,[S#qcst.s]}},
     {history, {call,Backend,list_bucket,[S#qcst.s,bucket()]}},
     {{stopped, Backend, Volatile}, {call,Backend,drop,[S#qcst.s]}},
     {{stopped, Backend, Volatile}, {call,Backend,stop,[S#qcst.s]}}
    ].


precondition(_From,_To,_S,_C) ->
    true.

postcondition(_From,_To,S,_C={call,_M,get,[_BeState, Key]},R) ->
    case R of
        {error, notfound} ->
            not orddict:is_key(Key, S#qcst.d);
        {ok, Val} ->
            {ok, Val} =:= orddict:find(Key, S#qcst.d)
    end;
postcondition(_From,_To,_S,_C={call,_M,put,[_BeState, _Key, _Val]},R) ->
    R =:= ok;
postcondition(_From,_To,_S,_C={call,_M,delete,[_BeState, _Key]},R) ->
    R =:= ok;
postcondition(_From,_To,S,_C={call,_M,list,[_BeState]},R) ->
    lists:sort(orddict:fetch_keys(S#qcst.d)) =:= lists:sort(R);
postcondition(_From,_To,S,_C={call,_M,fold,[_Backend,_BeState]},R) ->
    lists:sort(orddict:to_list(S#qcst.d)) =:= lists:sort(R);
postcondition(_From,_To,S,_C={call,_M,is_empty,[_BeState]},R) ->
    R =:= (orddict:size(S#qcst.d) =:= 0);
postcondition(_From,_To,S,_C={call,_M,list_bucket,[_BeState,Bucket]},R) ->
    AllKeys = orddict:fetch_keys(S#qcst.d),
    lists:sort(R) =:= lists:sort([K || {B,K} <- AllKeys, B =:= Bucket]);
postcondition(_From,_To,_S,_C,_R) ->
    true.


init_backend(Backend, Config) ->
    {ok, S} = Backend:start(42, Config),
    S.

fold_fun(K,V,Acc) ->
    [{K,V}|Acc].

fold(Backend, BeState) ->
    Backend:fold(BeState, fun fold_fun/3, []).

-endif.
    
