-module(riak_mapper).

-record(jsenv, {ctx, csums=dict:new()}).

-export([init_state/0, init_js/1, do_map/8]).

init_state() ->
    {ok, JsCtx} = js_driver:new({?MODULE, init_js}),
    #jsenv{ctx=JsCtx}.

do_map({map,FunTerm,Arg,_Acc}, BKey, Mod, ModState, KeyData, MapState, Cache, VNode) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(FunTerm, BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            uncached_map(BKey, Mod, ModState, MapState, FunTerm, Arg, KeyData, VNode);
        CV ->
            {ok, CV, MapState}
    end.

init_js(Ctx) ->
    EmitFunction = <<"var __map_results__ = []; function emit(data) { __map_results__[__map_results__.length] = data; };">>,
    GetResultsFunction = <<"function __get_map_results__() { var retval = __map_results__; __map_results__ = []; return retval; };">>,
    ok = js:define(Ctx, EmitFunction),
    ok = js:define(Ctx, GetResultsFunction),
    ok.

%% Internal functions
build_key({modfun, CMod, CFun}, Arg, KeyData) ->
    {CMod, CFun, Arg, KeyData};
build_key(_, _, _) ->
    no_key.

cache_fetch({qfun, _}, _BKey, _CacheKey, _MapState) ->
    not_cached;
cache_fetch({jsfun, _}, _BKey, _CacheKey, _MapState) ->
    not_cached;
cache_fetch({modfun, _CMod, _CFun}, BKey, CacheKey, Cache) ->
    case orddict:find(BKey, Cache) of
        error -> not_cached;
        {ok,CDict} ->
            case orddict:find(CacheKey,CDict) of
                error -> not_cached;
                {ok,CVal} -> CVal
            end
    end.

uncached_map(BKey, Mod, ModState, MapState, FunTerm, Arg, KeyData, VNode) ->
    riak_eventer:notify(riak_vnode, uncached_map, {FunTerm, Arg, BKey}),
    case Mod:get(ModState, BKey) of
        {ok, Binary} ->
            V = binary_to_term(Binary),
            exec_map(V, MapState, FunTerm, Arg, BKey, KeyData, VNode);
        {error, notfound} ->
            exec_map({error, notfound}, MapState, FunTerm, Arg, BKey, KeyData, VNode);
        X ->
            {error, X, MapState}
    end.

exec_map(V, MapState, FunTerm, Arg, BKey, KeyData, VNode) ->
    try
        {MapVal, NewMapState} = case FunTerm of
            {qfun,F} -> {(F)(V,KeyData,Arg), MapState};
            {jsfun,F} -> invoke_js(MapState, [extract_values(V),KeyData,Arg], <<"riak_mapper">>, F);
            {modfun,M,F} ->
                MF_Res = M:F(V,KeyData,Arg),
                gen_fsm:send_event(VNode,
                                   {mapcache, BKey,{M,F,Arg,KeyData},MF_Res}),
                {MF_Res, MapState}
        end,
        {ok, MapVal, NewMapState}
    catch C:R ->
            Reason = {C, R, erlang:get_stacktrace()},
            {error, Reason, MapState}
    end.

extract_values({error, not_found}) ->
    [{<<"error">>, <<"not_found">>}];
extract_values(V) ->
    case riak_object:value_count(V) of
        0 ->
            [];
        1 ->
            riak_object:get_value(V);
        _ ->
            riak_object:get_values(V)
    end.

invoke_js(#jsenv{ctx=JsCtx, csums=CSums}=MapState, V, FunName, F) ->
    MD5 = erlang:md5(F),
    NewState = case needs_defining(CSums, FunName, MD5) of
                   true ->
                       io:format("Compiling map fun~n"),
                       F1 = list_to_binary(["var ", FunName, "=", F]),
                       js:define(JsCtx, F1),
                       MapState#jsenv{csums=dict:store(FunName, MD5, CSums)};
                   false ->
                       io:format("Not compiling map fun~n"),
                       MapState
               end,
    R = js:call(JsCtx, FunName, [V]),
    io:format("R: ~p~n", [R]),
    case R of
        {ok, _} ->
            case js:call(JsCtx, <<"__get_map_results__">>, []) of
                {ok, Results} ->
                    {Results, NewState};
                Error ->
                    {Error, NewState}
            end
    end.

needs_defining(CSums, FunName, CSum) ->
    case dict:find(FunName, CSums) of
        error ->
            true;
        {ok, OldCSum} ->
            not(OldCSum =:= CSum)
    end.
