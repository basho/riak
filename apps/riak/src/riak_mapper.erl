-module(riak_mapper).

-record(jsenv, {ctx, csums=dict:new()}).

-export([init_state/0, terminate/1, do_map/8]).

init_state() ->
    {ok, JsCtx} = riak_js:new_context(),
    #jsenv{ctx=JsCtx}.

terminate(#jsenv{ctx=Ctx}) ->
    js_driver:destroy(Ctx).

do_map({map,FunTerm,Arg,_Acc}, BKey, Mod, ModState, KeyData, MapState, Cache, VNode) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(FunTerm, BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            uncached_map(BKey, Mod, ModState, MapState, FunTerm, Arg, KeyData, VNode);
        CV ->
            {ok, CV, MapState}
    end.

%% Internal functions

build_key({modfun, CMod, CFun}, Arg, KeyData) ->
    {CMod, CFun, Arg, KeyData};
build_key(_, _, _) ->
    no_key.

cache_fetch({qfun, _}, _BKey, _CacheKey, _MapState) ->
    not_cached;
cache_fetch({jsfun, _}, _BKey, _CacheKey, _MapState) ->
    not_cached;
cache_fetch({jsanon, _}, _BKey, _CacheKey, _MapState) ->
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

exec_map(V, #jsenv{ctx=JsCtx, csums=CSums}=MapState, FunTerm, Arg, BKey, KeyData, VNode) ->
    try
        {MapVal, NewMapState} = case FunTerm of
                                    {qfun, F} -> {(F)(V,KeyData,Arg), MapState};
                                    {jsfun, F} ->
                                        {Retval, _} = riak_js:invoke_map(JsCtx, CSums, [extract_values(V), KeyData, Arg],
                                                                         <<"Riak">>, F, undefined),
                                        {Retval, MapState};
                                    {jsanon, F} ->
                                        {Retval, NewCSums} = riak_js:invoke_map(JsCtx, CSums, [extract_values(V), KeyData, Arg],
                                                                                undefined, <<"riakMapper">>, F),
                                        {Retval, MapState#jsenv{csums=NewCSums}};
                                    {modfun, M, F} ->
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
