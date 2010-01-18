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
build_key({jsfun, FunName}, Arg, KeyData) ->
    {FunName, Arg, KeyData};
build_key(_, _, _) ->
    no_key.

cache_fetch({qfun, _}, _BKey, _CacheKey, _MapState) ->
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
    end;
%% TODO: Cache jsfun results, too
cache_fetch({jsfun, _FunName}, _BKey, _CacheKey, _Cache) ->
    not_cached.

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
                                        {Retval, _} = riak_js:invoke_map(JsCtx, CSums, [jsonify_object(V), KeyData, jsonify_arg(Arg)],
                                                                         <<"Riak">>, F, undefined),
                                        {Retval, MapState};
                                    {jsanon, {Bucket, Key}} ->
                                        exec_map(V, MapState, {jsanon, riak_js:fetch_fun(Bucket, Key)}, Arg, BKey, KeyData, VNode);
                                    {jsanon, F} ->
                                        {Retval, NewCSums} = riak_js:invoke_map(JsCtx, CSums, [jsonify_object(V), KeyData, jsonify_arg(Arg)],
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

jsonify_object({error, notfound}) ->
    {struct, [{<<"error">>, <<"notfound">>}]};
jsonify_object(Obj) ->
    {_,Vclock} = raw_http_resource:vclock_header(Obj),
    {struct, [{<<"bucket">>, riak_object:bucket(Obj)},
              {<<"key">>, riak_object:key(Obj)},
              {<<"vclock">>, list_to_binary(Vclock)},
              {<<"values">>,
               [{struct,
                 [{<<"metadata">>, jsonify_metadata(MD)},
                  {<<"data">>, V}]}
                || {MD, V} <- riak_object:get_contents(Obj)
                      ]}]}.

jsonify_metadata(MD) ->
    MDJS = fun({LastMod, Now={_,_,_}}) ->
                   % convert Now to JS-readable time string
                   {LastMod, list_to_binary(
                               httpd_util:rfc1123_date(
                                 calendar:now_to_local_time(Now)))};
              ({<<"Links">>, Links}) ->
                   {<<"Links">>, [ [B, K, T] || {{B, K}, T} <- Links ]};
              ({Name, List=[_|_]}) ->
                   {Name, jsonify_metadata_list(List)};
              ({Name, Value}) ->
                   {Name, Value}
           end,
    {struct, lists:map(MDJS, dict:to_list(MD))}.

%% @doc convert strings to binaries, and proplists to JSON objects
jsonify_metadata_list([]) -> [];
jsonify_metadata_list(List) ->
    Classifier = fun({Key,_}, Type) when (is_binary(Key) orelse is_list(Key)),
                                         Type /= array, Type /= string ->
                         struct;
                    (C, Type) when is_integer(C), C >= 0, C =< 256,
                                   Type /= array, Type /= struct ->
                         string;
                    (_, _) ->
                         array
                 end,
    case lists:foldl(Classifier, undefined, List) of
        struct -> {struct, [ {if is_list(Key) -> list_to_binary(Key);
                                 true         -> Key
                              end,
                              if is_list(Value) -> jsonify_metadata_list(Value);
                                 true           -> Value
                              end}
                             || {Key, Value} <- List]};
        string -> list_to_binary(List);
        array -> List
    end.

jsonify_arg({Bucket,Tag}) when (Bucket == '_' orelse is_binary(Bucket)),
                               (Tag == '_' orelse is_binary(Tag)) ->
    %% convert link match syntax
    {struct, [{<<"bucket">>,Bucket},
              {<<"tag">>,Tag}]};
jsonify_arg(Other) ->
    Other.
