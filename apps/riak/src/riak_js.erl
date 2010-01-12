-module(riak_js).

-export([new_context/0, init_context/1, invoke_map/6, invoke_reduce/6]).
-export([fetch_fun/2]).

new_context() ->
    js_driver:new({?MODULE, init_context}).

init_context(Ctx) ->
    case load_init_source() of
        {ok, Source} ->
            js:define(Ctx, Source);
        Error ->
            Error
    end.


invoke_map(JsCtx, CSums, Args, Class, FunName, undefined) when Class =:= <<"Riak">> ->
    RealFunName = list_to_binary([Class, <<".">>, FunName]),
    case js:call(JsCtx, RealFunName, Args) of
        {ok, _} ->
            case js:call(JsCtx, <<"Riak.getResults">>, []) of
                {ok, Results} ->
                    {Results, CSums};
                Error ->
                    {Error, CSums}
            end;
        Error ->
            {Error, CSums}
    end;

invoke_map(JsCtx, CSums, Args, undefined, FunName, F) ->
    MD5 = erlang:md5(F),
    {Continue, NewCSums} = case needs_defining(CSums, FunName, MD5) of
                               true ->
                                   F1 = list_to_binary(["var ", FunName, "=", F]),
                                   case js:define(JsCtx, F1) of
                                       ok ->
                                           {ok, dict:store(FunName, MD5, CSums)};
                                       Error ->
                                           {Error, CSums}
                                   end;
                               false ->
                                   {ok, CSums}
                           end,
    case Continue of
        ok ->
            case js:call(JsCtx, FunName, Args) of
                {ok, _} ->
                    case js:call(JsCtx, <<"Riak.getResults">>, []) of
                        {ok, Results} ->
                            {Results, NewCSums};
                        Err ->
                            {Err, CSums}
                    end;
                Err ->
                    {Err, CSums}
            end;
        Err ->
            {Err, CSums}
    end.

invoke_reduce(JsCtx, CSums, Args, Class, FunName, undefined) when Class =:= <<"Riak">> ->
    RealFunName = list_to_binary([Class, <<".">>, FunName]),
    case js:call(JsCtx, RealFunName, Args) of
        {ok, Results} ->
            {Results, CSums};
        Error ->
            {Error, CSums}
    end;

invoke_reduce(JsCtx, CSums, Args, undefined, FunName, F) ->
    MD5 = erlang:md5(F),
    {Continue, NewCSums} = case needs_defining(CSums, FunName, MD5) of
                               true ->
                                   F1 = list_to_binary(["var ", FunName, "=", F]),
                                   case js:define(JsCtx, F1) of
                                       ok ->
                                           {ok, dict:store(FunName, MD5, CSums)};
                                       Error ->
                                           {Error, CSums}
                                   end;
                               false ->
                                   {ok, CSums}
                           end,
    case Continue of
        ok ->
            case js:call(JsCtx, FunName, Args) of
                {ok, Results} ->
                    {Results, NewCSums};
                Err ->
                    {Err, CSums}
            end;
        Err ->
            {Err, CSums}
    end.

needs_defining(CSums, FunName, CSum) ->
    case dict:find(FunName, CSums) of
        error ->
            true;
        {ok, OldCSum} ->
            not(OldCSum =:= CSum)
    end.

fetch_fun(Bucket, Key) ->
    {ok, Client} = riak:local_client(),
    Source = case Client:get(Bucket, Key) of
                 {ok, Obj} ->
                     riak_object:get_value(Obj);
                 Error ->
                     error_logger:error_report(Error),
                     <<>>
                         end,
    Client:stop(),
    Source.

%% Internal functions
priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir(riak) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.

load_init_source() ->
    case js_cache:fetch("mapred_builtins") of
        none ->
            {ok, Contents} = file:read_file(filename:join([priv_dir(), "mapred_builtins.js"])),
            js_cache:store("mapred_builtins", Contents),
            {ok, Contents};
        Contents ->
            {ok, Contents}
    end.
