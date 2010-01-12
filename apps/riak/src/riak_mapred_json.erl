-module(riak_mapred_json).

-export([parse_targets/1, parse_query/1]).

parse_targets({struct, Targets}) ->
    parse_targets(Targets, []).

parse_targets([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            error
    end;
parse_targets([{Bucket, Key}|T], Accum) when is_binary(Bucket),
                                                         is_binary(Key) ->
    parse_targets(T, [{Bucket, Key}|Accum]);
parse_targets([Bucket|T], Accum) when is_binary(Bucket) ->
    parse_targets(T, [Bucket|Accum]).

parse_query(Query) ->
    parse_query(Query, []).

parse_query([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            error
    end;
parse_query([Type, {struct, StepDef}|T], Accum) when Type =:= <<"map">>;
                                                       Type =:= <<"reduce">> ->
    StepType = case Type of
                   <<"map">> -> map;
                   <<"reduce">> -> reduce
               end,
    Lang = proplists:get_value(<<"language">>, StepDef),
    Keep = proplists:get_value(<<"keep">>, StepDef),
    case not(Keep =:= true orelse Keep =:= false) of
        true ->
            error;
        false ->
            case parse_step(Lang, StepDef) of
                error ->
                    error;
                {ok, ParsedStep} ->
                    Arg = proplists:get_value(<<"arg">>, StepDef, none),
                    parse_query(T, [{StepType, ParsedStep, Arg, Keep}|Accum])
            end
    end;
parse_query(_, _Accum) ->
    error.

parse_step(<<"javascript">>, StepDef) ->
    Source = proplists:get_value(<<"source">>, StepDef),
    Name = proplists:get_value(<<"name">>, StepDef),
    Bucket = proplists:get_value(<<"bucket">>, StepDef),
    Key = proplists:get_value(<<"key">>, StepDef),
    case Source of
        undefined ->
            case Name of
                undefined ->
                    case Bucket of
                        undefined ->
                            error;
                        _ ->
                            case Key of
                                undefined ->
                                    error;
                                _ ->
                                    {ok, {jsanon, {Bucket, Key}}}
                            end
                    end;
                _ ->
                    {ok, {jsfun, Name}}
            end;
        _ ->
            {ok, {jsanon, Source}}
    end;
parse_step(<<"erlang">>, StepDef) ->
    Module = proplists:get_value(<<"module">>, StepDef),
    Function = proplists:get_value(<<"function">>, StepDef),
    bin_to_atom(Module, fun(A1) ->
                                   bin_to_atom(Function, fun(A2) -> {modfun, A1, A2} end) end).

bin_to_atom(Binary, Cont) ->
    L = binary_to_list(Binary),
    Result = try
                 list_to_existing_atom(L)
             catch
                 error:badarg ->
                     try
                         list_to_atom(L)
                     catch
                         error:badarg ->
                             error
                     end
             end,
    case Result of
        error ->
            error;
        _ ->
            Cont(Result)
    end.
