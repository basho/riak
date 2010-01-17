-module(riak_mapred_json).

-export([parse_inputs/1, parse_query/1]).

parse_inputs(Bucket) when is_binary(Bucket) ->
    {ok, Bucket};
parse_inputs(Targets) ->
    parse_inputs(Targets, []).

parse_inputs([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            error
    end;
parse_inputs([[Bucket, Key]|T], Accum) when is_binary(Bucket),
                                             is_binary(Key) ->
    parse_inputs(T, [{Bucket, Key}|Accum]);
parse_inputs([[Bucket, Key, KeyData]|T], Accum) when is_binary(Bucket),
                                                      is_binary(Key) ->
    parse_inputs(T, [{{Bucket, Key}, KeyData}|Accum]);
parse_inputs(_, _Accum) ->
    error.

parse_query(Query) ->
    parse_query(Query, []).

parse_query([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            error
    end;
parse_query([{struct, [{Type, {struct, StepDef}}]}|T], Accum)
  when Type =:= <<"map">>; Type =:= <<"reduce">>; Type =:= <<"link">> ->
    StepType = case Type of
                   <<"map">> -> map;
                   <<"reduce">> -> reduce;
                   <<"link">> -> link
               end,
    Keep = proplists:get_value(<<"keep">>, StepDef, T==[]),
    Step = case not(Keep =:= true orelse Keep =:= false) of
               true -> error;
               false ->
                   if StepType == link ->
                          case parse_link_step(StepDef) of
                              {ok, {Bucket, Tag}} ->
                                  {ok, {link, Bucket, Tag, Keep}};
                              LError ->
                                  LError
                          end;
                      true -> % map or reduce
                           Lang = proplists:get_value(<<"language">>, StepDef),
                           case parse_step(Lang, StepDef) of
                               error ->
                                   error;
                               {ok, ParsedStep} ->
                                   Arg = proplists:get_value(<<"arg">>, StepDef, none),
                                   {ok, {StepType, ParsedStep, Arg, Keep}}
                           end
                   end
           end,
    case Step of
        {ok, S} -> parse_query(T, [S|Accum]);
        SError  -> SError
    end;
parse_query(_, _Accum) ->
    error.

parse_link_step(StepDef) ->
    Bucket = proplists:get_value(<<"bucket">>, StepDef, <<"_">>),
    Tag = proplists:get_value(<<"tag">>, StepDef, <<"_">>),
    case not(is_binary(Bucket) andalso is_binary(Tag)) of
        true ->
            error;
        false ->
            {ok, {if Bucket == <<"_">> -> '_';
                     true              -> Bucket
                  end,
                  if Tag == <<"_">> -> '_';
                     true           -> Tag
                  end}}
    end.

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
