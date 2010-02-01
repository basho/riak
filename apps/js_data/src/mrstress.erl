-module(mrstress).

-compile([export_all]).

populate(InputSize) ->
    {ok, Client} = riak:client_connect('riak@127.0.0.1'),
    create_entries(Client, generate_inputs(<<"stress">>, InputSize)).

create_entries(_Client, []) ->
    ok;
create_entries(Client, [{Bucket, Key}|T]) ->
    Obj = riak_object:new(Bucket, Key, <<"1">>),
    Md = dict:store(<<"content-type">>, "text/plain", dict:new()),
    Client:put(riak_object:update_metadata(Obj, Md), 1),
    create_entries(Client, T).

config(Lang, Clients, Count, KeyCount) ->
    [{lang, Lang}, {clients, Clients}, {count, Count}, {keys, KeyCount}].

stress(Config) ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    Lang = proplists:get_value(lang, Config),
    Count = proplists:get_value(count, Config, 100),
    Clients = proplists:get_value(clients, Config, 1),
    KeyCount = proplists:get_value(keys, Config, 10),
    InputPercent = proplists:get_value(input_size, Config, 0.15),
    InputSize = erlang:trunc(KeyCount * InputPercent),
    io:format("Using ~p out of ~p entries per mapred call~n", [InputSize, KeyCount]),
    populate(KeyCount),
    RawSuffix = integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(erlang:now()))),
    Suffix = string:substr(RawSuffix, length(RawSuffix) - 5),
    LogFile = proplists:get_value(log_file, Config, "/tmp/stress_" ++ Suffix ++ ".log"),
    stress_collector:start(LogFile),
    Inputs = generate_inputs(<<"stress">>, KeyCount),
    start_test(Lang, Count, Clients, InputSize, Inputs),
    wait_for_end(Clients).

wait_for_end(0) ->
    timer:sleep(1000),
    stress_collector:test_complete();
wait_for_end(Clients) ->
    receive
        done ->
            wait_for_end(Clients - 1)
    end.

start_test(_Lang, _Count, 0, _, _) ->
    ok;
start_test(Lang, Count, Clients, InputSize, Inputs) ->
    Owner = self(),
    spawn(fun() -> {ok, Client} = riak:client_connect('riak@127.0.0.1'),
                   stress(Lang, Count, Client, Owner, Inputs, InputSize) end),
    start_test(Lang, Count, Clients - 1, InputSize, Inputs).

stress(_Lang, 0, _Client, Owner, _, _) ->
    Owner ! done,
    ok;
stress(javascript, Count, Client, Owner, Inputs, InputSize) ->
    %M = <<"function(v, _, _) { var value = v[\"values\"][0][\"data\"]; return [parseInt(value)]; }">>,
    R = <<"function(v, _) { var sum = 0; v.forEach(function(x) { sum = sum + x; }); return [sum]; }">>,
    R1 = <<"function(values, _) { return values.map(function(v) { return parseInt(v); }); }">>,
    %Selected = select_inputs(Inputs, InputSize, []),
    Selected = <<"stress">>,
    Start = erlang:now(),
    case Client:mapred_bucket_stream(Selected, [{map, {jsfun, <<"Riak.mapValues">>}, none, false},
                                                {reduce, {jsanon, R1}, none, false},
                                                {reduce, {jsanon, R}, none, true}]) of
%%     case Client:mapred(Selected, [{map, {jsfun, <<"Riak.mapValues">>}, none, false},
%%                                   {reduce, {jsanon, R1}, none, false},
%%                                   {reduce, {jsanon, R}, none, true}]) of
        {ok, [InputSize]} ->
            End = erlang:now(),
            stress_collector:log(timer:now_diff(End, Start), 0),
            stress(javascript, Count - 1, Client, Owner, Inputs, InputSize);
        _Error ->
            End = erlang:now(),
            io:format("Error: ~p~n", [_Error]),
            stress_collector:log(0, timer:now_diff(End, Start)),
            stress(javascript, 0, Client, Owner, Inputs, InputSize)
    end;

stress(erlang, Count, Client, Owner, Inputs, InputSize) ->
    M = fun(Obj, _, _) ->
                Value = riak_object:get_value(Obj),
                [list_to_integer(binary_to_list(Value))] end,
    R = fun(Values, _) -> [lists:sum(Values)] end,
    Selected = select_inputs(Inputs, InputSize, []),
    Correct = length(Selected),
    Start = erlang:now(),
    case Client:mapred(Selected, [{map, {qfun, M}, none, false},
                                {reduce, {qfun, R}, none, true}]) of
        {ok, [Correct]} ->
            End = erlang:now(),
            stress_collector:log(timer:now_diff(End, Start), 0),
            stress(erlang, Count - 1, Client, Owner, Inputs, InputSize);
        _Error ->
            End = erlang:now(),
            io:format("Error: ~p~n", [_Error]),
            stress_collector:log(0, timer:now_diff(End, Start)),
            stress(erlang, Count, Client, Owner, Inputs, InputSize)
    end.

generate_inputs(Bucket, Size) ->
    [{Bucket, list_to_binary("test" ++ integer_to_list(X))} || X <- lists:seq(1, Size)].

select_inputs(_Inputs, InputSize, Accum) when length(Accum) == InputSize ->
    Accum;
select_inputs(Inputs, InputSize, Accum) ->
    Pos = random:uniform(InputSize),
    Input = lists:nth(Pos, Inputs),
    case lists:member(Input, Accum) of
        false ->
            select_inputs(Inputs, InputSize, [Input|Accum]);
        true ->
            select_inputs(Inputs, InputSize, Accum)
    end.
