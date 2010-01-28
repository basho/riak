-module(mrstress).

-compile([export_all]).

populate(InputSize) ->
    {ok, Client} = riak:client_connect('riak@127.0.0.1'),
    create_entries(Client, generate_inputs(<<"stress">>, InputSize)).

create_entries(_Client, []) ->
    ok;
create_entries(Client, [{Bucket, Key}|T]) ->
    Obj = riak_object:new(Bucket, Key, <<"1">>),
    FinalObj = case T of
                   [] ->
                       Obj;
                   _ ->
                       Next = hd(T),
                       Md = dict:store(<<"Links">>, [{Next, <<"next">>}], dict:new()),
                       Md1 = dict:store(<<"content-type">>, "text/plain", Md),
                       riak_object:update_metadata(Obj, Md1)
               end,
    ok = Client:put(FinalObj, 1),
    create_entries(Client, T).

config(Lang, Clients, Count, KeyCount) ->
    [{lang, Lang}, {clients, Clients}, {count, Count}, {keys, KeyCount}].

stress(Config) ->
    Lang = proplists:get_value(lang, Config),
    Count = proplists:get_value(count, Config, 100),
    Clients = proplists:get_value(clients, Config, 1),
    KeyCount = proplists:get_value(keys, Config, 10),
    populate(KeyCount),
    LogFile = proplists:get_value(log_file, Config, "/tmp/stress.log"),
    stress_collector:start(LogFile),
    start_test(Lang, Count, Clients, KeyCount),
    wait_for_end(Clients).

wait_for_end(0) ->
    timer:sleep(1000),
    stress_collector:test_complete();
wait_for_end(Clients) ->
    receive
        done ->
            wait_for_end(Clients - 1)
    end.

start_test(_Lang, _Count, 0, _) ->
    ok;
start_test(Lang, Count, Clients, KeyCount) ->
    Owner = self(),
    spawn(fun() -> {ok, Client} = riak:client_connect('riak@127.0.0.1'),
                   stress(Lang, Count, Client, Owner, KeyCount) end),
    start_test(Lang, Count, Clients - 1, KeyCount).

stress(_Lang, 0, _Client, Owner, _) ->
    Owner ! done,
    ok;
stress(javascript, Count, Client, Owner, KeyCount) ->
    M = <<"function(v, _, _) { var value = v[\"values\"][0][\"data\"]; return [parseInt(value)]; }">>,
    R = <<"function(v, _) { var sum = 0; v.forEach(function(x) { sum = sum + x; }); return [sum]; }">>,
    Inputs = generate_inputs(<<"stress">>, KeyCount),
    Correct = length(Inputs),
    Start = erlang:now(),
    case Client:mapred(Inputs, [{map, {jsanon, M}, none, false},
                                {reduce, {jsanon, R}, none, true}]) of
        {ok, [Correct]} ->
            End = erlang:now(),
            stress_collector:log(erlang:trunc(timer:now_diff(End, Start) / 1000), 0),
            stress(javascript, Count - 1, Client, Owner, KeyCount);
        _Error ->
            End = erlang:now(),
            stress_collector:log(0, erlang:trunc(timer:now_diff(End, Start) / 1000)),
            stress(javascript, 0, Client, Owner, KeyCount)
    end;

stress(erlang, Count, Client, Owner, KeyCount) ->
    M = fun(Obj, _, _) ->
                Value = riak_object:get_value(Obj),
                [list_to_integer(binary_to_list(Value))] end,
    R = fun(Values, _) -> [lists:sum(Values)] end,
    Inputs = generate_inputs(<<"stress">>, KeyCount),
    Correct = length(Inputs),
    Start = erlang:now(),
    case Client:mapred(Inputs, [{map, {qfun, M}, none, false},
                                {reduce, {qfun, R}, none, true}]) of
        {ok, [Correct]} ->
            End = erlang:now(),
            stress_collector:log(erlang:trunc(timer:now_diff(End, Start) / 1000), 0),
            stress(erlang, Count - 1, Client, Owner, KeyCount);
        _Error ->
            io:format("Error: ~p~n", [_Error]),
            End = erlang:now(),
            stress_collector:log(0, erlang:trunc(timer:now_diff(End, Start) / 1000)),
            stress(erlang, Count, Client, Owner, KeyCount)
    end.

generate_inputs(Bucket, Size) ->
    [{Bucket, list_to_binary("test" ++ integer_to_list(X))} || X <- lists:seq(1, Size)].
