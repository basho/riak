-module(jsd_generator).

-compile([export_all]).
%-export([generate/2]).

start_proc_count() ->
    spawn(fun() -> proc_count([], []) end).

proc_count([], []) ->
    timer:sleep(5000),
    proc_count(erlang:processes(), []);
proc_count(Last, Accum) ->
    timer:sleep(5000),
    Current = erlang:processes(),
    case diff(Current, Last) of
        [] ->
            proc_count(Current, display_procs(Accum));
        Procs ->
            proc_count(Current, display_procs(Procs ++ Accum))
    end.

display_procs(Procs) ->
    case prune(Procs) of
        [] ->
            [];
        Orphans ->
            Info = get_info(Orphans),
            lists:foreach(fun({Pid, Mod}) ->
                                  io:format("~p: ~p~n", [Pid, Mod]) end, Info),
            io:format("~p total orphans~n", [length(Orphans)]),
            Orphans
    end.

prune(Procs) ->
    lists:filter(fun(P) ->
                         lists:member(P, erlang:processes()) end, Procs).

diff(Current, Last) ->
    lists:foldl(fun(C, A) ->
                        case lists:member(C, Last) of
                            false ->
                                [C|A];
                            true ->
                                A
                        end end, [], Current).

get_info(Procs) ->
    lists:map(fun(P) ->
                      {_, {Mod, _, _}} = erlang:process_info(P, current_function),
                      {P, Mod} end, Procs).

generate(Client, Count) ->
    rand_init(),
    Records = generate_data(Client, Count, []),
    Client:put(riak_object:new(<<"customers">>, <<"customer_list">>, Records), 1).

stress(Count) ->
    {ok, C} = riak:client_connect('riak@127.0.0.1'),
    jsd_generator:generate(C, 1000),
    Start = erlang:now(),
    stress(C, Count),
    End = erlang:now(),
    Elapsed = timer:now_diff(End, Start),
    {Elapsed, Elapsed / Count}.

stress(_Client, 0) ->
    io:format("~n"),
    ok;
stress(Client, Count) ->
    if
        Count rem 1000 == 0 ->
            io:format(".");
        true ->
            nop
    end,
    %M = fun(Obj, _, _) ->
    %            Values = riak_object:get_value(Obj), [proplists:get_value(avg_sales, V) || V <- Values] end,
    %R = fun(Values, _) ->
    %            lists:sum(Values) / length(Values) end,
    M = <<"function(values, key_data, arg) { return values.map(function(value) { return value[\"avg_sales\"]; });};">>,
    R = <<"function(values, arg) { var accum = 0; values.map(function(v) { accum = accum + v;}); return accum / values.length; };">>,
    {ok, _} = Client:mapred([{<<"customers">>, <<"customer_list">>}], [{map, {jsanon, M}, none, false},
                                                                       {reduce, {jsanon, R}, none, true}]),
    stress(Client, Count - 1).


%% Internal functions
generate_data(_Client, 0, Accum) ->
    lists:reverse(Accum);
generate_data(Client, Count, Accum) ->
    generate_data(Client, Count - 1, [generate_record(Count)|Accum]).

generate_record(Count) ->
    [{id, generate_id(Count)},
     {name, generate_name()},
     {sales_group, rand_int(8)},
     {avg_sales, rand_float(500)},
     {last_sales_date, rand_timestamp(120)}].

generate_id(Id) ->
    list_to_binary(pad_id(integer_to_list(Id))).

pad_id(Id) when length(Id) < 9 ->
    pad_id(["0"|Id]);
pad_id(Id) ->
    Id.

rand_timestamp(Limit) ->
    Now = erlang:now(),
    T1 = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)) -
         calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    T1 - (rand_int(Limit) * 86400).

generate_name() ->
    FirstName = rand_string(),
    LastName = rand_string(),
    list_to_binary([LastName, ", ", FirstName]).

rand_float(Limit) ->
    [F] = io_lib:format("~6.2f", [(random:uniform() * Limit)]),
    list_to_float(string:strip(lists:flatten(F))).

rand_int() ->
    rand_int(2000000).

rand_int(Limit) ->
    random:uniform(Limit).

rand_init() ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3).

rand_string() ->
    rand_string([rand_alpha()], rand_int(15)).

rand_string(Accum, 0) ->
    lists:reverse(Accum);
rand_string(Accum, Count) ->
    rand_string([rand_alpha(lower)|Accum], Count - 1).

rand_alpha(lower) ->
    rand_alpha() + 31.

rand_alpha() ->
    random:uniform(26) + 64.

integer_to_binary(N) ->
    list_to_binary(integer_to_list(N)).
