-module(jsd_generator).

-compile([export_all]).
%-export([generate/2]).

generate(Client, Count) ->
    rand_init(),
    Records = generate_data(Client, Count, []),
    lists:foreach(fun(Record) ->
                          Obj = riak_object:new(<<"customers">>, <<"customer_list">>, Record),
                          Client:put(Obj, 1) end, Records).

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
