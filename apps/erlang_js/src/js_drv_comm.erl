-module(js_drv_comm).

-export([pack/2]).

pack(Cmd, Terms) when length(Cmd) == 2 ->
    NewTerms = lists:foldr(fun(T, Acc) -> marshal_data(T, Acc) end, [], Terms),
    list_to_binary(lists:flatten([[Cmd], [NewTerms]])).

%% Internal functions
marshal_data(Term, Acc) when is_integer(Term) ->
    [<<Term:32>>|Acc];
marshal_data(Term, Acc) when is_list(Term) ->
    marshal_data(list_to_binary(Term), Acc);
marshal_data(Term, Acc) when is_binary(Term) ->
    S = size(Term),
    [[<<S:32>>, Term]|Acc].
