-module(js).

-export([define/2, define/3, eval/2, call/3, call/4]).

define(Ctx, Js) ->
    define(Ctx, Js, []).

define(Ctx, Js, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    FinalJs = iolist_to_binary([JsBindings, Js]),
    js_driver:define_js(Ctx, FinalJs).

eval(Ctx, Js) ->
    js_driver:eval_js(Ctx, Js).

call(Ctx, FunctionName, Args) ->
    call(Ctx, FunctionName, Args, []).

call(Ctx, FunctionName, Args, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    ArgList = build_arg_list(Args, []),
    Js = iolist_to_binary([JsBindings, FunctionName, "(", ArgList, ");"]),
    js_driver:eval_js(Ctx, Js).

%% Internal functions
build_bindings([], Accum) ->
    Accum;
build_bindings([{VarName, Value}|T], Accum) ->
    FinalVarName = case is_atom(VarName) of
                       true ->
                           atom_to_list(VarName);
                       false ->
                           VarName
                   end,
    build_bindings(T, [["var ", FinalVarName, "=", mochijson2:encode(Value), ";\n"]|Accum]).

build_arg_list([], Accum) ->
    lists:reverse(Accum);
build_arg_list([H|[]], Accum) ->
    build_arg_list([], [mochijson2:encode(H)|Accum]);
build_arg_list([H|T], Accum) ->
    build_arg_list(T, [[mochijson2:encode(H), ","]|Accum]).
