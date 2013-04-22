-module(intercept).
%% Export explicit API but also send compile directive to export all
%% becuase some of these private functions are useful in their own
%% right.
-export([add/3]).
-compile(export_all).

-type abstract_code() :: term().
-type form() :: term().
-type proplist(K, V) :: proplists:proplist(K, V).
-type fun_name() :: atom().
-type target_fun() :: {fun_name(), arity()}.
-type intercept_fun() :: fun_name().
-type mapping() :: proplist(target_fun(), intercept_fun()).
-type form_mod() :: fun((form()) -> form()).
-type code_mod() :: fun((form(), abstract_code()) -> abstract_code()).

%% The "original" is the `Target' module with the suffix `_orig'.  It
%% is where original code for the `Target' module resides after
%% intercepts are added.
-define(ORIGINAL(Mod), list_to_atom(atom_to_list(Mod) ++ "_orig")).
-define(FAKE_LINE_NO,1).

%% @doc Add intercepts against the `Target' module.
%%
%% `Target' - The module on which to intercept calls.
%%            E.g. `hashtree'.
%%
%% `Intercept' - The module containing intercept definitions.
%%               E.g. `hashtree_intercepts'
%%
%% `Mapping' - The mapping from target functions to intercept
%%             functions.
%%
%%             E.g. `[{{update_perform,2}, sleep_update_perform}]'
-spec add(module(), module(), mapping()) -> ok.
add(Target, Intercept, Mapping) ->
    Original = ?ORIGINAL(Target),
    TargetAC = get_abstract_code(Target),

    ProxyAC = make_proxy_abstract_code(Target, Intercept, Mapping,
                                       Original, TargetAC),
    OrigAC = make_orig_abstract_code(Target, Original, TargetAC),

    ok = compile_and_load(Original, OrigAC),
    ok = compile_and_load(Target, ProxyAC).

%% @private
%%
%% @doc Compile the abstract code `AC' and load it into the code server.
-spec compile_and_load(module(), abstract_code()) -> ok.
compile_and_load(Module, AC) ->
    {ok, Module, Bin} = compile:forms(AC),
    {module, Module} = code:load_binary(Module, atom_to_list(Module), Bin),
    ok.

%% @private
-spec make_orig_abstract_code(module(), module(), abstract_code()) ->
                                      abstract_code().
make_orig_abstract_code(Target, OrigName, TargetAC) ->
    export_all(move_all_funs(Target, change_module_name(OrigName, TargetAC))).

%% @private
%%
%% @doc Make the abstract code for the proxy module.  The proxy module
%%      sits in place of the original module and decides whether to
%%      forward to the `Intercept' or the `Original' depending on the
%%      `Mapping'.
-spec make_proxy_abstract_code(module(), module(), mapping(),
                               module(), abstract_code()) ->
                                      abstract_code().
make_proxy_abstract_code(Target, Intercept, Mapping, Original, TargetAC) ->
    AC1 = forward_all(Original, TargetAC),
    AC2 = export_all(change_module_name(Target, AC1)),
    apply_intercepts(AC2, Intercept, Mapping).


%% @private
%%
%% @doc Apply intercepts to the abstract code `AC' based on `Mapping'.
-spec apply_intercepts(abstract_code(), module(), mapping()) -> abstract_code().
apply_intercepts(AC, Intercept, Mapping) ->
    apply_to_funs(mapping_fun(Intercept, Mapping), AC).

%% @private
%%
%% @doc Return a form modifier function that uses `Mapping' to
%%      determine if a function should be modified to forward to the
%%      `Intercept' module.
-spec mapping_fun(module(), proplists:proplist()) -> form_mod().
mapping_fun(Intercept, Mapping) ->
    fun(Form) ->
            Key = {fun_name(Form), fun_arity(Form)},
            case proplists:get_value(Key, Mapping, '$none') of
                '$none' ->
                    Form;
                InterceptFunName ->
                    forward(Intercept, InterceptFunName, Form)
            end
    end.

%% @private
%%
%% @doc Modify the abstract code `AC' to forward all function calls to
%%      `Module' and move the original definitions under
%%      `<function_name>_orig'.
-spec move_all_funs(module(), abstract_code()) -> abstract_code().
move_all_funs(Module, AC) ->
    lists:reverse(lists:foldl(move_all_funs(Module), [], AC)).

%% @private
%%
%% @doc Return a function which folds over the abstract code of a
%%      module, represented by `Form'.  Every function is modified to
%%      forward to `ModuleName' and it's original definition is stored
%%      under `<function_name>_orig'.
-spec move_all_funs(module()) -> code_mod().
move_all_funs(ModuleName) ->
    fun(Form, NewAC) ->
            case is_fun(Form) of
                false ->
                    [Form|NewAC];
                true ->
                    %% Move current function code under different name
                    Name = fun_name(Form),
                    OrigForm = setelement(3, Form, ?ORIGINAL(Name)),

                    %% Modify original function to forward to `ModuleName'
                    FwdForm = forward(ModuleName, Name, Form),
                    [FwdForm,OrigForm|NewAC]
            end
    end.

%% @private
%%
%% @doc Modify all function definitions in the abstract code `AC' to
%%      forward to `Module:FunName_orig'.
-spec forward_all(module(), abstract_code()) -> abstract_code().
forward_all(Module, AC) ->
    F = fun(Form) ->
                forward(Module, ?ORIGINAL(fun_name(Form)), Form)
        end,
    apply_to_funs(F, AC).

%% @private
%%
%% @doc Modify the function `Form' to forward to `Module:FunName'.
-spec forward(module(), atom(), form()) -> form().
forward(Module, FunName, Form) ->
    Clause = hd(fun_clauses(Form)),
    Args = clause_args(Clause),
    NumArgs = length(Args),
    GenArgs = [{var,?FAKE_LINE_NO,list_to_atom("Arg" ++ integer_to_list(I))}
               || I <- lists:seq(1,NumArgs)],
    Clause2 = clause_set_args(Clause, GenArgs),
    Clause3 = clause_clear_guards(Clause2),
    Body = [{call, 1, {remote,1,{atom,1,Module},{atom,1,FunName}}, GenArgs}],
    Clause4 = clause_set_body(Clause3, Body),
    fun_set_clauses(Form, [Clause4]).

change_module_name(NewName, AC) ->
    lists:keyreplace(module, 3, AC, {attribute,1,module,NewName}).

%% @private
%%
%% @doc Add the `export_all' compile directive to the abstract code `AC'.
export_all(AC) ->
    [A,B|Rest] = AC,
    [A,B,{attribute,2,compile,export_all}|Rest].

%% @private
%%
%% @doc Apply the form modify `F' to all forms in `AC' that are
%%      function definitions.
-spec apply_to_funs(form_mod(), abstract_code()) -> abstract_code().
apply_to_funs(F, AC) ->
    F2 = apply_if_fun_def(F),
    lists:map(F2, AC).

%% @private
%%
%% @doc Get the abstract code for `Module'.  This function assumes
%%      code is compiled with `debug_info'.
-spec get_abstract_code(module()) -> abstract_code().
get_abstract_code(Module) ->
    {_, Bin, _} = code:get_object_code(Module),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Bin, [abstract_code]),
    AC.

%% @private
apply_if_fun_def(Fun) ->
    fun(Form) when element(1, Form) == function -> Fun(Form);
       (Form) -> Form
    end.

%% @private
is_fun(Form) ->
    element(1, Form) == function.

%% @private
clause_args(Form) ->
    element(3, Form).

%% @private
clause_set_args(Form, Args) ->
    setelement(3, Form, Args).

%% @private
clause_clear_guards(Form) ->
    setelement(4, Form, []).

%% @private
clause_set_body(Form, Body) ->
    setelement(5, Form, Body).

%% @private
fun_arity(Form) ->
    element(4, Form).

%% @private
fun_clauses(Form) ->
    element(5, Form).

%% @private
fun_set_clauses(Form, Clauses) ->
    setelement(5, Form, Clauses).

%% @private
fun_name(Form) ->
    element(3, Form).
