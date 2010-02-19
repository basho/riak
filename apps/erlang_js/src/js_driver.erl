%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc This module manages all of the low-level details surrounding the
%% linked-in driver. It is reponsible for loading and unloading the driver
%% as needed. This module is also reponsible for creating and destroying
%% instances of Javascript VMs.

-module(js_driver).

-export([load_driver/0, new/0, new/1, destroy/1, shutdown/1]).
-export([define_js/2, define_js/3, define_js/4, eval_js/2, eval_js/3]).

-define(SCRIPT_TIMEOUT, 5000).
-define(DRIVER_NAME, "erlang_js_drv").

%% @spec load_driver() -> true | false
%% @doc Attempt to load the Javascript driver
load_driver() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
        false ->
            case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
                ok ->
                    true;
                {error, Error} ->
                    error_logger:error_msg("Error loading ~p: ~p~n", [?DRIVER_NAME, erl_ddll:format_error(Error)]),
                    false
            end;
        true ->
            true
    end.

%% @spec new() -> {ok, port()} | {error, atom()} | {error, any()}
%% @doc Create a new Javascript VM instance and preload Douglas Crockford's
%% json2 converter (http://www.json.org/js.html)
new() ->
    {ok, Port} = new(no_json),
    %% Load json converter for use later
    case define_js(Port, <<"json2.js">>, json_converter(), ?SCRIPT_TIMEOUT) of
        ok ->
            {ok, Port};
        {error, Reason} ->
            port_close(Port),
            {error, Reason}
    end.

%% @type init_fun() = function(port()).
%% @spec new(no_json | init_fun() | {ModName::atom(), FunName::atom()}) -> {ok, port()} | {error, atom()} | {error, any()}
%% @doc Create a new Javascript VM instance. The function arguments control how the VM instance is initialized.
%% User supplied initializers must return true or false.
new(no_json) ->
    {ok, open_port({spawn, ?DRIVER_NAME}, [binary])};
new(Initializer) when is_function(Initializer) ->
    {ok, Port} = new(),
    case Initializer(Port) of
        ok ->
            {ok, Port};
        {error, Error} ->
            js_driver:destroy(Port),
            error_logger:error_report(Error),
            throw({error, init_failed})
    end;
new({InitMod, InitFun}) ->
    {ok, Port} = new(),
    case InitMod:InitFun(Port) of
        ok ->
            {ok, Port};
        {error, Error} ->
            js_driver:destroy(Port),
            error_logger:error_report(Error),
            throw({error, init_failed})
    end.

%% @spec destroy(port()) -> ok
%% @doc Destroys a Javascript VM instance
destroy(Ctx) ->
    port_close(Ctx).

%% @spec shutdown(port()) -> ok
%% @doc Destroys a Javascript VM instance and shuts down the underlying Javascript infrastructure.
%% NOTE: No new VMs can be created after this call is made!
shutdown(Ctx) ->
    call_driver(Ctx, "sd", [], 60000),
    port_close(Ctx).

%% @spec define_js(port(), binary()) -> ok | {error, any()}
%% @doc Define a Javascript expression:
%% js_driver:define(Port, &lt;&lt;"var x = 100;"&gt;&gt;).
define_js(Ctx, Js) ->
    define_js(Ctx, Js, ?SCRIPT_TIMEOUT).

%% @private
define_js(Ctx, {file, FileName}, Timeout) ->
    {ok, File} = file:read_file(FileName),
    define_js(Ctx, list_to_binary(FileName), File, Timeout);
define_js(Ctx, Js, Timeout) when is_binary(Js) ->
    define_js(Ctx, <<"unnamed">>, Js, Timeout).

%% @spec define_js(port(), binary(), binary(), integer()) -> {ok, binary()} | {error, any()}
%% @doc Define a Javascript expression:
%% js_driver:define(Port, &lt;&lt;var blah = new Wubba();"&gt;&gt;).
%% Note: Filename is used only as a label for error reporting.
define_js(Ctx, FileName, Js, Timeout) when is_binary(FileName),
                                           is_binary(Js) ->
    case call_driver(Ctx, "dj", [FileName, Js], Timeout) of
        {error, ErrorJson} when is_binary(ErrorJson) ->
            {struct, [{<<"error">>, {struct, Error}}]} = js_mochijson2:decode(ErrorJson),
            {error, Error};
        ok ->
            ok
    end.

%% @spec eval_js(port(), binary()) -> {ok, any()} | {error, any()}
%% @doc Evaluate a Javascript expression and return the result
eval_js(Ctx, Js) ->
    eval_js(Ctx, Js, ?SCRIPT_TIMEOUT).

%% @private
eval_js(Ctx, {file, FileName}, Timeout) ->
    {ok, File} = file:read_file(FileName),
    eval_js(Ctx, File, Timeout);
eval_js(Ctx, Js, Timeout) when is_binary(Js) ->
    case call_driver(Ctx, "ej", [<<"<unnamed>">>, jsonify(Js)], Timeout) of
        {ok, Result} ->
            {ok, js_mochijson2:decode(Result)};
        {error, ErrorJson} when is_binary(ErrorJson) ->
            case js_mochijson2:decode(ErrorJson) of
                {struct, [{<<"error">>, {struct, Error}}]} ->
                    {error, Error};
                _ ->
                    {error, ErrorJson}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Internal functions
%% @private
jsonify(Code) when is_binary(Code) ->
    {Body, <<LastChar:8>>} = split_binary(Code, size(Code) - 1),
    C = case LastChar of
            $; ->
                Body;
            _ ->
                Code
        end,
    list_to_binary([<<"JSON.stringify(">>, C, <<");">>]).

%% @private
priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir(erlang_js) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.

%% @private
call_driver(Ctx, Command, Args, Timeout) ->
    CallToken = make_call_token(),
    Marshalled = js_drv_comm:pack(Command, [CallToken] ++ Args),
    port_command(Ctx, Marshalled),
    Result = receive
                 {CallToken, ok} ->
                     ok;
                 {CallToken, ok, R} ->
                     {ok, R};
                 {CallToken, error, Error} ->
                     {error, Error}
             after Timeout ->
                     {error, timeout}
             end,
    Result.

%% @private
make_call_token() ->
    list_to_binary(integer_to_list(erlang:phash2(erlang:make_ref()))).

%% @private
json_converter() ->
    FileName = filename:join([priv_dir(), "json2.js"]),
    case js_cache:fetch(FileName) of
        none ->
            {ok, Contents} = file:read_file(FileName),
            js_cache:store(FileName, Contents),
            Contents;
        Contents ->
            Contents
    end.
