%% -------------------------------------------------------------------
%%
%% riak_js_vm: interaction with JavaScript VMs
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc interaction with JavaScript VMs

-module(riak_kv_js_vm).

-behaviour(gen_server).

-define(MAX_ANON_FUNS, 25).

%% API
-export([start_link/1, dispatch/4, blocking_dispatch/3, reload/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {manager, ctx, next_funid=1, anon_funs=[]}).

start_link(Manager) ->
    gen_server:start_link(?MODULE, [Manager], []).

dispatch(VMPid, Requestor, JobId, JSCall) ->
    gen_server:cast(VMPid, {dispatch, Requestor, JobId, JSCall}).

blocking_dispatch(VMPid, JobId, JSCall) ->
    gen_server:call(VMPid, {dispatch, JobId, JSCall}, 10000).

reload(VMPid) ->
    gen_server:cast(VMPid, reload).

init([Manager]) ->
    HeapSize = case app_helper:get_env(riak_kv, js_max_vm_mem, 8) of
                   N when is_integer(N) ->
                       N;
                   _ ->
                       8
               end,
    case new_context(HeapSize) of
        {ok, Ctx} ->
            error_logger:info_msg("Spidermonkey VM (max heap: ~pMB) host starting (~p)~n", [HeapSize, self()]),
            riak_kv_js_manager:add_to_manager(),
            erlang:monitor(process, Manager),
            {ok, #state{manager=Manager, ctx=Ctx}};
        Error ->
            {stop, Error}
    end.

%% Reduce phase with anonymous function
handle_call({dispatch, _JobId, {{jsanon, JS}, Reduced, Arg}}, _From, #state{ctx=Ctx}=State) ->
    {Reply, UpdatedState} = case define_anon_js(JS, State) of
                                {ok, FunName, NewState} ->
                                    case invoke_js(Ctx, FunName, [Reduced, Arg]) of
                                        {ok, R} ->
                                            {{ok, R}, NewState};
                                        Error ->
                                            {Error, State}
                                    end;
                                {Error, undefined, NewState} ->
                                    {Error, NewState}
                            end,
    {reply, Reply, UpdatedState};
%% Reduce phase with named function
handle_call({dispatch, _JobId, {{jsfun, JS}, Reduced, Arg}}, _From, #state{ctx=Ctx}=State) ->
    {reply, invoke_js(Ctx, JS, [Reduced, Arg]), State};
%% Pre-commit hook with named function
handle_call({dispatch, _JobId, {{jsfun, JS}, Obj}}, _From, #state{ctx=Ctx}=State) ->
    {reply, invoke_js(Ctx, JS, [riak_object:to_json(Obj)]), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(reload, #state{ctx=Ctx}=State) ->
    init_context(Ctx),
    error_logger:info_msg("Spidermonkey VM host reloaded (~p)~n", [self()]),
    {noreply, State};

%% Map phase with anonymous function
handle_cast({dispatch, Requestor, _JobId, {FsmPid, {map, {jsanon, JS}, Arg, _Acc},
                                            Value,
                                            KeyData, _BKey}}, #state{ctx=Ctx}=State) ->
    {Result, UpdatedState} = case define_anon_js(JS, State) of
                                {ok, FunName, NewState} ->
                                    JsonValue = riak_object:to_json(Value),
                                    JsonArg = jsonify_arg(Arg),
                                    case invoke_js(Ctx, FunName, [JsonValue, KeyData, JsonArg]) of
                                        {ok, R} ->
                                            {{ok, R}, NewState};
                                        Error ->
                                            {Error, State}
                                    end;
                                {Error, undefined, NewState} ->
                                    {Error, NewState}
                            end,
    case Result of
        {ok, ReturnValue} ->
            gen_fsm:send_event(FsmPid, {mapexec_reply, ReturnValue, Requestor}),
            {noreply, UpdatedState};
        ErrorResult ->
            gen_fsm:send_event(FsmPid, {mapexec_error_noretry, Requestor, ErrorResult}),
            {noreply, State}
    end;

%% Map phase with named function
handle_cast({dispatch, Requestor, _JobId, {FsmPid, {map, {jsfun, JS}, Arg, _Acc},
                                            Value,
                                            KeyData, BKey}}, #state{ctx=Ctx}=State) ->
    JsonValue = riak_object:to_json(Value),
    JsonArg = jsonify_arg(Arg),
    case invoke_js(Ctx, JS, [JsonValue, KeyData, JsonArg]) of
        {ok, R} ->
            %% Requestor should be the dispatching vnode
            gen_fsm:send_event(Requestor, {mapcache, BKey, {JS, Arg, KeyData}, R}),
            gen_fsm:send_event(FsmPid, {mapexec_reply, R, Requestor});
        Error ->
            gen_fsm:send_event(FsmPid, {mapexec_error_noretry, Requestor, Error})
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, Manager, _Info}, #state{manager=Manager}=State) ->
    {stop, normal, State#state{manager=undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ctx=Ctx}) ->
    js_driver:destroy(Ctx),
    error_logger:info_msg("Spidermonkey VM host stopping (~p)~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
invoke_js(Ctx, Js, Args) ->
    try
        case js:call(Ctx, Js, Args) of
            {ok, {struct, R}} ->
                case proplists:get_value(<<"lineno">>, R) of
                    undefined ->
                        {ok, R};
                    _ ->
                        {error, R}
                end;
            R ->
                R
        end
    catch
        exit: {ucs, {bad_utf8_character_code}} ->
            error_logger:error_msg("Error JSON encoding arguments: ~p~n", [Args]),
            {error, bad_encoding};
        exit: {json_encode, _} ->
            {error, bad_json};
        throw:invalid_utf8 ->
            {error, bad_encoding}
    end.

define_anon_js(JS, #state{ctx=Ctx, anon_funs=AnonFuns, next_funid=NextFunId}=State) ->
    Hash = erlang:phash2(JS),
    case proplists:get_value(Hash, AnonFuns) of
        undefined ->
            FunId = case NextFunId > ?MAX_ANON_FUNS of
                        true ->
                            1;
                        false ->
                            NextFunId
                    end,
            FunName = list_to_binary("riakAnon" ++ integer_to_list(FunId)),
            case js:define(Ctx, list_to_binary([<<"var ">>, FunName, <<"=">>, JS, <<";">>])) of
                ok ->
                    {ok, FunName, State#state{anon_funs=[{Hash, FunName}|AnonFuns], next_funid=NextFunId + 1}};
                Error ->
                    error_logger:warning_msg("Error defining anonymous Javascript function: ~p~n", [Error]),
                    {error, undefined, State}
            end;
        FunName ->
            {ok, FunName, State}
    end.

new_context(HeapSize) ->
    InitFun = fun(Ctx) -> init_context(Ctx) end,
    js_driver:new(HeapSize, InitFun).

init_context(Ctx) ->
    load_user_builtins(Ctx),
    load_mapred_builtins(Ctx).

priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir(riak_kv) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.

load_user_builtins(Ctx) ->
    case app_helper:get_env(riak_kv, js_source_dir, undefined) of
        undefined ->
            ok;
        Path ->
            Files = filelib:wildcard("*.js", Path),
            lists:foreach(fun(File) ->
                                  {ok, Contents} = file:read_file(filename:join([Path, File])),
                                  js:define(Ctx, Contents) end, Files)
    end.

load_mapred_builtins(Ctx) ->
    {ok, Contents} = file:read_file(filename:join([priv_dir(), "mapred_builtins.js"])),
    js:define(Ctx, Contents).

jsonify_arg({Bucket,Tag}) when (Bucket == '_' orelse is_binary(Bucket)),
                               (Tag == '_' orelse is_binary(Tag)) ->
    %% convert link match syntax
    {struct, [{<<"bucket">>,Bucket},
              {<<"tag">>,Tag}]};
jsonify_arg([H|_]=Other) when is_tuple(H);
                              is_atom(H) ->
    {struct, Other};
jsonify_arg(Other) ->
    Other.
