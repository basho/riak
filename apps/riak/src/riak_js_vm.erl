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

-module(riak_js_vm).

-behaviour(gen_server).

%% API
-export([start_link/1, dispatch/4, blocking_dispatch/3, reload/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {manager, ctx, last_mapper, last_reducer}).

start_link(Manager) ->
    gen_server:start_link(?MODULE, [Manager], []).

dispatch(VMPid, Requestor, JobId, JSCall) ->
    gen_server:cast(VMPid, {dispatch, Requestor, JobId, JSCall}).

blocking_dispatch(VMPid, JobId, JSCall) ->
    gen_server:call(VMPid, {dispatch, JobId, JSCall}, 10000).

reload(VMPid) ->
    gen_server:cast(VMPid, reload).

init([Manager]) ->
    error_logger:info_msg("Spidermonkey VM host starting (~p)~n", [self()]),
    case new_context() of
        {ok, Ctx} ->
            riak_js_manager:add_to_manager(),
            erlang:monitor(process, Manager),
            {ok, #state{manager=Manager, ctx=Ctx}};
        Error ->
            {stop, Error}
    end.

handle_call({dispatch, _JobId, {{jsanon, JS}, Reduced, Arg}}, _From, #state{ctx=Ctx}=State) ->
    {Result, NewState} = case define_anon_js(reduce, JS, State) of
                             {ok, State1} ->
                                 case invoke_js(Ctx, <<"riakReducer">>, [Reduced, Arg]) of
                                     {ok, R} ->
                                         {{ok, R}, State1};
                                     Error ->
                                         {Error, State}
                                 end;
                             {Error, State1} ->
                                 {Error, State1}
                         end,
    {reply, Result, NewState};
handle_call({dispatch, _JobId, {{jsfun, JS}, Reduced, Arg}}, _From, #state{ctx=Ctx}=State) ->
    {reply, invoke_js(Ctx, JS, [Reduced, Arg]), State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(reload, #state{ctx=Ctx}=State) ->
    init_context(Ctx),
    error_logger:info_msg("Spidermonkey VM host reloaded (~p)~n", [self()]),
    {noreply, State};

handle_cast({dispatch, Requestor, _JobId, {FsmPid, {map, {jsanon, JS}, Arg, _Acc},
                                            Value,
                                            KeyData, _BKey}}, #state{ctx=Ctx}=State) ->
    {Result, NewState} = case define_anon_js(map, JS, State) of
                             {ok, State1} ->
                                 JsonValue = jsonify_object(Value),
                                 JsonArg = jsonify_arg(Arg),
                                 case invoke_js(Ctx, <<"riakMapper">>, [JsonValue, KeyData, JsonArg]) of
                                     {ok, R} ->
                                         {{ok, R}, State1};
                                     Error ->
                                         {Error, State}
                                 end;
                             {_, _}=Error->
                                 Error
                         end,
    case Result of
        {ok, ReturnValue} ->
            gen_fsm:send_event(FsmPid, {mapexec_reply, ReturnValue, Requestor}),
            {noreply, NewState};
        ErrorResult ->
            gen_fsm:send_event(FsmPid, {mapexec_error_noretry, Requestor, ErrorResult}),
            {noreply, State}
    end;
handle_cast({dispatch, Requestor, _JobId, {FsmPid, {map, {jsfun, JS}, Arg, _Acc},
                                            Value,
                                            KeyData, BKey}}, #state{ctx=Ctx}=State) ->
    JsonValue = jsonify_object(Value),
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

define_anon_js(Name, JS, #state{ctx=Ctx, last_mapper=LastMapper, last_reducer=LastReducer}=State) ->
    Hash = erlang:phash2(JS),
    {OldHash, FunName} = if
                             Name == map ->
                                 {LastMapper, <<"riakMapper">>};
                             true ->
                                 {LastReducer, <<"riakReducer">>}
                         end,
    if
        Hash == OldHash ->
            {ok, State};
        true ->
            case js:define(Ctx, list_to_binary([<<"var ">>, FunName, <<" = ">>, JS, <<";">>])) of
                ok ->
                    if
                        Name == map ->
                            {ok, State#state{last_mapper=Hash}};
                        true ->
                            {ok, State#state{last_reducer=Hash}}
                    end;
                {error, _}=Error ->
                    error_logger:warning_msg("Error defining Javascript expression: ~p~n", [Error]),
                    {Error, State}
            end
    end.

new_context() ->
    InitFun = fun(Ctx) -> init_context(Ctx) end,
    js_driver:new(InitFun).

init_context(Ctx) ->
    load_user_builtins(Ctx),
    load_mapred_builtins(Ctx).

priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir(riak) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.

load_user_builtins(Ctx) ->
    case riak:get_app_env(js_source_dir, undefined) of
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

jsonify_object({error, notfound}=Obj) ->
    {struct, [Obj]};
jsonify_object(Obj) ->
    {_,Vclock} = raw_http_resource:vclock_header(Obj),
    {struct, [{<<"bucket">>, riak_object:bucket(Obj)},
              {<<"key">>, riak_object:key(Obj)},
              {<<"vclock">>, list_to_binary(Vclock)},
              {<<"values">>,
               [{struct,
                 [{<<"metadata">>, jsonify_metadata(MD)},
                  {<<"data">>, V}]}
                || {MD, V} <- riak_object:get_contents(Obj)
                      ]}]}.

jsonify_metadata(MD) ->
    MDJS = fun({LastMod, Now={_,_,_}}) ->
                   % convert Now to JS-readable time string
                   {LastMod, list_to_binary(
                               httpd_util:rfc1123_date(
                                 calendar:now_to_local_time(Now)))};
              ({<<"Links">>, Links}) ->
                   {<<"Links">>, [ [B, K, T] || {{B, K}, T} <- Links ]};
              ({Name, List=[_|_]}) ->
                   {Name, jsonify_metadata_list(List)};
              ({Name, Value}) ->
                   {Name, Value}
           end,
    {struct, lists:map(MDJS, dict:to_list(MD))}.

%% @doc convert strings to binaries, and proplists to JSON objects
jsonify_metadata_list([]) -> [];
jsonify_metadata_list(List) ->
    Classifier = fun({Key,_}, Type) when (is_binary(Key) orelse is_list(Key)),
                                         Type /= array, Type /= string ->
                         struct;
                    (C, Type) when is_integer(C), C >= 0, C =< 256,
                                   Type /= array, Type /= struct ->
                         string;
                    (_, _) ->
                         array
                 end,
    case lists:foldl(Classifier, undefined, List) of
        struct -> {struct, [ {if is_list(Key) -> list_to_binary(Key);
                                 true         -> Key
                              end,
                              if is_list(Value) -> jsonify_metadata_list(Value);
                                 true           -> Value
                              end}
                             || {Key, Value} <- List]};
        string -> list_to_binary(List);
        array -> List
    end.

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
