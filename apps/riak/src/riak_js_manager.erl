%% -------------------------------------------------------------------
%%
%% riak_js_manager: dispatch work to JavaScript VMs
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

%% @doc dispatch work to JavaScript VMs

-module(riak_js_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, dispatch/1, blocking_dispatch/1, add_to_manager/0, reload/1, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tid}).

dispatch(JSCall) ->
    case select_random() of
        no_vms ->
            {error, no_vms};
        Target ->
            JobId = {Target, make_ref()},
            riak_js_vm:dispatch(Target, self(), JobId, JSCall),
            {ok, JobId}
    end.

blocking_dispatch(JSCall) ->
    case select_random() of
        no_vms ->
            {error, no_vms};
        Target ->
            JobId = {Target, make_ref()},
            riak_js_vm:blocking_dispatch(Target, JobId, JSCall)
    end.

%% Hack to allow riak-admin to trigger a reload
reload([]) ->
    reload().

reload() ->
    gen_server:call(?MODULE, reload_all_vm).

add_to_manager() ->
    gen_server:cast(?MODULE, {add_child, self()}).

start_link(ChildCount) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ChildCount], []).

init([ChildCount]) ->
    Tid = ets:new(?MODULE, [named_table]),
    start_children(ChildCount),
    {ok, #state{tid=Tid}}.

handle_call(reload_all_vm, _From, #state{tid=Tid}=State) ->
    ets:safe_fixtable(Tid, true),
    reload_children(ets:first(Tid), Tid),
    ets:safe_fixtable(Tid, false),
    VNodes = gen_server2:call(riak_vnode_master, all_vnodes),
    lists:foreach(fun(VNode) -> gen_fsm:send_event(VNode, purge_mapcache) end, VNodes),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({add_child, ChildPid}, #state{tid=Tid}=State) ->
    erlang:monitor(process, ChildPid),
    ets:insert_new(Tid, {ChildPid}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, Pid, _Info}, #state{tid=Tid}=State) ->
    case ets:lookup(Tid, Pid) of
        [] ->
            {noreply, State};
        [{Pid}] ->
            ets:delete(?MODULE, Pid),
            riak_js_sup:start_js(self()),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
start_children(0) ->
    ok;
start_children(Count) ->
    riak_js_sup:start_js(self()),
    start_children(Count - 1).

select_random() ->
    case ets:match(?MODULE, {'$1'}) of
        [] ->
            no_vms;
        Members ->
            {T1, T2, T3} = erlang:now(),
            random:seed(T1, T2, T3),
            Pos = pick_pos(erlang:get(?MODULE), length(Members)),
            [Member] = lists:nth(Pos, Members),
            Member
    end.

pick_pos(undefined, Size) ->
    Pos = random:uniform(Size),
    erlang:put(?MODULE, Pos),
    Pos;
pick_pos(OldPos, Size) ->
    case random:uniform(Size) of
        OldPos ->
            pick_pos(OldPos, Size);
        Pos ->
            erlang:put(?MODULE, Pos),
            Pos
    end.

reload_children('$end_of_table', _Tid) ->
    ok;
reload_children(Current, Tid) ->
    riak_js_vm:reload(Current),
    reload_children(ets:next(Tid, Current), Tid).
