%% -------------------------------------------------------------------
%%
%% riak_vnode_master: dispatch to vnodes
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

%% @doc dispatch to vnodes

-module(riak_core_vnode_master).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-behaviour(gen_server).
-export([start_link/1, start_link/2, get_vnode_pid/2,
         start_vnode/2, command/3, command/4, sync_command/3,
         sync_command/4,
         sync_spawn_command/3, make_request/3,
         all_nodes/1, reg_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-record(idxrec, {idx, pid, monref}).
-record(state, {idxtab, excl=ordsets:new(), sup_name, vnode_mod, legacy}).

-define(DEFAULT_TIMEOUT, 5000).

make_name(VNodeMod,Suffix) -> list_to_atom(atom_to_list(VNodeMod)++Suffix).
reg_name(VNodeMod) ->  make_name(VNodeMod, "_master").

start_link(VNodeMod) -> 
    start_link(VNodeMod, undefined).

start_link(VNodeMod, LegacyMod) -> 
    RegName = reg_name(VNodeMod),
    gen_server:start_link({local, RegName}, ?MODULE, 
                          [VNodeMod,LegacyMod,RegName], []).

start_vnode(Index, VNodeMod) ->
    RegName = reg_name(VNodeMod),
    gen_server:cast(RegName, {Index, start_vnode}).

get_vnode_pid(Index, VNodeMod) ->
    RegName = reg_name(VNodeMod),
    gen_server:call(RegName, {Index, get_vnode}).
    
command(Preflist, Msg, VMaster) ->
    command(Preflist, Msg, noreply, VMaster).
     
%% Send the command to the preflist given with responses going to Sender
command([], _Msg, _Sender, _VMaster) ->
    ok;
command([{Index,Node}|Rest], Msg, Sender, VMaster) ->
    gen_server:cast({VMaster, Node}, make_request(Msg, Sender, Index)),
    command(Rest, Msg, Sender, VMaster);

%% Send the command to an individual Index/Node combination
command({Index,Node}, Msg, Sender, VMaster) ->
    gen_server:cast({VMaster, Node}, make_request(Msg, Sender, Index)).

%% Send a synchronous command to an individual Index/Node combination.
%% Will not return until the vnode has returned
sync_command(IndexNode, Msg, VMaster) ->
    sync_command(IndexNode, Msg, VMaster, ?DEFAULT_TIMEOUT).

sync_command({Index,Node}, Msg, VMaster, Timeout) ->
    %% Issue the call to the master, it will update the Sender with
    %% the From for handle_call so that the {reply} return gets 
    %% sent here.
    gen_server:call({VMaster, Node}, 
                    make_request(Msg, {server, undefined, undefined}, Index), Timeout).


%% Send a synchronous spawned command to an individual Index/Node combination.
%% Will not return until the vnode has returned, but the vnode_master will
%% continue to handle requests.
sync_spawn_command({Index,Node}, Msg, VMaster) ->
    gen_server:call({VMaster, Node}, 
                    {spawn, make_request(Msg, {server, undefined, undefined}, Index)},
                    infinity).

    
%% Make a request record - exported for use by legacy modules
-spec make_request(vnode_req(), sender(), partition()) -> #riak_vnode_req_v1{}.
make_request(Request, Sender, Index) ->
    #riak_vnode_req_v1{
              index=Index,
              sender=Sender,
              request=Request}.

%% Request a list of Pids for all vnodes 
all_nodes(VNodeMod) ->
    RegName = reg_name(VNodeMod),
    gen_server:call(RegName, all_nodes).

%% @private
init([VNodeMod, LegacyMod, RegName]) ->
    %% Get the current list of vnodes running in the supervisor. We use this
    %% to rebuild our ETS table for routing messages to the appropriate
    %% vnode.
    VnodePids = [Pid || {_, Pid, worker, _}
                            <- supervisor:which_children(riak_core_vnode_sup)],
    IdxTable = ets:new(RegName, [{keypos, 2}]),

    %% In case this the vnode master is being restarted, scan the existing
    %% vnode children and work out which module and index they are responsible
    %% for.  During startup it is possible that these vnodes may be shutting
    %% down as we check them if there are several types of vnodes active.
    PidIdxs = lists:flatten(
                [try 
                     [{Pid, riak_core_vnode:get_mod_index(Pid)}] 
                 catch
                     _:_Err ->
                         []
                 end || Pid <- VnodePids]),

    %% Populate the ETS table with processes running this VNodeMod (filtered
    %% in the list comprehension)
    F = fun(Pid, Idx) ->
                Mref = erlang:monitor(process, Pid),
                #idxrec { idx = Idx, pid = Pid, monref = Mref }
        end,
    IdxRecs = [F(Pid, Idx) || {Pid, {Mod, Idx}} <- PidIdxs, Mod =:= VNodeMod],
    true = ets:insert_new(IdxTable, IdxRecs),
    {ok, #state{idxtab=IdxTable,
                vnode_mod=VNodeMod,
                legacy=LegacyMod}}.

handle_cast({Partition, start_vnode}, State=#state{excl=Excl}) ->
    get_vnode(Partition, State),
    {noreply, State#state{excl=ordsets:del_element(Partition, Excl)}};    
handle_cast(Req=?VNODE_REQ{index=Idx}, State) ->
    Pid = get_vnode(Idx, State),
    gen_fsm:send_event(Pid, Req),
    {noreply, State};
handle_cast(Other, State=#state{legacy=Legacy}) when Legacy =/= undefined ->
    case catch Legacy:rewrite_cast(Other) of
        {ok, ?VNODE_REQ{}=Req} ->
            handle_cast(Req, State);
        _ ->
            {noreply, State}
    end.

handle_call(Req=?VNODE_REQ{index=Idx, sender={server, undefined, undefined}}, From, State) ->
    Pid = get_vnode(Idx, State),
    gen_fsm:send_event(Pid, Req?VNODE_REQ{sender={server, undefined, From}}),
    {noreply, State};
handle_call({spawn, 
             Req=?VNODE_REQ{index=Idx, sender={server, undefined, undefined}}}, From, State) ->
    Pid = get_vnode(Idx, State),
    Sender = {server, undefined, From},
    spawn(
      fun() -> gen_fsm:send_all_state_event(Pid, Req?VNODE_REQ{sender=Sender}) end),
    {noreply, State};
handle_call(all_nodes, _From, State) ->
    {reply, lists:flatten(ets:match(State#state.idxtab, {idxrec, '_', '$1', '_'})), State};
handle_call({Partition, get_vnode}, _From, State) ->
    Pid = get_vnode(Partition, State),
    {reply, {ok, Pid}, State};
handle_call(Other, From, State=#state{legacy=Legacy}) when Legacy =/= undefined ->
    case catch Legacy:rewrite_call(Other, From) of
        {ok, ?VNODE_REQ{}=Req} ->
            handle_call(Req, From, State);
        _ ->
            {noreply, State}
    end.

handle_info({'DOWN', MonRef, process, _P, _I}, State) ->
    delmon(MonRef, State),
    {noreply, State}.

%% @private
terminate(_Reason, _State) -> 
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% @private
idx2vnode(Idx, _State=#state{idxtab=T}) ->
    case ets:match(T, {idxrec, Idx, '$1', '_'}) of
        [[VNodePid]] -> VNodePid;
        [] -> no_match
    end.

%% @private
delmon(MonRef, _State=#state{idxtab=T}) ->
    ets:match_delete(T, {idxrec, '_', '_', MonRef}).

%% @private
add_vnode_rec(I,  _State=#state{idxtab=T}) -> ets:insert(T,I).

%% @private
get_vnode(Idx, State=#state{vnode_mod=Mod}) ->
    case idx2vnode(Idx, State) of
        no_match ->
            {ok, Pid} = riak_core_vnode_sup:start_vnode(Mod, Idx),
            MonRef = erlang:monitor(process, Pid),
            add_vnode_rec(#idxrec{idx=Idx,pid=Pid,monref=MonRef}, State),
            Pid;
        X -> X
    end.
