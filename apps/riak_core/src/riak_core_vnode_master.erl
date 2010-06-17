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
-export([start_link/1, command/3, command/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-record(idxrec, {idx, pid, monref}).
-record(state, {idxtab, excl=ordsets:new(), sup_name, vnode_mod}).

make_name(VNodeMod,Suffix) -> list_to_atom(atom_to_list(VNodeMod)++Suffix).
reg_name(VNodeMod) ->  make_name(VNodeMod, "_master").

start_link(VNodeMod) -> 
    RegName = reg_name(VNodeMod),
    gen_server:start_link({local, RegName}, ?MODULE, [VNodeMod,RegName], []).

command(Preflist, Msg, VMaster) ->
    command(Preflist, Msg, noreply, VMaster).
     
%% Send the command to the preflist given with responses going to Sender
command([], _Msg, _Sender, _VMaster) ->
    ok;
command([{Index,Node}|Rest], Msg, Sender, VMaster) ->
    gen_server:cast({VMaster, Node}, make_request(Msg, Sender, Index)),
    command(Rest, Msg, Sender, VMaster).

%% @private
init([VNodeMod, RegName]) ->
    %% Get the current list of vnodes running in the supervisor. We use this
    %% to rebuild our ETS table for routing messages to the appropriate
    %% vnode.
    VnodePids = [Pid || {_, Pid, worker, _}
                            <- supervisor:which_children(riak_core_vnode_sup)],
    IdxTable = ets:new(RegName, [{keypos, 2}]),
    PidIdxs = [{Pid, riak_core_vnode:get_mod_index(Pid)} || Pid <- VnodePids],

    %% Populate the ETS table with processes running this VNodeMod (filtered
    %% in the list comprehension)
    F = fun(Pid, Idx) ->
                Mref = erlang:monitor(process, Pid),
                #idxrec { idx = Idx, pid = Pid, monref = Mref }
        end,
    IdxRecs = [F(Pid, Idx) || {Pid, {Mod, Idx}} <- PidIdxs, Mod =:= VNodeMod],
    true = ets:insert_new(IdxTable, IdxRecs),
    {ok, #state{idxtab=IdxTable,
                vnode_mod=VNodeMod}}.

handle_cast({Partition, start_vnode}, State=#state{excl=Excl}) ->
    get_vnode(Partition, State),
    {noreply, State#state{excl=ordsets:del_element(Partition, Excl)}};    
handle_cast(Req=?VNODE_REQ{index=Idx}, State) ->
    Pid = get_vnode(Idx, State),
    gen_fsm:send_event(Pid, Req),
    {noreply, State};
handle_cast({Partition, Msg}, State) ->
    Pid = get_vnode(Partition, State),
    gen_fsm:send_event(Pid, Msg),
    {noreply, State}.

handle_call({Partition, Msg}, From, State) ->
    Pid = get_vnode(Partition, State),
    gen_fsm:send_event(Pid, {From, Msg}),
    {noreply, State}.

%    
%handle_cast({vnode_map, {Partition,_Node},
%             {ClientPid,QTerm,BKey,KeyData}}, State) ->
%    Pid = get_vnode(Partition, State),
%    gen_fsm:send_event(Pid, {map, ClientPid, QTerm, BKey, KeyData}),
%    {noreply, State};
%handle_cast({vnode_merkle, {RemoteVN,Partition,Merkle,ObjList}}, State) ->
%    Pid = get_vnode(Partition, State),
%    gen_fsm:send_event(Pid, {vnode_merkle, {RemoteVN,Merkle,ObjList}}),
%    {noreply, State};
%handle_cast({add_exclusion, Partition}, State=#state{excl=Excl}) ->
%    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
%    riak_core_ring_events:ring_update(Ring),
%    {noreply, State#state{excl=ordsets:add_element(Partition, Excl)}}.
%handle_call(all_possible_vnodes, _From, State) ->
%    {reply, make_all_active(State), State};
%handle_call(all_vnodes, _From, State) ->
%    {reply, all_vnodes(State), State};
%handle_call({vnode_del, {Partition,_Node},
%             {BKey,ReqID}}, From, State) ->
%    Pid = get_vnode(Partition, State),
%   gen_fsm:send_event(Pid, {delete, From, BKey, ReqID}),
%    {noreply, State};
%handle_call({get_merkle, Partition}, From, State) ->
%    Pid = get_vnode(Partition, State),
%    spawn(fun() -> gen_fsm:send_all_state_event(Pid, {get_merkle, From}) end),
%    {noreply, State};
%handle_call({get_vclocks,Partition,KeyList},From,State) ->
%    Pid = get_vnode(Partition, State),
%    spawn(fun() -> gen_fsm:send_all_state_event(
%                     Pid,{get_vclocks,From,KeyList}) end),
%    {noreply, State};
%handle_call({fold, {Partition, Fun, Acc0}}, From, State) ->
%    Pid = get_vnode(Partition, State),
%    spawn(
%      fun() -> gen_fsm:send_all_state_event(Pid, {fold, {Fun,Acc0,From}}) end),
%    {noreply, State};
%handle_call({get_vnode, Partition}, _From, State) ->
%    {reply, {ok, get_vnode(Partition, State)}, State};
%handle_call(get_exclusions, _From, State=#state{excl=Excl}) ->
%    {reply, {ok, ordsets:to_list(Excl)}, State}.
%%% @private
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

%% @private
%all_vnodes(_State=#state{idxtab=T}) ->
%    lists:flatten(ets:match(T, {idxrec, '_', '$1', '_'})).

%make_all_active(State) ->
%    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
%    [{I,get_vnode(I,State)} || I <- riak_core_ring:my_indices(Ring)].

-spec make_request(vnode_req(), sender(), partition()) -> #riak_vnode_req_v1{}.
make_request(Request, Sender, Index) ->
    #riak_vnode_req_v1{
              index=Index,
              sender=Sender,
              request=Request}.
