%% -------------------------------------------------------------------
%%
%% core_vnode_eqc: QuickCheck tests for riak_core_vnode code
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

%% @doc  QuickCheck tests for riak_core_vnode code

%% Things to test...
%% riak_core_vnode_master:command gets delivered to the right node
%% riak_core_vnode_master:sync_command works

-module(core_vnode_eqc).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-compile([export_all]).

-record(qcst, {started,
               counters, % Dict of counters for each index
               indices}).
               
simple_test() ->
    simple_test(100).

simple_test(N) ->
    ?assertEqual(true, quickcheck(numtests(N, prop_simple()))).

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE, {stopped, initial_state_data()}),
            aggregate(command_names(Cmds),
                      begin
                          start_servers(),
                          {H,S,Res} = run_commands(?MODULE, Cmds),
                          stop_servers(),            
                          ?WHENFAIL(
                             begin
                                 io:format("History: ~p\n", [H]),
                                 io:format("State: ~p\n", [S]),
                                 io:format("Result: ~p\n", [Res])
                             end,
                             Res =:= ok)
                      end)).

active_index(#qcst{started=Started}) ->
    elements(Started).

%% Generate a preflist element
active_preflist1(S) ->
    {active_index(S), node()}.

%% Generate a preflist - making sure the partitions are unique
active_preflist(S) ->
    ?SUCHTHAT(Xs,list(active_preflist1(S)),lists:sort(Xs)==lists:usort(Xs)).

initial_state() ->
    stopped.

index(S) ->
    oneof(S#qcst.indices).

initial_state_data() ->
    Ring = riak_core_ring:fresh(8, node()),
    riak_core_ring_manager:set_ring_global(Ring),
    #qcst{started=[],
          counters=orddict:new(),
          indices=[I || {I,_N} <- riak_core_ring:all_owners(Ring)]
         }.

%% Mark the vnode as started
next_state_data(_From,_To,S=#qcst{started=Started,
                                  counters=Counters},_R,
                {call,?MODULE,start_vnode,[Index]}) ->
    S#qcst{started=[Index|Started],
           counters=orddict:store(Index, 0, Counters)};
next_state_data(_From,_To,S=#qcst{counters=Counters},_R,
                {call,mock_vnode,stop,[{Index,_Node}]}) ->
    %% If a node is stopped, reset the counter ready for next
    %% time it is called which should start it
    S#qcst{counters=orddict:store(Index, 0, Counters)};
%% Update the counters for the index if a command that changes them
next_state_data(_From,_To,S=#qcst{counters=Counters},_R,
                {call,_Mod,Func,[Preflist]})
  when Func =:= neverreply; Func =:= returnreply; Func =:= latereply ->
    S#qcst{counters=lists:foldl(fun({I, _N}, C) ->
                                        orddict:update_counter(I, 1, C)
                                end, Counters, Preflist)};
next_state_data(_From,_To,S,_R,_C) ->
    S.
% 

stopped(S) ->
    [{running, {call,?MODULE,start_vnode,[index(S)]}}].

running(S) ->
    [
     {history, {call,?MODULE,start_vnode,[index(S)]}},
     {history, {call,mock_vnode,get_index,[active_preflist1(S)]}},
     {history, {call,mock_vnode,get_counter,[active_preflist1(S)]}},
     {history, {call,mock_vnode,neverreply,[active_preflist(S)]}},
     {history, {call,?MODULE,returnreply,[active_preflist(S)]}},
     {history, {call,?MODULE,latereply,[active_preflist(S)]}},
     {history, {call,?MODULE,restart_master,[]}},
     {history, {call,mock_vnode,stop,[active_preflist1(S)]}},
     {history, {call,riak_core_vnode_master,all_nodes,[mock_vnode]}}
    ].

precondition(_From,_To,#qcst{started=Started},{call,?MODULE,start_vnode,[Index]}) ->
    not lists:member(Index, Started);
precondition(_From,_To,#qcst{started=Started},{call,_Mod,Func,[Preflist]}) 
  when Func =:= get_index; Func =:= get_counter; Func =:= neverreply; Func =:= returnreply;
       Func =:= latereply ->
    preflist_is_active(Preflist, Started);
precondition(_From,_To,_S,_C) ->
    true.

postcondition(_From,_To,_S,
              {call,mock_vnode,get_index,[{Index,_Node}]},{ok,ReplyIndex}) ->
    Index =:= ReplyIndex;
postcondition(_From,_To,#qcst{counters=Counters},
              {call,mock_vnode,get_counter,[{Index,_Node}]},{ok,ReplyCount}) ->
    orddict:fetch(Index, Counters) =:= ReplyCount;
postcondition(_From,_To,_S,
              {call,_Mod,Func,[]},Result)
  when Func =:= neverreply; Func =:= returnreply; Func =:= latereply ->
    Result =:= ok;
postcondition(_From,_To,_S,
              {call,riak_core_vnode_master,all_nodes,[mock_vnode]},Result) ->
    Pids = [Pid || {_,Pid,_,_} <- supervisor:which_children(riak_core_vnode_sup)],
    lists:sort(Result) =:= lists:sort(Pids);
postcondition(_From,_To,_S,_C,_R) ->
    true.

%% Pre/post condition helpers

preflist_is_active({Index,_Node}, Started) ->
    lists:member(Index, Started);
preflist_is_active(Preflist, Started) ->
    lists:all(fun({Index,_Node}) -> lists:member(Index, Started) end, Preflist).


%% Local versions of commands
start_vnode(I) ->
    ok = mock_vnode:start_vnode(I).

returnreply(Preflist) ->
    {ok, Ref} = mock_vnode:returnreply(Preflist),
    check_receive(length(Preflist), returnreply, Ref).

latereply(Preflist) ->
    {ok, Ref} = mock_vnode:latereply(Preflist),
    check_receive(length(Preflist), latereply, Ref).

                 
check_receive(0, _Msg, _Ref) ->
    ok;
check_receive(Replies, Msg, Ref) ->
    receive
        {Ref, Msg} ->
            check_receive(Replies-1, Msg, Ref);
        {Ref, OtherMsg} ->
            {error, {bad_msg, Msg, OtherMsg}}
    after
        1000 ->
            {error, timeout}
    end.

%% Server start/stop infrastructure

start_servers() ->
    stop_servers(),
    {ok, _Sup} = riak_core_vnode_sup:start_link(),
    {ok, _VMaster} = riak_core_vnode_master:start_link(mock_vnode).

stop_servers() ->
    %% Make sure VMaster is killed before sup as start_vnode is a cast
    %% and there may be a pending request to start the vnode.
    stop_pid(whereis(mock_vnode_master)),
    stop_pid(whereis(riak_core_vnode_sup)).

restart_master() ->
    %% Call get status to make sure the riak_core_vnode_master
    %% has processed any commands that were cast to it.  Otherwise
    %% commands like neverreply are not cast on to the vnode and the
    %% counters are not updated correctly.
    sys:get_status(mock_vnode_master),
    stop_pid(whereis(mock_vnode_master)),
    {ok, _VMaster} = riak_core_vnode_master:start_link(mock_vnode).

stop_pid(undefined) ->
    ok;
stop_pid(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ok = wait_for_pid(Pid).

wait_for_pid(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN',Mref,process,_,_} ->
            ok
    after
        5000 ->
            {error, didnotexit}
    end.

-endif.
