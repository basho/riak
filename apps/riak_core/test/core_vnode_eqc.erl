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
               counters}).% Dict of counters for each index
               
simple_test() ->
    ?assertEqual(true, eqc:quickcheck(prop_simple())).

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE, {stopped, initial_state_data()}),
            aggregate(command_names(Cmds),
                      begin
                          Pids = start_servers(),
                          %io:format("Running Cmds: ~p\n", [Cmds]),
                          {H,S,Res} = run_commands(?MODULE, Cmds),
                          [stop_pid(Pid) || Pid <- Pids],                     
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

active_preflist1(S) ->
    {active_index(S), node()}.

initial_state() ->
    stopped.

index() ->
    oneof([0,
           182687704666362864775460604089535377456991567872,
           365375409332725729550921208179070754913983135744,
           548063113999088594326381812268606132370974703616,
           730750818665451459101842416358141509827966271488,
           913438523331814323877303020447676887284957839360,
           1096126227998177188652763624537212264741949407232,
           1278813932664540053428224228626747642198940975104]).
    

initial_state_data() ->
    #qcst{started=[],
          counters=orddict:new()}.

next_state_data(_From,_To,S=#qcst{started=Started,
                                  counters=Counters},_R,
                {call,?MODULE,start_vnode,[Index]}) ->
    S#qcst{started=[Index|Started],
           counters=orddict:store(Index, 0, Counters)};
next_state_data(_From,_To,S,_R,_C) ->
    S.
%           counters=orddict:update_counter(Index, 1, Counters)};

stopped(_S) ->
    [{running, {call,?MODULE,start_vnode,[index()]}}].

running(S) ->
    %% StartVnode = case ordsets:is_empty(S#state.stopped) of
    %%                  true ->
    %%                      [];
    %%                  false ->
    %%                      [{running, {call,mock_vnode,start_vnode,[stopped_index(S)]}}]
    %%              end,
    %% StartVnode ++ 
    [
     {history, {call,?MODULE,start_vnode,[index()]}},
     {history, {call,mock_vnode,get_index,[active_preflist1(S)]}},
     {history, {call,mock_vnode,get_counter,[active_preflist1(S)]}}
     %% {history, {call,?MODULE,noop,[2]}},
     %%{history, {call,?MODULE,noop,[3]}}].
    ].

precondition(_From,_To,#qcst{started=Started},{call,?MODULE,start_vnode,[Index]}) ->
    not lists:member(Index, Started);
precondition(_From,_To,_S,_C) ->
    true.

postcondition(_From,_To,_S,
              {call,mock_vnode,get_index,[{Index,_Node}]},{ok,ReplyIndex}) ->
    Index =:= ReplyIndex;
postcondition(_From,_To,#qcst{counters=Counters},
              {call,mock_vnode,get_counter,[{Index,_Node}]},{ok,ReplyCount}) ->
    Expect = orddict:fetch(Index, Counters),
%    io:format("Expect=~p ReplyCount=~p\n", [Expect, ReplyCount]),
    Expect =:= ReplyCount;
postcondition(_From,_To,_S,_C,_R) ->
    true.


start_vnode(I) ->
    ok = mock_vnode:start_vnode(I).

start_servers() ->
    %catch riak_core_vnode_master:stop(mock_vnode),
    stop_pid(whereis(mock_vnode_master)),
    stop_pid(whereis(riak_core_vnode_sup)),
    {ok, Sup} = supervisor:start_link({local, riak_core_vnode_sup}, riak_core_vnode_sup, []),
    {ok, VMaster} = riak_core_vnode_master:start_link(mock_vnode),
    %% Make sure VMaster is killed before sup as start_vnode is a cast
    %% and there may be a pending request to start the vnode.
    [VMaster, Sup].

stop_pid(undefined) ->
    ok;
stop_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
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


noop(_Arg) ->
    ok.

-endif.
