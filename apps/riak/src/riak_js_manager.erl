%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(riak_js_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, dispatch/1, blocking_dispatch/1, add_to_manager/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {children=[]}).

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

add_to_manager() ->
    gen_server:cast(?MODULE, {add_child, self()}).

start_link(ChildCount) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ChildCount], []).

init([ChildCount]) ->
    start_children(ChildCount),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({add_child, ChildPid}, #state{children=Children}=State) ->
    erlang:monitor(process, ChildPid),
    {noreply, State#state{children=Children ++ [ChildPid]}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, Pid, _Info}, #state{children=Children}=State) ->
    case lists:member(Pid, Children) of
        true ->
            riak_js_sup:start_js(self()),
            {noreply, State#state{children=lists:delete(Pid, Children)}};
        false ->
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
    case pg2:get_members({node(), js_vm}) of
        [] ->
            no_vms;
        {error, _} ->
            novms;
        Members ->
            {T1, T2, T3} = erlang:now(),
            random:seed(T1, T2, T3),
            Pos = random:uniform(length(Members)),
            lists:nth(Pos, Members)
    end.
