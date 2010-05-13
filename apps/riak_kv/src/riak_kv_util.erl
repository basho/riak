%% -------------------------------------------------------------------
%%
%% riak_util: functions that are useful throughout Riak
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


%% @doc Various functions that are useful throughout riak_kv.
-module(riak_kv_util).


-export([is_x_deleted/1,
         obj_not_deleted/1,
         try_cast/4,
         fallback/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Public API
%% ===================================================================

%% @spec is_x_deleted(riak_object:riak_object()) -> boolean()
%% @doc 'true' if all contents of the input object are marked
%%      as deleted; 'false' otherwise
%% @equiv obj_not_deleted(Obj) == undefined
is_x_deleted(Obj) ->
    case obj_not_deleted(Obj) of
        undefined -> true;
        _ -> false
    end.

%% @spec obj_not_deleted(riak_object:riak_object()) ->
%%          undefined|riak_object:riak_object()
%% @doc Determine whether all contents of an object are marked as
%%      deleted.  Return is the atom 'undefined' if all contents
%%      are marked deleted, or the input Obj if any of them are not.
obj_not_deleted(Obj) ->
    case [{M, V} || {M, V} <- riak_object:get_contents(Obj),
                    dict:is_key(<<"X-Riak-Deleted">>, M) =:= false] of
	[] -> undefined;
	_ -> Obj
    end.

%% @spec try_cast(term(), term(), [node()], [{Index :: term(), Node :: node()}]) ->
%%          {[{Index :: term(), Node :: node(), Node :: node()}],
%%           [{Index :: term(), Node :: node()}]}
%% @doc Cast {Cmd, {Index,Node}, Msg} at riak_kv_vnode_master on Node
%%      if Node is in UpNodes.  The list of successful casts is the
%%      first element of the return tuple, and the list of unavailable
%%      nodes is the second element.  Used in riak_kv_put_fsm and riak_kv_get_fsm.
try_cast(Cmd, Msg, UpNodes, Targets) ->
    try_cast(Cmd, Msg, UpNodes, Targets, [], []).
try_cast(_Cmd, _Msg, _UpNodes, [], Sent, Pangs) -> {Sent, Pangs};
try_cast(Cmd, Msg, UpNodes, [{Index,Node}|Targets], Sent, Pangs) ->
    case lists:member(Node, [node()|UpNodes]) of
        false ->
            try_cast(Cmd, Msg, UpNodes, Targets, Sent, [{Index,Node}|Pangs]);
        true ->
            gen_server:cast({riak_kv_vnode_master, Node},
                            {Cmd, {Index,Node}, Msg}),
            try_cast(Cmd, Msg, UpNodes, Targets, [{Index,Node,Node}|Sent],Pangs)
    end.

%% @spec fallback(term(), term(), [{Index :: term(), Node :: node()}],
%%                [{any(), Fallback :: node()}]) ->
%%         [{Index :: term(), Node :: node(), Fallback :: node()}]
%% @doc Cast {Cmd, {Index,Node}, Msg} at a node in the Fallbacks list
%%      for each node in the Pangs list.  Pangs should have come
%%      from the second element of the response tuple of a call to
%%      try_cast/3.
%%      Used in riak_kv_put_fsm and riak_kv_get_fsm
fallback(Cmd, Msg, Pangs, Fallbacks) ->
    fallback(Cmd, Msg, Pangs, Fallbacks, []).
fallback(_Cmd, _Msg, [], _Fallbacks, Sent) -> Sent;
fallback(_Cmd, _Msg, _Pangs, [], Sent) -> Sent;
fallback(Cmd, Msg, [{Index,Node}|Pangs], [{_,FN}|Fallbacks], Sent) ->
    case lists:member(FN, [node()|nodes()]) of
        false -> fallback(Cmd, Msg, [{Index,Node}|Pangs], Fallbacks, Sent);
        true ->
            gen_server:cast({riak_kv_vnode_master, FN},
                            {Cmd, {Index,Node}, Msg}),
            fallback(Cmd, Msg, Pangs, Fallbacks, [{Index,Node,FN}|Sent])
    end.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

deleted_test() ->
    O = riak_object:new(<<"test">>, <<"k">>, "v"),
    false = is_x_deleted(O),
    MD = dict:new(),
    O1 = riak_object:apply_updates(
           riak_object:update_metadata(
             O, dict:store(<<"X-Riak-Deleted">>, true, MD))),
    true = is_x_deleted(O1).

-endif.
