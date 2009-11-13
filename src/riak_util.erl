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

%% @doc Various functions that are useful throughout Riak.
-module(riak_util).
-include_lib("eunit/include/eunit.hrl").

-export([moment/0,make_tmp_dir/0,compare_dates/2,reload_all/1,
         is_x_deleted/1,obj_not_deleted/1,integer_to_list/2,
         unique_id_62/0]).
-export([chash_key/1,chash_std_keyfun/1,chash_bucketonly_keyfun/1]).
-export([try_cast/4, fallback/4, mkclientid/1]).

%% @spec moment() -> integer()
%% @doc Get the current "moment".  Current implementation is the
%%      number of seconds from year 0 to now, universal time, in
%%      the gregorian calendar.
moment() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% @spec compare_dates(string(), string()) -> boolean()
%% @doc Compare two RFC1123 date strings or two now() tuples (or one
%%      of each).  Return true if date A is later than date B.
compare_dates(A={_,_,_}, B={_,_,_}) ->
    %% assume 3-tuples are now() times
    A > B;
compare_dates(A, B) when is_list(A) ->
    %% assume lists are rfc1123 date strings
    compare_dates(rfc1123_to_now(A), B);
compare_dates(A, B) when is_list(B) ->
    compare_dates(A, rfc1123_to_now(B)).

%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).

rfc1123_to_now(String) when is_list(String) ->
    GSec = calendar:datetime_to_gregorian_seconds(
             httpd_util:convert_request_date(String)),
    ESec = GSec-?SEC_TO_EPOCH,
    Sec = ESec rem 1000000,
    MSec = ESec div 1000000,
    {MSec, Sec, 0}.

%% @spec make_tmp_dir() -> string()
%% @doc Create a unique directory in /tmp.  Returns the path
%%      to the new directory.
make_tmp_dir() ->
    TmpId = io_lib:format("riptemp.~p",
                          [erlang:phash2({random:uniform(),self()})]),
    TempDir = filename:join("/tmp", TmpId),
    case filelib:is_dir(TempDir) of
        true -> make_tmp_dir();
        false ->
            ok = file:make_dir(TempDir),
            TempDir
    end.

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

%% @spec integer_to_list(Integer :: integer(), Base :: integer()) ->
%%          string()
%% @doc Convert an integer to its string representation in the given
%%      base.  Bases 2-62 are supported.
integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base) 
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|integer_to_list(-I, Base, [])];
       true ->
            integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

%% @spec integer_to_list(integer(), integer(), string()) -> string()
integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
		 [D-36+$a|R0];
	    D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

%% @spec unique_id_62() -> string()
%% @doc Create a random identifying integer, returning its string
%%      representation in base 62.
unique_id_62() ->
    Rand = crypto:sha(term_to_binary({make_ref(), now()})),
    <<I:160/integer>> = Rand,
    integer_to_list(I, 62).

%% @spec reload_all(Module :: atom()) ->
%%         [{purge_response(), load_file_response()}]
%% @type purge_response() = boolean()
%% @type load_file_response() = {module, Module :: atom()}|
%%                              {error, term()}
%% @doc Ask each member node of the riak ring to reload the given
%%      Module.  Return is a list of the results of code:purge/1
%%      and code:load_file/1 on each node.
reload_all(Module) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    [{rpc:call(Node, code, purge, [Module]), 
     rpc:call(Node, code, load_file, [Module])} || 
        Node <- riak_ring:all_members(Ring)].

%% @spec try_cast(term(), term(), [{Index :: term(), Node :: node()}]) ->
%%          {[{Index :: term(), Node :: node(), Node :: node()}],
%%           [{Index :: term(), Node :: node()}]}
%% @doc Cast {Cmd, {Index,Node}, Msg} at riak_vnode_master on Node
%%      if Node responds 'pong' to a net_adm:ping.  The list of
%%      successful casts is the first element of the return tuple, and
%%      the list of pang-responding nodes is the second element.
%%      Used in riak_put_fsm and riak_get_fsm.
try_cast(Cmd, Msg, UpNodes, Targets) ->
    try_cast(Cmd, Msg, UpNodes, Targets, [], []).
try_cast(_Cmd, _Msg, _UpNodes, [], Sent, Pangs) -> {Sent, Pangs};
try_cast(Cmd, Msg, UpNodes, [{Index,Node}|Targets], Sent, Pangs) ->
    case lists:member(Node, [node()|UpNodes])
          orelse net_adm:ping(Node) == pong of
        false ->
            try_cast(Cmd, Msg, UpNodes, Targets, Sent, [{Index,Node}|Pangs]);
        true ->
            gen_server:cast({riak_vnode_master, Node},
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
%%      Used in riak_put_fsm and riak_get_fsm
fallback(Cmd, Msg, Pangs, Fallbacks) ->
    fallback(Cmd, Msg, Pangs, Fallbacks, []).
fallback(_Cmd, _Msg, [], _Fallbacks, Sent) -> Sent;
fallback(_Cmd, _Msg, _Pangs, [], Sent) -> Sent;
fallback(Cmd, Msg, [{Index,Node}|Pangs], [{_,FN}|Fallbacks], Sent) ->
    case lists:member(FN, [node()|nodes()]) orelse net_adm:ping(FN) == pong of
        false -> fallback(Cmd, Msg, [{Index,Node}|Pangs], Fallbacks, Sent);
        true ->
            gen_server:cast({riak_vnode_master, FN},
                            {Cmd, {Index,Node}, Msg}),
            fallback(Cmd, Msg, Pangs, Fallbacks, [{Index,Node,FN}|Sent])
    end.

%% @spec mkclientid(RemoteNode :: term()) -> ClientID :: list()
%% @doc Create a unique-enough id for vclock clients.
mkclientid(RemoteNode) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    Id = erlang:phash2([Y,Mo,D,H,Mi,S,node(),RemoteNode,NowPart]),
    <<Id:32>>.

%% @spec chash_key(BKey :: riak_object:bkey()) -> chash:index()
%% @doc Create a binary used for determining replica placement.
chash_key({Bucket,Key}) ->
    BucketProps = riak_bucket:get_bucket(Bucket),
    {M, F} = case proplists:lookup(chash_keyfun, BucketProps) of
                 {chash_keyfun, MF} -> MF;
                 none ->
                     %% 0.5 didn't have chash_keyfun - upgrade
                     {chash_keyfun, Default} =
                         proplists:lookup(
                           chash_keyfun, riak_bucket:defaults()),
                     riak_bucket:set_bucket(
                       Bucket, [{chash_keyfun,Default}]),
                     Default
             end,
    M:F({Bucket,Key}).

%% @spec chash_std_keyfun(BKey :: riak_object:bkey()) -> chash:index()
%% @doc Default object/ring hashing fun, direct passthrough of bkey.
chash_std_keyfun({Bucket, Key}) -> chash:key_of({Bucket, Key}).
    
%% @spec chash_bucketonly_keyfun(BKey :: riak_object:bkey()) -> chash:index()
%% @doc Object/ring hashing fun that ignores Key, only uses Bucket.
chash_bucketonly_keyfun({Bucket, _Key}) -> chash:key_of(Bucket).

%% @spec moment_test() -> boolean()
moment_test() ->
    M1 = riak_util:moment(),
    M2 = riak_util:moment(),
    ?assert(M2 >= M1).

deleted_test() ->
    O = riak_object:new(<<"test">>, <<"k">>, "v"),
    false = is_x_deleted(O),
    MD = dict:new(),
    O1 = riak_object:apply_updates(
           riak_object:update_metadata(
             O, dict:store(<<"X-Riak-Deleted">>, true, MD))),
    true = is_x_deleted(O1).

clientid_uniqueness_test() ->
    ClientIds = [mkclientid('somenode@somehost') || _I <- lists:seq(0, 10000)],
    length(ClientIds) =:= length(sets:to_list(sets:from_list(ClientIds))).
