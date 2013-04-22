%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010-2012 Basho Technologies, Inc.
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

%%
%% Pre/post commit hooks for testing
%%
-module(hooks).
-compile([export_all]).

precommit_nop(Obj) ->
    Obj.

precommit_fail(_Obj) ->
    fail.

precommit_failatom(_Obj) ->
    {fail, on_purpose}.

precommit_failstr(_Obj) ->
    {fail, "on purpose"}.

precommit_failbin(_Obj) ->
    {fail, <<"on purpose">>}.

precommit_failkey(Obj) ->
    case riak_object:key(Obj) of
        <<"fail">> ->
            fail;
        _ ->
            Obj
    end.


set_precommit(Bucket, Hook) when is_atom(Hook) ->
    set_precommit(Bucket, atom_to_binary(Hook, latin1));
set_precommit(Bucket, Hook) when is_list(Hook) ->
    set_precommit(Bucket, list_to_binary(Hook));
set_precommit(Bucket, Hook) ->
    {ok,C} = riak:local_client(),
    C:set_bucket(Bucket,
                 [{precommit, [{struct,[{<<"mod">>,<<"hooks">>},
                                        {<<"fun">>,Hook}]}]}]).
set_hooks() ->
    set_precommit(),
    set_postcommit().

set_precommit() ->
    hooks:set_precommit(<<"failatom">>,precommit_failatom),
    hooks:set_precommit(<<"failstr">>,precommit_failstr),
    hooks:set_precommit(<<"failbin">>,precommit_failbin),
    hooks:set_precommit(<<"failkey">>,precommit_failkey).

set_postcommit() ->
    {ok, C} = riak:local_client(),
    C:set_bucket(<<"postcommit">>,[{postcommit, [{struct,[{<<"mod">>,<<"hooks">>},{<<"fun">>, <<"postcommit_msg">>}]}]}]).

postcommit_msg(Obj) ->
    Bucket = riak_object:bucket(Obj),
    Key = riak_object:key(Obj),
    case application:get_env(riak_test, test_pid) of
        {ok, RTPid} when is_pid(RTPid) ->
            RTPid ! {wrote, Bucket, Key};
        _ ->
            error_logger:error_msg("No riak_test pid to send the postcommit to!")
    end,
    ok.
