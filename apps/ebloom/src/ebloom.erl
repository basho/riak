%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
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

-module(ebloom).
-author('Dave Smith <dizzyd@dizzyd.com>').
-export([new/3,
         insert/2,
         contains/2,
         clear/1,
         size/1,
         elements/1,
         effective_fpp/1,
         intersect/2,
         union/2,
         difference/2]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir(ebloom) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ebloom_nifs);
        Dir ->
            SoName = filename:join(Dir, ebloom_nifs)
    end,
    erlang:load_nif(SoName, 0).

new(_Count, _FalseProb, _Seed) ->
    "NIF library not loaded".

insert(_Ref, _Bin) ->
    "NIF library not loaded".

contains(_Ref, _Bin) ->
    "NIF library not loaded".

clear(_Ref) ->
    "NIF library not loaded".

size(_Ref) ->
    "NIF library not loaded".

elements(_Ref) ->
    "NIF library not loaded".

effective_fpp(_Ref) ->
    "NIF library not loaded".

intersect(_Ref, _OtherRef) ->
    "NIF library not loaded".

union(_Ref, _OtherRef) ->
    "NIF library not loaded".

difference(_Ref, _OtherRef) ->
    "NIF library not loaded".


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    0 = elements(Ref),
    insert(Ref, <<"abcdef">>),
    true = contains(Ref, <<"abcdef">>),
    false = contains(Ref, <<"zzzzzz">>).

union_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    {ok, Ref2} = new(5, 0.01, 123),
    insert(Ref, <<"abcdef">>),
    false = contains(Ref2, <<"abcdef">>),
    union(Ref2, Ref),
    true = contains(Ref2, <<"abcdef">>).

-endif.
