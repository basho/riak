%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
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


%% @doc Exercise link walking queries through the special URLs

-module(verify_link_walk_urls).

-include_lib("eunit/include/eunit.hrl").

-export([confirm/0]).

-define(NUM_NODES, 4).

-record(config, { ip, port, prefix }).


confirm() ->
    [Node0 | _] = rt:build_cluster(?NUM_NODES),
    Pbc = rt:pbc(Node0),

    lager:info("Inserting linked graph"),
    %%               (deleted)        (b/4,v4b) <-> (b/5,v5b)
    %%                  |            /
    %% (a/1,v1) <-> (a/2,v2) <-> (a/3,v3) <-> (a/4,v4) <-> (a/5,v5)  >
    %%     ^_________________________________________________________|
    put_obj(Pbc, "a", "1", "v1", [{"a", "2", "next"}]),
    put_obj(Pbc, "a", "2", "v2", [{"a", "3", "next"}, {"a", "1", "prev"}, {"b", "2", "next"}]),
    put_obj(Pbc, "a", "3", "v3", [{"a", "4", "next"}, {"b", "4", "next"}, {"a", "2", "prev"}]),
    put_obj(Pbc, "a", "4", "v4", [{"a", "5", "next"}, {"a", "3", "prev"}]),
    put_obj(Pbc, "a", "5", "v5", [{"a", "1", "next"}, {"a", "4", "prev"}]),

    put_obj(Pbc, "b", "4", "v4b", [{"b", "5", "next"}, {"a", "3", "prev"}]),
    put_obj(Pbc, "b", "5", "v5b", [{"b", "4", "prev"}]),

    Config = get_config(Node0),

    lager:info("Verifying link walk queries"),

    verify_query(Config, "a", "1", "_,next,1", 
                 ["v2"]),
    verify_query(Config, "a", "1", "_,_,1",
                 ["v2"]),
    verify_query(Config, "a", "1", "b,next,1", 
                 []),
    verify_query(Config, "a", "1", "a,next,1", 
                 ["v2"]),
    verify_query(Config, "a", "1", "_,next,1/_,next,1", 
                 ["v2", "v3"]),
    verify_query(Config, "a", "1", "_,next,1/b,next,1/_,next,1/_,next,1/_,next,1", 
                 ["v2"]),
    verify_query(Config, "a", "1", "_,next,1/_,next,1/_,next,1/_,next,1/_,next,1", 
                 ["v1", "v2", "v3", "v4", "v4b", "v5", "v5b"]),
    verify_query(Config, "a", "1", "_,next,0/_,next,1/a,next,0/a,next,1", 
                 ["v3", "v5"]),
    verify_query(Config, "a", "1", "_,next,0/_,next,0/_,prev,1/_,next,0/_,next,1", 
                 ["v2", "v4", "v4b"]),

    verify_query(Config, "a", "3", "_,_,1",
                 ["v2", "v4", "v4b"]),
    verify_query(Config, "a", "3", "a,_,1",
                 ["v2", "v4"]),
    verify_query(Config, "a", "3", "b,_,1",
                 ["v4b"]),
    verify_query(Config, "a", "3", "_,_,0/_,next,1",
                 ["v3", "v5", "v5b"]),

    verify_query(Config, "a", "5", "_,prev,1", 
                 ["v4"]),
    verify_query(Config, "a", "5", "_,prev,1/_,prev,1/_,prev,1/_,prev,1", 
                 ["v1", "v2", "v3", "v4"]),
    verify_query(Config, "a", "5", "_,prev,1/_,prev,1/_,next,1/_,next,1", 
                 ["v3", "v4", "v4", "v4b", "v5", "v5b"]),
    verify_query(Config, "a", "5", "b,next,1", 
                 []),
    verify_query(Config, "a", "5", "_,_,1",
                 ["v1", "v4"]),

    lager:info("Au revoir mes amies"),
    riakc_pb_socket:stop(Pbc),
    pass.

verify_query(Cfg, Bucket, Key, Query, Expected) ->
    lager:info("Verifying (~p,~p) '~s' -> ~p", [Bucket, Key, Query, Expected]),
    ?assertEqual(Expected, link_query(Cfg, Bucket, Key, Query)).


get_config(Node0) ->
    {ok, [{IP, Port}|_]} = 
        rpc:call(Node0, application, get_env, [riak_core, http]),
    Prefix = 
        rpc:call(Node0, app_helper, get_env, [riak_kv, raw_name, "riak"]),
    #config{ip = IP, port = Port, prefix = Prefix}.


link_query(#config{ip=IP, port=Port, prefix=Prefix}, B, K, LinkStr) ->
    Url = lists:flatten(io_lib:format("http://~s:~p/~s/~s/~s/~s", 
                                      [IP, Port, Prefix, B, K, LinkStr])),
    {ok, "200", _Headers, Body} = ibrowse:send_req(Url, [], get),
    get_return_values(Body).

%% @doc Extracts values from multipart body in a hacky way copied from
%% scripts in the fast track tutorial: simply filter out headers and 
%% multipart markers and the rest is our value lines.
get_return_values(Body) ->
    Lines = re:split(Body, "\r\n", [multiline, {return, list}]),
    Vs = [Line || Line <- Lines, 
                  length(Line) > 0, 
                  string:str(Line, ":") =:= 0, 
                  string:str(Line, "--") =:= 0],
    lists:sort(Vs).

put_obj(Pbc, Bucket, Key, Value, Links) when is_list(Bucket), is_list(Key), 
                                             is_list(Value), is_list(Links) ->
    Obj = riakc_obj:new(list_to_binary(Bucket), 
                        list_to_binary(Key), 
                        list_to_binary(Value)),
    Lns = [{{B, K}, T} || {B, K, T} <- Links],
    Md = dict:store(<<"Links">>, Lns, dict:new()),
    ObjWLinks = riakc_obj:update_metadata(Obj, Md),
    riakc_pb_socket:put(Pbc, ObjWLinks).

