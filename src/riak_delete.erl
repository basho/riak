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

%% @doc Interface for object deletion.

-module(riak_delete).

-export([delete/5]).

%% @spec delete(riak_object:bucket(), riak_object:key(), RW :: integer(),
%%              TimeoutMillisecs :: integer(), Client :: pid()) -> term()
%% @doc Delete the object at Bucket/Key.  Direct return value is uninteresting,
%%      see riak_client:delete/3 for expected gen_server replies to Client.
delete(Bucket,Key,RW,Timeout,Client) ->           
    RealStartTime = riak_util:moment(),
    ReqID = erlang:phash2({random:uniform(),self(),Bucket,Key,RealStartTime}),
    riak_eventer:notify(riak_delete, delete_start, {ReqID, Bucket, Key}),
    {ok,C} = riak:local_client(),
    case C:get(Bucket,Key,RW,Timeout) of
        {ok, OrigObj} ->
            RemainingTime = Timeout - (riak_util:moment() - RealStartTime),
            OrigMD = hd([MD || {MD,_V} <- riak_object:get_contents(OrigObj)]),
            NewObj = riak_object:update_metadata(OrigObj,
                            dict:store(<<"X-Riak-Deleted">>, "true", OrigMD)),
            Reply = C:put(NewObj, RW, RW, RemainingTime),
            case Reply of
                ok -> 
                    spawn(
                      fun()-> reap(Bucket,Key,RemainingTime,Timeout,ReqID) end);
                _ -> nop
            end,
            riak_eventer:notify(riak_delete, delete_reply, {ReqID, Reply}),
            Client ! Reply;
        {error, notfound} ->
            riak_eventer:notify(riak_delete, delete_reply,
                                {ReqID, {error, notfound}}),
            Client ! {error, notfound};
        X ->
            riak_eventer:notify(riak_delete, delete_reply, {ReqID, X}),
            Client ! X
    end.

reap(Bucket, Key, WaitTime, Timeout, ReqId) ->
    timer:sleep(WaitTime),
    {ok,C} = riak:local_client(),
    case C:get(Bucket,Key,1,Timeout) of
        {error, notfound} ->
            riak_eventer:notify(riak_delete, finalize_reap, 
                                {ReqId, Bucket, Key, ok});
        {ok, _Obj} ->
            riak_eventer:notify(riak_delete, finalize_reap, 
                                {ReqId, Bucket, Key, not_deleted});
        O ->
            riak_eventer:notify(riak_delete, finalize_reap,
                                {ReqId, Bucket, Key, O})
    end.
