%% Clear variables.
f().

%% Search Parameters.
Bucket = <<"mapred_test">>.
SearchString = "one OR two OR three".

%% Create the clients.
{ok, RiakClient} = riak:local_client().
{ok, SearchClient} = riak_search:local_client().


%% Map Function.
MapFun = fun(Obj, _, _) ->
    io:format("Obj: ~p~n", [Obj]),
    [1]
end.

%% Reduce Function.
ReduceFun = fun(Inputs, _) -> 
    io:format("Inputs: ~p~n", [Inputs]),
    [lists:sum(Inputs)]
end.

%% Create some objects
riak_search_kv_hook:install(Bucket).
RiakClient:put(riak_object:new(Bucket, <<"key1">>, <<"This is value one">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key2">>, <<"This is value two">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key3">>, <<"This is value three">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key4">>, <<"This is value four">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key5">>, <<"This is value five">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key6">>, <<"This is value six">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key7">>, <<"This is value seven">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key8">>, <<"This is value eight">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key9">>, <<"This is value nine">>), 1).
RiakClient:put(riak_object:new(Bucket, <<"key10">>, <<"This is value ten">>), 1).

Query = [
    {map, {qfun, MapFun}, undefined, false},
    {reduce, {qfun, ReduceFun}, undefined, true}
].

{ok, [Count]} = SearchClient:mapred(Bucket, SearchString, Query, undefined, 10000).
