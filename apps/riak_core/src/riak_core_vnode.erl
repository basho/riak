-module(riak_core_vnode).
-include_lib("riak_kv/include/riak_kv_commands.hrl").
-export([reply/2, test/2]).

-spec reply(sender(), term()) -> true.
reply({Type, _Ref, Pid}, Reply) ->
    case Type of
        fsm ->
            gen_fsm:send_event(Pid, Reply);
        server ->
            gen_server:reply(Pid, Reply);
        raw ->
            Pid ! Reply
    end.
                   

test(K, V) ->
    {ok, C} = riak:local_client(),
    O = riak_object:new(<<"corevnodetest">>, K, V),
    C:put(O, 2, 2),
    {ok, O1} = C:get(<<"corevnodetest">>, K, 1),
    <<"corevnodetest">> = riak_object:bucket(O1),
    K = riak_object:key(O1),
    V = riak_object:get_value(O1),
    O1.
