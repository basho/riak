
-module(verify_api_timeouts).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    [Node] = rt:build_cluster(1),
    rt:wait_until_pingable(Node),
    
    HC = rt:httpc(Node),
    lager:info("setting up initial data and loading remote code"),
    rt:httpc_write(HC, <<"foo">>, <<"bar">>, <<"foobarbaz\n">>),
    rt:httpc_write(HC, <<"foo">>, <<"bar2">>, <<"foobarbaz2\n">>),

    rt_intercept:add(Node, {riak_kv_get_fsm,
                            [{{prepare,2}, slow_prepare}]}),
    rt_intercept:add(Node, {riak_kv_put_fsm,
                            [{{prepare,2}, slow_prepare}]}),
    
    
    lager:info("testing HTTP API"),

    lager:info("testing GET timeout"),
    {error, Tup1} = rhc:get(HC, <<"foo">>, <<"bar">>, [{timeout, 100}]),
    ?assertMatch({ok, "503", _, <<"request timed out\n">>}, Tup1),
    
    lager:info("testing PUT timeout"),
    {error, Tup2} = rhc:put(HC, riakc_obj:new(<<"foo">>, <<"bar">>,
                                              <<"getgetgetgetget\n">>),
                            [{timeout, 100}]),
    ?assertMatch({ok, "503", _, <<"request timed out\n">>}, Tup2),
 
    lager:info("testing DELETE timeout"),
    {error, Tup3} = rhc:delete(HC, <<"foo">>, <<"bar">>, [{timeout, 100}]),
    ?assertMatch({ok, "503", _, <<"request timed out\n">>}, Tup3),
    
    lager:info("testing invalid timeout value"),
    {error, Tup4} = rhc:get(HC, <<"foo">>, <<"bar">>, [{timeout, asdasdasd}]),
    ?assertMatch({ok, "400", _,
                  <<"Bad timeout value \"asdasdasd\"\n">>}, 
                 Tup4),

    lager:info("testing GET still works before long timeout"),
    {ok, O} = rhc:get(HC, <<"foo">>, <<"bar">>, [{timeout, 4000}]),

    %% either of these are potentially valid.
    case riakc_obj:get_value(O) of 
        <<"foobarbaz\n">> -> 
            lager:info("Original Value"),
            ok;
        <<"getgetgetgetget\n">> -> 
            lager:info("New Value"),
            ok;
        V -> ?assertEqual({object_value, <<"getgetgetgetget\n">>}, 
                          {object_value, V})
    end,


    PC = rt:pbc(Node),

    lager:info("testing PBC API"),

    BOOM = {error, <<"timeout">>},

    lager:info("testing GET timeout"),
    PGET = riakc_pb_socket:get(PC, <<"foo">>, <<"bar2">>, [{timeout, 100}]),
    ?assertEqual(BOOM, PGET),

    lager:info("testing PUT timeout"),
    PPUT = riakc_pb_socket:put(PC, 
                               riakc_obj:new(<<"foo">>, <<"bar2">>,
                                             <<"get2get2get2get2get\n">>),
                               [{timeout, 100}]),
    ?assertEqual(BOOM, PPUT),
 
    lager:info("testing DELETE timeout"),
    PDEL = riakc_pb_socket:delete(PC, <<"foo">>, <<"bar2">>, 
                                  [{timeout, 100}]),
    ?assertEqual(BOOM, PDEL),

    lager:info("testing invalid timeout value"),
    ?assertError(badarg, riakc_pb_socket:get(PC, <<"foo">>, <<"bar2">>, 
                                             [{timeout, asdasdasd}])),

    lager:info("testing GET still works before long timeout"),
    {ok, O2} = riakc_pb_socket:get(PC, <<"foo">>, <<"bar2">>, 
                                  [{timeout, 4000}]),

    %% either of these are potentially valid.
    case riakc_obj:get_value(O2) of  
        <<"get2get2get2get2get\n">> -> 
            lager:info("New Value"),
            ok;
        <<"foobarbaz2\n">> -> 
            lager:info("Original Value"),
            ok;
        V2 -> ?assertEqual({object_value, <<"get2get2get2get2get\n">>}, 
                           {object_value, V2})
    end,
    pass.    
