%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-define(REPL_FSM_TIMEOUT, 15000).
-define(REPL_QUEUE_TIMEOUT, 1000).
-define(REPL_MERK_TIMEOUT, infinity).
-define(REPL_CONN_RETRY, 30000).
-define(DEFAULT_REPL_PORT, 9010).
-define(NEVER_SYNCED, {0, 0, 0}).
-define(MERKLE_BUFSZ, 1048576*10).
-define(MERKLE_CHUNKSZ, 65536).
-define(FSM_SOCKOPTS, [{active, once}, {packet, 4}]).
-define(REPL_VERSION, 3).

-type(ip_addr_str() :: string()).
-type(ip_portnum() :: non_neg_integer()).
-type(repl_addr() :: {ip_addr_str(), ip_portnum()}).
-type(repl_addrlist() :: [repl_addr()]).
-type(repl_socket() :: port()).
-type(repl_sitename() :: string()).
-type(ring() :: tuple()).

-record(peer_info, {
          riak_version :: string(), %% version number of the riak_kv app
          repl_version :: string(), %% version number of the riak_kv app
          ring         :: ring()    %% instance of riak_core_ring()
         }).

-record(fsm_state, {
          socket          :: repl_socket(),   %% peer socket
          sitename        :: repl_sitename(), %% peer sitename
          my_pi           :: #peer_info{},    %% local peer_info
          client          :: tuple(),         %% riak local_client
          partitions = [] :: list(),          %% list of local partitions
          work_dir        :: string()         %% working directory 
         }).

-record(repl_listener, {
          nodename    :: atom(),     %% cluster-local node name
          listen_addr :: repl_addr() %% ip/port to bind/listen on
         }).
-record(repl_site, {
          name  :: repl_sitename(),   %% site name
          addrs=[] :: repl_addrlist(),%% list of ip/ports to connect to
          last_sync=?NEVER_SYNCED :: tuple()  
         }).


-define(REPL_HOOK, {struct, 
                    [{<<"mod">>, <<"riak_repl_leader">>},
                     {<<"fun">>, <<"postcommit">>}]}).
