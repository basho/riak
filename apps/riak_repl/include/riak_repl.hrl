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

-record(peer_info, {
          riak_version :: string(), 
          repl_version :: string(), 
          ring :: tuple()}).

-record(fsm_state, {
          socket :: port(),
          sitename :: string(),
          my_pi :: #peer_info{},
          client :: tuple(),
          partitions = [] :: list(),
          work_dir :: string()}).

-define(socket, State#fsm_state.socket).
-define(sitename, State#fsm_state.sitename).
-define(peerinfo, State#fsm_state.my_pi).
-define(client, State#fsm_state.client).
-define(partitions, State#fsm_state.partitions).
-define(work_dir, State#fsm_state.work_dir).

