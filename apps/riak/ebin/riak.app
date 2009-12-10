% -*- mode: erlang -*-
{application, riak,
 [{description, "riak"},
  {vsn, "0.6"},
  {modules, [
             chash,
             gen_server2,
             jaywalker_resource,
             jiak,
             jiak_client,
             jiak_context,
             jiak_context_tests,
             jiak_default,
             jiak_default_tests,
             jiak_example,
             jiak_object,
             jiak_resource,
             jiak_util,
             json_pp,
             merkerl,
             priority_queue,
             raw_http_resource,
             raw_link_walker_resource,
             riak,
             riak_app,
             riak_backup,
             riak_bucket,
             riak_claim,
             riak_client,
             riak_connect,
             riak_delete,
             riak_dets_backend,
             riak_ets_backend,
             riak_event_logger,
             riak_eventer,
             riak_fs_backend,
             riak_gb_trees_backend,
             riak_get_fsm,
             riak_keys_fsm,
             riak_local_logger,
             riak_map_executor,
             riak_map_localphase,
             riak_map_phase_fsm,
             riak_mapreduce,
             riak_mapreduce_fsm,
             riak_object,
             riak_osmos_backend,
             riak_put_fsm,
             riak_reduce_phase_fsm,
             riak_ring,
             riak_ring_manager,
             riak_stat,
             riak_sup,
             riak_test_util,
             riak_util,
             riak_vnode,
             riak_vnode_master,
             riak_web,
             spiraltime,
             vclock
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl,
                  crypto
                 ]},
  {registered, []},
  {mod, {riak_app, []}},
  {env, [
         %% Cluster name
         {cluster_name, "default"},
         
         %% Default location of ringstate
         {ring_state_dir, "ringstate"},
         
         %% Default ring creation size
         {ring_creation_size, 64},
         
         %% Default gossip interval (milliseconds)
         {gossip_interval, 60000},

         %% Default claims functions
         {wants_claim_fun, {riak_claim, default_wants_claim}},
         {choose_claim_fun, {riak_claim, default_choose_claim}},

         %% Secondary code paths
         {add_paths, []}
        ]}
 ]}.

