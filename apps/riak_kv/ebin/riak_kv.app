%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
{application, riak_kv,
 [
  {description, "Riak Key/Value Store"},
  {vsn, "0.10"},
  {modules, [
             raw_link_walker,
             riak,
             riak_client,
             riak_kv_app,
             riak_kv_backup,
             riak_kv_cache_backend,
             riak_kv_console,
             riak_kv_delete,
             riak_kv_dets_backend,
             riak_kv_ets_backend,
             riak_kv_fs_backend,
             riak_kv_gb_trees_backend,
             riak_kv_get_fsm,
             riak_kv_handoff_listener,
             riak_kv_handoff_receiver,
             riak_kv_handoff_sender,
             riak_kv_js_manager,
             riak_kv_js_sup,
             riak_kv_js_vm,
             riak_kv_keys_fsm,
             riak_kv_map_executor,
             riak_kv_map_localphase,
             riak_kv_map_phase,
             riak_kv_mapred_json,
             riak_kv_mapred_query,
             riak_kv_mapreduce,
             riak_kv_multi_backend,
             riak_kv_pb_listener,
             riak_kv_pb_socket,
             riak_kv_pb_socket_sup,
             riak_kv_phase_proto,
             riak_kv_put_fsm,
             riak_kv_reduce_phase,
             riak_kv_ring_handler,
             riak_kv_stat,
             riak_kv_sup,
             riak_kv_test_util,
             riak_kv_util,
             riak_kv_vnode,
             riak_kv_vnode_master,
             riak_kv_vnode_sup,
             riak_kv_web,
             riak_kv_wm_link_walker,
             riak_kv_wm_mapred,
             riak_kv_wm_ping,
             riak_kv_wm_raw,
             riak_kv_wm_stats,
             riak_object,
             riakserver_pb
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl,
                  crypto,
                  riak_core
                 ]},
  {registered, []},
  {mod, {riak_kv_app, []}},
  {env, [
         %% Number of VNodes allowed to do handoff concurrently.
         {handoff_concurrency, 4},

         %% Handoff IP/port
         {handoff_port, 8099},
         {handoff_ip, "0.0.0.0"},

         %% Endpoint for system stats HTTP provider
         {stats_urlpath, "stats"},

         %% Secondary code paths
         {add_paths, []}
        ]}
 ]}.
