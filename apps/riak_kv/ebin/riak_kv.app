%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
{application, riak_kv,
 [
  {description, "Riak Key/Value Store"},
  {vsn, "0.9"},
  {modules, [
             bloom,
             chash,
             gen_nb_server,
             gen_server2,
             json_pp,
             mapred_resource,
             merkerl,
             ping_http_resource,
             priority_queue,
             raw_http_resource,
             raw_link_walker_resource,
             riak,
             riak_app,
             riak_backup,
             riak_cache_backend,
             riak_client,
             riak_console,
             riak_delete,
             riak_dets_backend,
             riak_ets_backend,
             riak_fs_backend,
             riak_gb_trees_backend,
             riak_get_fsm,
             riak_handoff_listener,
             riak_handoff_receiver,
             riak_handoff_sender,
             riak_js_manager,
             riak_js_sup,
             riak_js_vm,
             riak_keys_fsm,
             riak_map_executor,
             riak_map_localphase,
             riak_map_phase,
             riak_mapred_json,
             riak_mapred_query,
             riak_mapreduce,
             riak_multi_backend,
             riak_object,
             riak_phase_proto,
             riak_put_fsm,
             riak_reduce_phase,
             riak_stat,
             riak_sup,
             riak_test_util,
             riak_util,
             riak_vnode,
             riak_vnode_master,
             riak_vnode_sup,
             riak_web,
             riakserver_pb,
             slide,
             spiraltime,
             stats_http_resource,
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
         %% Default claims functions
         {wants_claim_fun, {riak_claim, default_wants_claim}},
         {choose_claim_fun, {riak_claim, default_choose_claim}},

         %% Number of VNodes allowed to do handoff concurrently.
         {handoff_concurrency, 4},

         %% Endpoint for system stats HTTP provider
         {stats_urlpath, "stats"},

         %% Secondary code paths
         {add_paths, []}
        ]}
 ]}.
