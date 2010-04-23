%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
{application, riak_core,
 [
  {description, "Riak Core"},
  {vsn, "0.10"},
  {modules, [
             app_helper,
             bloom,
             chash,
             gen_nb_server,
             gen_server2,
             json_pp,
             merkerl,
             priority_queue,
             riak_core_app,
             riak_core_bucket,
             riak_core_claim,
             riak_core_gossip,
             riak_core_ring,
             riak_core_ring_events,
             riak_core_ring_manager,
             riak_core_sup,
             riak_core_test_util,
             riak_core_util,
             riak_core_web,
             slide,
             spiraltime,
             vclock
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  sasl,
                  crypto,
                  webmachine
                 ]},
  {mod, { riak_core_app, []}},
  {env, [
         %% Cluster name
         {cluster_name, "default"},

         %% Default location of ringstate
         {ring_state_dir, "data/ring"},

         %% Default ring creation size
         {ring_creation_size, 64},

         %% Default gossip interval (milliseconds)
         {gossip_interval, 60000},

         %% Target N value
         {target_n_val, 3},

         %% Default claims functions
         {wants_claim_fun, {riak_core_claim, default_wants_claim}},
         {choose_claim_fun, {riak_core_claim, default_choose_claim}},

         %% Default bucket props
         {default_bucket_props, [{n_val,3},
                                 {allow_mult,false},
                                 {precommit, []},
                                 {postcommit, []},
                                 {chash_keyfun, {riak_core_util, chash_std_keyfun}}]}

        ]}
 ]}.
