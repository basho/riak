%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
{application, riak_core,
 [
  {description, "Riak Core"},
  {vsn, "0.9"},
  {modules, [
             app_helper,
             riak_core_app,
             riak_core_sup,
             riak_core_bucket,
             riak_core_claim,
             riak_core_connect,
             riak_core_event_logger,
             riak_core_eventer,
             riak_core_local_logger,
             riak_core_ring,
             riak_core_ring_manager
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  sasl,
                  crypto
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
         {gossip_interval, 60000}
        ]}
 ]}.
