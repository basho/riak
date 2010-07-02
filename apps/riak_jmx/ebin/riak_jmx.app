% -*- mode: erlang -*-
{application, 
 riak_jmx,
 [{description,  "riak_jmx"},
  {vsn,          "0.2"},
  {modules,      ['riak_jmx', 
                  'riak_jmx_app', 
                  'riak_jmx_monitor',
                  'riak_jmx_sup']},
  {registered,   []},
  {mod,          {riak_jmx_app, []}},
  {env,          [{enabled, false}, {port, 41110}]},
  {applications, [kernel, stdlib, sasl, crypto]}]}.
