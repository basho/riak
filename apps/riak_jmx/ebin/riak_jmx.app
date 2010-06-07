% -*- mode: erlang -*-
{application, 
 riak_jmx,
 [{description,  "riak_jmx"},
  {vsn,          "0.1"},
  {modules,      ['riak_jmx', 
                  'riak_jmx_app', 
                  'riak_jmx_monitor',
                  'riak_jmx_sup']},
  {registered,   []},
  {mod,          {riak_jmx_app, []}},
  {env,          [{enabled, false}]},
  {applications, [kernel, stdlib, sasl, crypto]}]}.
