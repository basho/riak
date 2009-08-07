{application, stickynotes,
 [{description, "stickynotes"},
  {vsn, "0.1"},
  {modules, [
    stickynotes,
    stickynotes_app,
    stickynotes_sup,
    stickynotes_deps,
    stickynotes_resource
  ]},
  {registered, []},
  {mod, {stickynotes_app, []}},
  {env, [{riak_ip, "127.0.0.1"},
         {riak_port, 9000},
         {riak_cookie, stickynotes_cookie}]},
  {applications, [kernel, stdlib, crypto]}]}.
