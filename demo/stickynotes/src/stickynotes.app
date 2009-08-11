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
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
