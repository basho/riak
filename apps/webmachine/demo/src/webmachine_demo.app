{application, webmachine_demo,
 [{description, "webmachine_demo"},
  {vsn, "0.1"},
  {modules, [
	     webmachine_demo,
	     webmachine_demo_app,
	     webmachine_demo_sup,
	     webmachine_demo_resource,
	     demo_fs_resource
	    ]},
  {registered, []},
  {mod, {webmachine_demo_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
