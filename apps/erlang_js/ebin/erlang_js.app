% -*- mode: erlang -*-
{application, erlang_js,
 [{description,  "Interface between BEAM and JS"},
  {vsn,          "0.1"},
  {modules,      [erlang_js, erlang_js_sup, js, js_benchmark, js_cache, js_driver, js_drv_comm, js_memory, js_json]},
  {registered,   [erlang_js_sup, js_cache]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlang_js, []}}]}.
