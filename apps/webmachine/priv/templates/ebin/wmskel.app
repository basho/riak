%%-*- mode: erlang -*-
{application, {{appid}},
 [
  {description, "{{appid}}"},
  {vsn, "1"},
  {modules, [
             {{appid}},
             {{appid}}_app,
             {{appid}}_sup,
             {{appid}}_resource
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { {{appid}}_app, []}},
  {env, []}
 ]}.
