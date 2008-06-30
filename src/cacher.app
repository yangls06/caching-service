{application, cacher,
 [{description, "cacher"},
  {vsn, "0.01"},
  {modules, [
    cacher,
    cacher_app,
    cacher_sup,
    cacher_web,
    cacher_deps
  ]},
  {registered, []},
  {mod, {cacher_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
