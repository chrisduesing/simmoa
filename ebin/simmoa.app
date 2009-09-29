{application, simmoa,
 [{description, "simmoa"},
  {vsn, "0.01"},
  {modules, [
    simmoa,
    simmoa_app,
    simmoa_sup,
    simmoa_web,
    simmoa_deps
  ]},
  {registered, []},
  {mod, {simmoa_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
