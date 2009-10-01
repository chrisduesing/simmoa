{application, simmoa,
 [{description, "Simmoa Mud"},
  {vsn, "0.01"},
  {modules, [
    simmoa,
    simmoa_app,
    simmoa_sup,
    simmoa_web,
    simmoa_deps,
    simmoa_avatar,
    simmoa_room,
    simmoa_tcp_acceptor,
    simmoa_tcp_client,
    simmoa_world
  ]},
  {registered, []},
  {mod, {simmoa_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
