{application, simmoa,
  [{description, "Simmoa MUD."},
  {vsn, "0.1.0"},
  {modules, [sm_app, sm_avatar, sm_room, sm_sup, sm_tcp_acceptor, sm_tcp_client, sm_world]},
  {registered, [sm_app, sm_avatar, sm_room, sm_sup, sm_tcp_acceptor, sm_tcp_client, sm_world]},
  {applications, [kernel, stdlib]},
  {mod, {sm_app, []}}
]}.