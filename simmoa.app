{application, simmoa,
  [{description, "Simmoa MUD."},
  {vsn, "0.1.0"},
  {modules, [sm_app, sm_sup, sm_tcp_acceptor, sm_tcp_client, sm_player]},
  {registered, [sm_app, sm_sup, sm_tcp_acceptor, sm_tcp_client, sm_player]},
  {applications, [kernel, stdlib]},
  {mod, {sm_app, []}}
]}.