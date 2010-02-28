%% This is the application resource file (.app file) for the simmoa_tcp,
%% application.
{application, simmoa_tcp, 
  [{description, "The Simmoa MUD TCP/IP (telnet) Client"},
   {vsn, "0.1.0"},
   {modules, [simmoa_tcp_app,
	     simmoa_tcp_acceptor,
	     simmoa_tcp_client,
              simmoa_tcp_sup]},
   {registered,[simmoa_tcp_sup]},
   {applications, [kernel, stdlib, simmoa]},
   {mod, {simmoa_tcp_app,[]}},
   {start_phases, []}]}.

