%% This is the application resource file (.app file) for the simmoa,
%% application.
{application, simmoa, 
  [{description, "The Simmoa MUD Engine"},
   {vsn, "0.1.0"},
   {modules, [simmoa_app,
              simmoa_sup]},
   {registered,[simmoa_sup]},
   {applications, [kernel, stdlib]},
   {mod, {simmoa_app,[]}},
   {start_phases, []}]}.

