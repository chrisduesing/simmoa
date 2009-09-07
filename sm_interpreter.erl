-module(sm_interpreter).

-export([interpret/3]).

-include("player.hrl").

%% API

interpret(Command, Args, Player) ->
  command_to_action(Command, Args, Player).


%% Internal 

command_to_action("north", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, north); 

command_to_action("n", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, north);

command_to_action("east", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, east);  

command_to_action("e", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, east);  

command_to_action("west", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, west);  

command_to_action("w", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, west);  

command_to_action("south", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, south);  

command_to_action("s", _Args, #player{avatar=Avatar} = _Player) ->
  sm_avatar:move(Avatar, south);  

command_to_action("help", _Args, #player{client=Client, client_module=ClientModule} = _Player) ->
  {ok, Binary} = file:read_file("help"),
  ClientModule:notify(Client, {help, Binary});

command_to_action(_CommandString, _Args, #player{client=Client, client_module=ClientModule} = _Player) ->
  ClientModule:notify(Client, {bad_command, "Sorry, could not process that. Type help for a list of valid commands."}).