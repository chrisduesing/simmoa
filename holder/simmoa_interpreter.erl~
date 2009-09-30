-module(sm_interpreter).

-export([interpret/3, notify/2]).

-include("player.hrl").
-include("room.hrl").

%% API

interpret(Command, Args, Player) ->
  command_to_action(Command, Args, Player).

notify(Update, Player) ->
  notification_to_message(Update, Player).

%% Internal - inbound 

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
  ClientModule:notify(Client, Binary);

command_to_action(_CommandString, _Args, #player{client=Client, client_module=ClientModule} = _Player) ->
  ClientModule:notify(Client, "Sorry, could not process that. Type help for a list of valid commands.").

%% Internal - outbound

notification_to_message({look_result, Room}, #player{client=Client, client_module=ClientModule} = _Player) ->
  #room{avatars=Avatars, description=Description} = Room,
  AvatarMessage = "\r\n" ++ [atom_to_list(A) ++ " is standing here.\r\n" || A <- Avatars],
  RoomMessage = Description ++ AvatarMessage,
  ClientModule:notify(Client, lists:flatten(RoomMessage));

notification_to_message({location, {X, Y}}, #player{client=Client, client_module=ClientModule} = _Player) ->
  Message = ["You moved to ",integer_to_list(X), ", ", integer_to_list(Y)],
  ClientModule:notify(Client, lists:flatten(Message));

notification_to_message({_, Update}, #player{avatar=Avatar} = _Player) ->
  io:format("sm_interpreter received an unhandled update ~p for ~p~n", [Update, Avatar]);

notification_to_message(_Update, #player{avatar=Avatar} = _Player) ->
  io:format("sm_interpreter received an unhandled update for ~p, dropping~n", [Avatar]).


