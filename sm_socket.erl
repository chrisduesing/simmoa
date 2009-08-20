-module(sm_socket).

-export([test/0]).


test() ->
       {ok, ListenSocket} = gen_tcp:listen(5550,[{active,false}]),
       accept(ListenSocket).


accept(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> catch handle(Socket) end),
	accept(ListenSocket).


handle(Socket) ->
	  try
	       Player = login(Socket),
	       loop(Player, Socket)
	  catch
		error:_Reason ->
		    gen_tcp:close(Socket)
	  end.

login(Socket) ->
	      gen_tcp:send(Socket, "username> "),
	      {ok, Data} = gen_tcp:recv(Socket, 0),
	      Player = list_to_atom(Data),
	      sm_player:start_link(Player),
	      Player.


loop(Player, Socket) ->
	       	gen_tcp:send(Socket, "> "),
	        {ok, RawData} = gen_tcp:recv(Socket, 0),
	       	Data = string:strip(string:strip(RawData, right, $\n), right, $\r),
		Command = list_to_atom(Data),
	       	{X, Y} = sm_player:move(Player, Command),
	       	Response = ["You moved to ", X, ", ", Y, "\r\n"],
	       	gen_tcp:send(Socket, Response),
	       	loop(Player, Socket).
		    
