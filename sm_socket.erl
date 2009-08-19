-module(sm_socket).

-export([test/0]).

test() ->
       {ok, ListenSocket} = gen_tcp:listen(5550,[{active,false}]),
       accept(ListenSocket).

accept(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	Player = login(Socket),
	spawn(fun() -> handle(Player, Socket) end),
	accept(ListenSocket).

login(Socket) ->
	      gen_tcp:send(Socket, "username>"),
	      {ok, Data} = gen_tcp:recv(Socket, 0),
	      Player = list_to_atom(Data),
	      sm_player:start_link(Player),
	      Player.


handle(Player, Socket) ->
	       {ok, RawData} = gen_tcp:recv(Socket, 0),
	       Data = string:strip(
			 string:strip(RawData, right, $\n),
			 right, $\r),
	       Command = list_to_atom(Data),
	       {X, Y} = sm_player:move(Player, Command),
	       Response = ["You moved to ", X, ", ", Y],
	       gen_tcp:send(Socket, Response),
	       handle(Player, Socket).
		