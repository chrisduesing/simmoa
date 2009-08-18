-module(sm_socket).

-export([test/0]).

test() ->
       {ok, ListenSocket} = gen_tcp:listen(5555,[{active,false}]),
       accept(ListenSocket).

accept(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> handle(Socket) end),
	accept(ListenSocket).

handle(Socket) ->
	       {ok, Data} = gen_tcp:recv(Socket, 0),
	       gen_tcp:send(Socket, Data),
	       handle(Socket).
		