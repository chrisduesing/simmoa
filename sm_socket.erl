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
	       inet:setopts(Socket,[{active,once}]),
	       receive
		{tcp, Socket, Data} ->
		      gen_tcp:send(Socket, Data),
		      handle(Socket);
		{tcp_closed,S} ->
		      io:format("Socket ~w closed [~w]~n",[S,self()]),
		      ok
		end.