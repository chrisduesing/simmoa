%%%-------------------------------------------------------------------
%%% File    : sm_socket.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : A tcp socket
%%%
%%% Created : August 29, 2009
%%%-------------------------------------------------------------------

-module(sm_socket).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
                socket,
                player 
               }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Socket, Reference) ->
  gen_server:start_link({local, Reference}, ?MODULE, [Socket], []).


%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Socket]) ->
  io:format("initializing socket, prompting player\n", []),
  inet:setopts(Socket, [{active, false}]),
  gen_tcp:send(Socket, "username> "),
  {ok, RawData} = gen_tcp:recv(Socket, 0),
  Data = string:strip(string:strip(RawData, right, $\n), right, $\r),
%Data = "Fred",
  io:format("~p logging in.\n", [Data]),
  Player = list_to_atom(Data),
  sm_player:start_link(Player),
  inet:setopts(Socket, [{active, once}]),
  {ok, #state{socket = Socket,
              player = Player}};

init([]) ->
  io:format("entered the empty init", []),
  ok.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{socket=Socket, player=_Player} = State) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    handle_request({data, Bin}, State),
    {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_request({data, RawData}, #state{socket=Socket, player=Player} = _State)  ->
  try
	%Data = string:strip(string:strip(RawData, right, $\n), right, $\r),
	[CommandString|Args] = string:tokens(RawData, " \r\n"),
	Response = command_to_action(CommandString, Args, Player),
	gen_tcp:send(Socket, Response),
	gen_tcp:send(Socket, "\r\n> "),
	{ok}
  catch
	error:Reason ->
	    {error, Reason}
  end.
		    

command_to_action("west", _Args, Player) ->
  {X, Y} = sm_player:move(Player, west),  
  ["You moved to ",integer_to_list(X), ", ", integer_to_list(Y)];

command_to_action("w", _Args, Player) ->
  {X, Y} = sm_player:move(Player, west),  
  ["You moved to ",integer_to_list(X), ", ", integer_to_list(Y)];

command_to_action(_CommandString, _Args, _Player) ->
  %Command = list_to_atom(Data),
  "unrecognized command.".