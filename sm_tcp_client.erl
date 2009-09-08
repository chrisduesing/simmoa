%%%-------------------------------------------------------------------
%%% File    : sm_tcp_client.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : A tcp socket
%%%
%%% Created : August 29, 2009
%%%-------------------------------------------------------------------

-module(sm_tcp_client).

-behaviour(gen_server).

-include("player.hrl").

%% API
-export([start_link/2, notify/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
                socket,			% The socket the client is connected on
                player			% The player record
               }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Socket, Client) ->
  gen_server:start_link({local, Client}, ?MODULE, [Socket, Client], []).

notify(Client, Event) ->
  gen_server:cast(Client, {notify, Event}).

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
init([Socket, Client]) ->
  io:format("initializing socket, prompting player\n", []),
  inet:setopts(Socket, [{active, false}]),
  gen_tcp:send(Socket, "username> "),
  {ok, RawData} = gen_tcp:recv(Socket, 0),
  Data = string:strip(string:strip(RawData, right, $\n), right, $\r),
  io:format("~p logging in.\n", [Data]),
  Avatar = list_to_atom(Data),
  Player = #player{avatar=Avatar, client=Client, client_module=?MODULE},
  sm_avatar:start_link(Player),
  {ok, Motd} = file:read_file("motd"),
  write_to_output(Socket, Motd),
  RoomId = sm_world:get_location_id({0,0}),
  sm_room:enter(RoomId, Avatar),  
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
handle_cast({notify, Event},  #state{socket=Socket} = State) ->
  Message = [Event],
  io:format("sending ~s to client \n", [Message]),
  write_to_output(Socket, Message),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("Client Socket received an unhandled event ~p~n", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{socket=Socket} = State) ->
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

write_to_output(Socket, Message) ->
  gen_tcp:send(Socket, "\r\n"),
  gen_tcp:send(Socket, Message),
  gen_tcp:send(Socket, "\r\n>").


handle_request({data, RawData}, #state{player=Player} = _State)  ->
  [CommandString|Args] = string:tokens(RawData, " \r\n"),
  sm_interpreter:interpret(CommandString, Args, Player),
  ok.

