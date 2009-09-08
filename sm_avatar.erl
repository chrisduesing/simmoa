%%%-------------------------------------------------------------------
%%% File    : sm_player.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : A tcp socket
%%%
%%% Created : August 29, 2009
%%%-------------------------------------------------------------------

-module(sm_avatar).

-behaviour(gen_server).

-include("player.hrl").
 
%% API
-export([move/2, notify/2, start_link/1]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
                player,			% The player record for this avatar
		location		% The current in world location of the player
               }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(#player{avatar=Avatar} = Player) -> 
  io:format("start_link for avatar ~p\n", [Avatar]),
  gen_server:start_link({local, Avatar}, ?MODULE, [Player], []).

move(Avatar, Direction) -> 
  gen_server:cast(Avatar, {move, Direction}).

notify(Avatar, Update) ->
  gen_server:cast(Avatar, {notify, Update}).

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
init([#player{avatar=Avatar} = Player]) ->
  io:format("initializing avatar ~p\n", [Avatar]),
  State = #state{player=Player, location={0,0}},
  {ok, State}.

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
handle_cast({move, Direction}, #state{player=Player, location=Location} = State) ->
  #player{avatar=Avatar} = Player,
  NewLocation = update_location(Location, Direction),
  RoomId = sm_world:get_location_id(Location),  
  NewRoomId = sm_world:get_location_id(NewLocation),
  sm_room:leave(RoomId, Avatar),
  sm_room:enter(NewRoomId, Avatar),
  NewState = State#state{location=NewLocation},
  sm_interpreter:notify({location, NewLocation}, Player),
  {noreply, NewState};

handle_cast({notify, Update}, #state{player=Player} = State) ->
  %io:format("notifying avatar of non list update ~p\n", [Update]),
  sm_interpreter:notify(Update, Player),
  {noreply, State};

handle_cast(_Msg, State) -> 
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Msg, State) -> 
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


update_location({X,Y}, north) -> {X + 1, Y};
update_location({X,Y}, east)  -> {X, Y + 1};
update_location({X,Y}, south) -> {X - 1, Y};
update_location({X,Y}, west) -> {X, Y - 1}.
