%%%-------------------------------------------------------------------
%%% File    : sm_room.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : gen server.
%%% Created : September 2009
%%%-------------------------------------------------------------------
-module(simmoa_room).

-behaviour(gen_server).

-include("room.hrl").

%% API
-export([start_link/2, enter/2, leave/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Location, Description) ->
  gen_server:start_link({local, sm_world:get_location_id(Location)}, ?MODULE, [Location, Description], []).

enter(Location, Avatar) ->
  gen_server:cast(Location, {enter, Avatar}).

leave(Location, Avatar) ->
  gen_server:cast(Location, {leave, Avatar}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Location, Description]) ->
  Room = #room{location=Location, exits=[n,e,s,w], avatars=[], description=Description},
  {ok, Room}.

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
handle_cast({enter, Avatar}, #room{avatars=Avatars} = Room) ->
  NewAvatars = [Avatar | Avatars],
  UpdatedRoom = Room#room{avatars=NewAvatars},
  sm_avatar:notify(Avatar, {look_result, Room}),
  {noreply, UpdatedRoom};

handle_cast({leave, Avatar}, #room{avatars=Avatars} = Room) ->
  NewAvatars = Avatars -- [Avatar],
  UpdatedRoom = Room#room{avatars=NewAvatars},
  {noreply, UpdatedRoom};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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


