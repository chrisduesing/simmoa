%%%-------------------------------------------------------------------
%%% File    : .erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : gen server.
%%%%%% Created :  
%%%-------------------------------------------------------------------
-module(simmoa_world2).

-behaviour(gen_server).

%% API
-export([start_link/0, add_location/2, get_location_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("location.hrl").

-record(state, {location_map}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(World) ->
  gen_server:start_link({local, World}, ?MODULE, [], []).

add_location(World, Location) ->
    gen_server:call(World, {add_location, Location}).

get_location_id(#location{coordinates=Coordinates} = Location) ->
  get_location_id(Coordinates);

get_location_id({X, Y, Z}) ->
  list_to_atom(lists:concat(['loc_', X, '_', Y, '_', Z])).

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
init([]) ->
  LocationMap = dicts:new(),
  {ok, #state{location_map=LocationMap}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_location, Location}, _From, #state{location_map=LocationMap} = State) where is_record(Location, location) ->
    LocationId = get_location_id(Location),
    NewLocationMap = dict:append(LocationId, Location, LocationMap),
    NewState = State#state{location_map=LocationMap},
    {reply, Reply, NewState}.

%handle_call({add_location, Location}, _From, #state{location_map=LocationMap} = State) where is_list(Location) ->
%    LocationId = get_location_id(Location),
%    NewLocationMap = dict:append(LocationId, Location, LocationMap), % oops thats one key for all values, need a recursive fun to handle this case
%    NewState = State#state{location_map=LocationMap},
%    {reply, Reply, NewState}.

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

