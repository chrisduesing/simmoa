-module(sm_player).
-behaviour(gen_server).
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([move/2, start_link/1]).
 
start_link(Player) -> gen_server:start_link({local, Player}, ?MODULE, [Player], []).

move(Player, Direction) -> gen_server:call(Player, {move, Direction}).

init([Player]) ->
	       io:format("initializing player ~p\n", [Player]),
       Tab = ets:new(Player, []),
       {ok, Tab}.
 
handle_call({move, Direction}, _From, Tab) ->
        Reply = case ets:lookup(Tab, location) of
               [{location, Location}] ->
                   NewLocation = update_location(Location, Direction),
		   ets:insert(Tab, {location, NewLocation}),
		   NewLocation;		 
               [] ->
                   NewLocation = {0,0},
		   ets:insert(Tab, {location, NewLocation}),		
		   NewLocation
        end,
        {reply, Reply, Tab}.
 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


update_location({X,Y}, north) -> {X + 1, Y};
update_location({X,Y}, east)  -> {X, Y + 1};
update_location({X,Y}, south) -> {X - 1, Y};
update_location({X,Y}, west) -> {X, Y - 1}.
