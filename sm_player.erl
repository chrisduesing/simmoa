-module(sm_player).
-behaviour(gen_server).
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([move/1, start_link/0]).
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

move(Direction) -> gen_server:call(?MODULE, {move, Direction}).

init([]) ->
       Tab = ets:new(?MODULE, []),
       {ok, Tab}.
 
handle_call({move, Direction}, _From, Tab) ->
        Reply = case ets:lookup(Tab, location) of
               [{location, {X,Y}}] ->
                   NewLocation = case Direction of
		   	       	      north -> {X + 1, Y};
				      east  -> {X, Y + 1};
				      south -> {X - 1, Y};
				      west  -> {X, Y - 1}
		   end,
		   ets:insert(Tab, {location, NewLocation}),
		   NewLocation;		 
               [] ->
                   ets:insert(Tab, {location, {0,0}}),		
		   {0,0} 
        end,
        {reply, Reply, Tab}.
 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
