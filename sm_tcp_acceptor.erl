%%%-------------------------------------------------------------------
%%% File    : sm_tcp_acceptor.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : Accepts TCP connections and spawns a handler process for each.
%%% Created : August 29, 2009 
%%%-------------------------------------------------------------------
-module(sm_tcp_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
                listener,		% Listening socket
                acceptor,          	% Asynchronous acceptor's internal reference
		connection_number 	% Counter
               }).

-define(SERVER, ?MODULE).
-define(PORT, 5555).
%-define(SOCKET_MODULE, 'sm_socket').

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
  process_flag(trap_exit, true),
  Opts = [{packet, 0}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(?PORT, Opts) of
    {ok, ListenSocket} ->
        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
	io:format("ListenSocket created for ~p\n", [Ref]),
        {ok, #state{listener = ListenSocket,
                    acceptor = Ref,
		    connection_number = 0}};
    {error, Reason} ->
        {stop, Reason}
    end.


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
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}},
            #state{listener=ListenSocket, acceptor=Ref, connection_number=Counter} = State) ->
    try
	io:format("Client connected ~p\n", [ClientSocket]),
        case set_sockopt(ListenSocket, ClientSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
	X = Counter + 1,
	Name = "sm_socket",
	Reference = list_to_atom(Name ++ "_" ++ integer_to_list(X)),
	Module = list_to_atom(Name),
	io:format("Starting process ~p\n", [Name]),
        {ok, Pid} = sm_sup:start_child(Module, ClientSocket, Reference),
        gen_tcp:controlling_process(ClientSocket, Pid),

        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(ListenSocket, -1) of
        {ok,    NewRef} -> ok;
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef, connection_number=X}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error}, #state{listener=ListenSocket, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

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
%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.

