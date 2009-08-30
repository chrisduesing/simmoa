%%%-------------------------------------------------------------------
%%% File    : sm_sup.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : simmoa supervisor
%%% Created :  August 29, 2009
%%%-------------------------------------------------------------------
-module(sm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Module, Args, Reference) ->
  io:format("Starting child ~p with args ~p\n", [Module, Args]),
  Child = {Reference,{Module,start_link,[Args, Reference]},
            permanent,2000,worker,[Module]},
  supervisor:start_child(?MODULE, Child).

start_child(Module, Reference) ->
  io:format("Starting child ~p\n", [Module]),
  Child = {Reference,{Module,start_link,[]},
            permanent,2000,worker,[Module]},
  supervisor:start_child(?MODULE, Child).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  TcpAcceptor = {'sm_tcp_acceptor',{'sm_tcp_acceptor',start_link,[]},
            permanent,2000,worker,['sm_tcp_acceptor']},
  {ok,{{one_for_all,0,1}, [TcpAcceptor]}}.

%%====================================================================
%% Internal functions
%%====================================================================

