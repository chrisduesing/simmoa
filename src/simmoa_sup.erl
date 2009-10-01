%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the simmoa application.

-module(simmoa_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_child/3, start_player/1, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("player.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Module, Args, Reference) ->
    io:format("Starting ~p as module ~p with args ~p~n", [Reference, Module, Args]),  
    Child = {Reference,{Module,start_link,[Args, Reference]}, permanent,2000,worker,[Module]},
    supervisor:start_child(?MODULE, Child).

start_player(#player{avatar=Avatar, client=Client, client_module=ClientModule} = Player) ->
    io:format("Starting avatar ~p with reference to client ~p in module ~p ~n", [Avatar, Client, ClientModule]),
    Child = {Avatar,{'sm_avatar','start_link',[Player]},
            permanent,2000,worker,['sm_avatar']},
    supervisor:start_child(?MODULE, Child).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
         {ip, Ip},
                 {port, 8000},
                 {docroot, simmoa_deps:local_path(["priv", "www"])}],
    Web = {simmoa_web,
           {simmoa_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    TcpAcceptor = {'sm_tcp_acceptor',{'simmoa_tcp_acceptor',start_link,[]},
            permanent,2000,worker,['simmoa_tcp_acceptor']},
    World = {'sm_world',{'simmoa_world',start_link,[]},
            permanent,2000,worker,['simmoa_world']},

    Processes = [Web, TcpAcceptor, World],
    {ok, {{one_for_one, 10, 10}, Processes}}.
