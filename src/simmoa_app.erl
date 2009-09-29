%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the simmoa application.

-module(simmoa_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for simmoa.
start(_Type, _StartArgs) ->
    simmoa_deps:ensure(),
    simmoa_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for simmoa.
stop(_State) ->
    ok.
