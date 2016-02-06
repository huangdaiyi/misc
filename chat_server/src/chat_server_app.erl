%% @author Mochi Media <dev@mochimedia.com>
%% @copyright chat_server Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the chat_server application.

-module(chat_server_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for chat_server.
start(_Type, _StartArgs) ->
    chat_server_deps:ensure(),
    chat_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for chat_server.
stop(_State) ->
    ok.
