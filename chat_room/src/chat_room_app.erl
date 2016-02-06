%% @author Mochi Media <dev@mochimedia.com>
%% @copyright chat_room Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the chat_room application.

-module(chat_room_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for chat_room.
start(_Type, _StartArgs) ->
    chat_room_deps:ensure(),
    chat_room_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for chat_room.
stop(_State) ->
    ok.
