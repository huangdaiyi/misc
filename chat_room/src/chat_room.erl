%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc chat_room.

-module(chat_room).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the chat_room server.
start() ->
    chat_room_deps:ensure(),
    ensure_started(crypto),
    application:start(chat_room).


%% @spec stop() -> ok
%% @doc Stop the chat_room server.
stop() ->
    application:stop(chat_room).
