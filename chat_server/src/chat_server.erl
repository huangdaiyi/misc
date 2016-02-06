%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc chat_server.

-module(chat_server).
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
%% @doc Start the chat_server server.
start() ->
    chat_server_deps:ensure(),
    ensure_started(crypto),
    application:start(chat_server).


%% @spec stop() -> ok
%% @doc Stop the chat_server server.
stop() ->
    application:stop(chat_server).
