%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc pomodoro.

-module(pomodoro).
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
%% @doc Start the pomodoro server.
start() ->
    pomodoro_deps:ensure(),
    ensure_started(crypto),
    application:start(pomodoro).


%% @spec stop() -> ok
%% @doc Stop the pomodoro server.
stop() ->
    application:stop(pomodoro).
