%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc multidatabase.

-module(multidatabase).
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
%% @doc Start the multidatabase server.
start() ->
    multidatabase_deps:ensure(),
    application:start(crypto),
    ssl:start(),
    erlcloud:start(),
    application:start(multidatabase).


%% @spec stop() -> ok
%% @doc Stop the multidatabase server.
stop() ->
    application:stop(multidatabase).
