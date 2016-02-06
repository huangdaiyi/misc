%% @author Mochi Media <dev@mochimedia.com>
%% @copyright pomodoro Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the pomodoro application.

-module(pomodoro_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for pomodoro.
start(_Type, _StartArgs) ->
    pomodoro_deps:ensure(),
    pomodoro_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for pomodoro.
stop(_State) ->
    ok.
