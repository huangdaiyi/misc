%% @author Mochi Media <dev@mochimedia.com>
%% @copyright sqsdemo Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the sqsdemo application.

-module(sqsdemo_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for sqsdemo.
start(_Type, _StartArgs) ->

    sqsdemo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for sqsdemo.
stop(_State) ->
    ok.
