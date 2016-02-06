%% @author Mochi Media <dev@mochimedia.com>
%% @copyright multidatabase Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the multidatabase application.

-module(multidatabase_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for multidatabase.
start(_Type, _StartArgs) ->
    multidatabase_deps:ensure(),
    multidatabase_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for multidatabase.
stop(_State) ->
    ok.
