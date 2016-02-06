-module(test_erlydtl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    test_erlydtl_sup:start_link().

stop(_State) ->
    ok.