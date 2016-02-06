-module(sqsdemo).
-export ([start/0, stop/0]).

start() ->
    sqsdemo_deps:ensure(),
    application:start(crypto),
    ssl:start(),
    erlcloud:start(),
    application:start(sqsdemo).

stop() ->
    application:stop(sqsdemo).
