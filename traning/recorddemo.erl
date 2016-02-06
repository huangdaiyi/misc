-module(recorddemo).
-export([run/1]).

-record(state,{id=1,name="roger",address="CD"}).

run(Value)->
	CurrentState = #state{},
	io:format("state: ~p~n",[CurrentState]),
	io:format("state: ~p~n",[CurrentState#state{address = Value}]),
	OtherState = #state{name = Value},
	io:format("state: ~p~n",[OtherState]).