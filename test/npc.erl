
-module(npc).
 
-behaviour(gen_fsm).
 
%% API
-export([start_link/0]).
 
%% gen_fsm callbacks
-export([init/1, static/2, moving/2, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([hero_join/0, hero_leave/0]).
 
-define(SERVER, ?MODULE).
 
-record(npc, {state}).
 
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
 
init([]) ->
    io:format("init...~n"),
    State = #npc{state = static},
    io:format("init State: ~p~n", [State#npc.state]),
    {ok, static, State}.
 
hero_join() ->
    gen_fsm:send_event(?SERVER, hero_join).
 
hero_leave() ->
    gen_fsm:send_event(?SERVER, hero_leave).
 
static(Event, State) ->
    case Event of
    hero_join -> 
        do_moving(), 
        NewState = State#npc{state = moving},
        io:format("npc set state: ~p~n", [NewState#npc.state]),
        {next_state, moving, NewState};
    _ ->
        NewState = State#npc{state = static},
        io:format("npc state is: ~p~n", [NewState#npc.state]),
        {next_state, static, NewState}
    end.
 
 moving(Event, State) ->
    case Event of
    hero_leave -> 
        do_static(),
        NewState = State#npc{state = static},
        io:format("npc set state: ~p~n", [NewState#npc.state]),
        {next_state, static, NewState};
    _ ->
        NewState = State#npc{state = moving},
        io:format("npc state is: ~p~n", [NewState#npc.state]),
        {next_state, moving, NewState}
    end.
 
handle_event(_Event, StateName, State) ->
    io:format("npc state name is: ~p, State is ~p.~n", [StateName, State#npc.state]),
    {next_state, StateName, State}.
 
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

do_moving() ->
    io:format("npc beigin moving...~n").
 
do_static() ->
    io:format("npc stop moving, join static...~n").