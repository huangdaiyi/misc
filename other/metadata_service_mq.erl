-module(metadata_service_mq).
-author("benjamin.c.yan@newegg.com").
-description("send message to active mq").
-record(connection, {pid}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([send_message/2]).

-behaviour(gen_server).

-spec init({string(), pos_integer()}) -> {ok, #connection{}}.
init({Host, Port}) ->
    Fun = fun(Msg) -> io:format("Message ~p~n",[Msg]) end,
    {ok, Pid} = stomp_client:start(Host, Port, "", "", Fun),
    {ok, #connection{pid=Pid}}.


handle_call({send,Queue,Message}, _Fram, State=#connection{pid=Pid}) ->
    Json = rfc4627:encode(Message),
    ok = stomp_client:send_queue(Queue, Json,[],Pid),
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #connection{pid=Pid}) ->
    stomp_client:stop(Pid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_link(MQConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, MQConfig, []).

%%%
% public methods
%%
send_message(Queue,Message) ->
    gen_server:call(?MODULE, {send, Queue, Message}).
