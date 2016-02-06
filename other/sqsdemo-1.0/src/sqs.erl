-module(sqs).
-author("benjamin.c.yan@newegg.com").
-behaviour(gen_server).

-export([init/1,  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([send_message/2, receive_message/1, delete_message/2]).

%%%
% gen_server callback function.
%%%

init(_Args) ->
    %{sqs, {"AKIAIP4T4Y5F2DBCKZVA", "1RsvWchQ9eKLZo9OicRFZ85CmWsSrJCkT0SzmO9U", "sqs.ap-southeast-1.amazonaws.com"}},
    %{dynamodb, {"AKIAIP4T4Y5F2DBCKZVA", "1RsvWchQ9eKLZo9OicRFZ85CmWsSrJCkT0SzmO9U", "dynamodb.ap-southeast-1.amazonaws.com"}}

    {ok, {AccessKeyID, SecretAccessKey, Host}} = application:get_env(sqsdemo, sqs),
    % configuration must be in one same process
    % otherwise use erlcloud_sqs:send_message(QueueName, Message, Config)
    % Config :: #aws_config{} in erlcloud
    erlcloud_sqs:configure(AccessKeyID, SecretAccessKey, Host),
    {ok, undefined}.

handle_call({sqs, QueueName, Message}, _From, State) ->
    erlcloud_sqs:send_message(QueueName, Message),
    io:format("send message ~p to ~p~n", [Message, QueueName]),
    {reply, true, State};

handle_call({sqs_retrieve, QueueName}, _From, State) ->
    [{messages, Messages}] = erlcloud_sqs:receive_message(QueueName, [], 3),
    lists:foreach(fun (Message) ->
        % business logic
        Body = proplists:get_value(body, Message),
        Handle = proplists:get_value(receipt_handle, Message),
        io:format("queue : ~s message : ~s ~n", [QueueName, Body]),
        % delete message
        ok = erlcloud_sqs:delete_message(QueueName, Handle)
    end, Messages),
    % io:format("receive message ~p to ~p~n", [Message, QueueName]),
    {reply, true, State};

handle_call({sqs_delete, QueueName, Handle}, _From, State) ->
    Result = erlcloud_sqs:delete_message(QueueName, Handle),
    {reply, Result, State}.

handle_cast(run, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
% public function
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_message(QueueName, Message) ->
    gen_server:call(?MODULE,  {sqs, QueueName, Message}).

receive_message(QueueName) ->
    gen_server:call(?MODULE, {sqs_retrieve, QueueName}).

delete_message(QueueName, Handle) ->
    gen_server:call(?MODULE, {sqs_delete, QueueName, Handle}).
