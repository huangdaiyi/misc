-module(dynamodb).
-author("benjamin.c.yan@newegg.com").
-behaviour(gen_server).

-export([init/1,  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([put/1, get/1, delete/1]).
-export ([test/0]).

%%%
% gen_server callback function.
%%%

init(_Args) ->
    {ok, {AccessKeyID, SecretAccessKey, Host}} = application:get_env(sqsdemo, dynamodb),
    % configuration must be in one same process
    % otherwise use erlcloud_sqs:send_message(QueueName, Message, Config)
    % Config :: #aws_config{} in erlcloud
    erlcloud_ddb:configure(AccessKeyID, SecretAccessKey, Host),
    {ok, undefined}.

handle_call(undefined, _From, State) ->
    % put items
    Id = list_to_binary(io_lib:format("NE_~p", [random:uniform(10000)])),
    Name = list_to_binary(io_lib:format("Ben_~p", [random:uniform(10000)])),
    Columns = [{id, Id}, {name, Name}, {age, 27},{tail, true}],
    io:format("put item : ~p~n",  [Columns]),
    % table name must be binary(), column name must be binary()
    {ok, _} = erlcloud_ddb:put_item(<<"documents">>, Columns),

    io:format("get item : ~p~n",  [Id]),
    {ok, NE_Columns} = erlcloud_ddb:get_item(<<"documents">>, Id),
    Name = proplists:get_value(<<"name">>, NE_Columns),
    io:format("delete item : ~p~n",  [Id]),
    {ok, _} = erlcloud_ddb:delete_item(<<"documents">>, Id),

    io:format("get item : ~p~n",  [Id]),
    {ok, []} = erlcloud_ddb:get_item(<<"documents">>, Id),
    {reply, true, State};

handle_call({get, Key}, _From, State) ->
    % put items
    {ok, Columns} = erlcloud_ddb:get_item(<<"documents">>, Key),
    lists:foreach(fun({Name, Value}) ->
        io:format("Name : ~p Value: ~p ~n", [Name, Value])
    end, Columns),
    {reply, true, State};

handle_call({del, Key}, _From, State) ->
    % put items
    Result = erlcloud_ddb:delete_item(<<"documents">>, Key),
    io:format("Result ~p ~n", [Result]),
    {reply, true, State};

handle_call({put, Items}, _From, State) ->
    % put items
    Result = erlcloud_ddb:put_item(<<"documents">>, Items),
    io:format("result : ~p ~n", [Result]),
    {reply, true, State}.

handle_cast(_Request, State) ->
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

put(Items) ->
    % dynamodb:test([{<<"id">>, <<"40">>}, {<<"name">>, <<"benjamin">>}, {<<"tail">>, true}]).
    gen_server:call(?MODULE, {put, Items}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

delete(Id) ->
    gen_server:call(?MODULE, {del, Id}).

test() ->
    gen_server:call(?MODULE, undefined).
