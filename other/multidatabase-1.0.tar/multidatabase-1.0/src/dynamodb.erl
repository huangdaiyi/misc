-module(dynamodb).
-author("benjamin.c.yan@newegg.com").
-behaviour(gen_server).

-export([init/1,  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([put/2, get/2, get_all/1, clear/1]).

%%%
% gen_server callback function.
%%%

init({AccessKeyID, SecretAccessKey, Host}) ->
    erlcloud_ddb:configure(AccessKeyID, SecretAccessKey, Host),
    {ok, undefined}.

handle_call({put, TableName, Columns}, _From, State) when is_binary(TableName), is_list(Columns) ->
    case erlcloud_ddb:put_item(TableName, Columns) of
        {ok, _} -> {reply, true, State};
        {error, Msg} -> {reply, {error, Msg}, State}
    end;

handle_call({scan, TableName}, _From, State) when is_binary(TableName) ->
    case erlcloud_ddb:scan(TableName) of
        {ok, Items} -> {reply, {ok, Items}, State};
        {error, Msg} -> {reply, {error, Msg}, State}
    end;

handle_call({clear, TableName}, _From, State) when is_binary(TableName) ->
    case erlcloud_ddb:scan(TableName) of
        {ok, Items} ->
            lists:foreach(fun(Item) ->
                Id = proplists:get_value(<<"id">>, Item),
                {ok, _} = erlcloud_ddb:delete_item(TableName, Id)
            end, Items),
            {reply, true, State};
        {error, Msg} -> {reply, {error, Msg}, State}
    end;

handle_call({get, TableName, Id}, _From, State) when is_binary(TableName) ->
    case erlcloud_ddb:get_item(TableName, Id) of
        {ok, Columns} -> {reply, {ok, Columns}, State};
        {error, Msg} -> {reply, {error, Msg}, State}
    end.

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
start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

put(TableName, Items) ->
    gen_server:call(?MODULE, {put, TableName, Items}).

get(TableName, Id) ->
    gen_server:call(?MODULE, {get, TableName, Id}).

get_all(TableName) ->
    gen_server:call(?MODULE, {scan, TableName}).

clear(TableName) ->
    gen_server:call(?MODULE, {clear, TableName}, infinity).
