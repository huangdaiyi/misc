-module (chat_gen_server).
-behaviour (gen_server).

%% API
-export ([add/2,delete/2,get/2,get_all/1,get_last/1,start_link/1]).

%% gen_server callbacks
-export ([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


start_link(ServerName) ->
        gen_server:start_link({local, ServerName}, ?MODULE, [], []).


add(Message,ServerName)->
    	gen_server:call(ServerName,{add, Message}).
   
       

delete(Message,ServerName) ->
        gen_server:call(ServerName,{delete, Message}).

get(Index,ServerName) ->
      gen_server:call(ServerName,{get, Index}).


get_all(ServerName)  ->
        gen_server:call(ServerName, get_all).

get_last(ServerName) ->
        gen_server:call(ServerName, get_last).




init([]) ->
        {ok, []}.


handle_call({add, Value}, _From , List) ->
        Reply = [Value | List],
        {reply, ok, Reply};

handle_call(get_last , _From , List) ->
        Last = lists:last(List),
        {reply, Last, List};

 handle_call(get_all , _From , List) ->
        {reply, List, List};

 handle_call({get,Index}, _From , List) ->
 		NewList = lists:sublist(Index,length(List)),

        {reply, NewList, List};

handle_call({delete,Value}, _From ,List) -> 
        Reply = [List -- Value],
        {reply, ok, Reply}.

handle_cast(_Info,State) ->
        {noreply, State}.

handle_info(_Info,State) ->
        {noreply, State}.

terminate(_Reason,_State) -> 
        ok.

code_change(_Oldvsn,State,_Extra) ->
        {ok,State}.