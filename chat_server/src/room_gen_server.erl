-module (room_gen_server).

-behaviour (gen_server).

-include ("chat_server_config.hrl").

%% API
-export ([stroe/2,logoff/1,get_user/1,start_link/0]).

%% gen_server callbacks
-export ([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stroe(Key,User)->
        gen_server:call(?SERVER,{stroe, Key,User}).

logoff(Key) ->
        gen_server:cast(?SERVER,{logoff, Key}).

get_user(Key) ->
        gen_server:call(?SERVER,{get_user,Key}).    

init([]) ->
        {ok, dict:new()}.


handle_call({stroe, Key, User}, _From , Dict) ->
         Reply = dict:stroe(Key,User,Dict),
        {reply, ok, Reply};
handle_call({get_user, Key},_From,Dict) ->
         User = dict:find(Key,Dict),
         {reply,User,Dict}.
        


handle_cast({logoff,Key},Dict) ->
		Reply = Dict:erase(Key,Dict),
        {noreply, Reply};
handle_cast(_Info,Dict) ->
        {noreply, Dict}.

handle_info(_Info,State) ->
        {noreply, State}.

terminate(_Reason,_State) -> 
        ok.

code_change(_Oldvsn,State,_Extra) ->
        {ok,State}.