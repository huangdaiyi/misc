-module (zipfile).
-behaviour (gen_server).

%API
-export ([	get_files/0,
			start_link/0,
			fill_list/1
		]).

-export ([  init/1,
			handle_call/3,
			handle_cast/2,
			handle_info/2,
			terminate/2,
			code_change/3
			]).
-define (SERVER,?MODULE).

start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).


fill_list(DirName) ->

	{ok,Files} = file:list_dir(DirName),
	io:format("~p~n",[DirName]),

	io:format("~p~n",[Files]),
	NewDirs = lists:filter(fun(Item) -> filelib:is_dir(Item) end, Files),

	lists:foreach(fun(Item) ->
		 			   append(fill_list(DirName++"/"++Item)) 
		 		 end, NewDirs),
	NewFiles = lists:filter(fun(Item) -> filelib:is_file(Item) end, Files),
	append(NewFiles),
	NewFiles.

append(FileList) ->
	gen_server:call(?SERVER, {append, FileList}).

get_files()	->
	Files = gen_server:call(?SERVER, get),
	clear(),
	Files.

clear() ->
	gen_server:call(?SERVER,clear).

init([]) ->
	{ok, []}.

handle_call({append,FileList}, _From, State) ->
	NewState = State ++ FileList,
	{reply, ok, NewState};
handle_call(get, _From, State) ->
	{reply, State, State};
handle_call(clear, _From, _State) ->
	{reply, ok, []}.

handle_cast(_Msg,State)->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


