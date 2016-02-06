-module (file_tar_init).
-export ([start_tar/1]).

get_files(DirName) ->
	{ok, Contents} = file:list_dir(DirName),
	NewContents = lists:map(fun
		(Item) ->
			DirName++"/"++Item
	end,Contents),
	{Dirs, Files} = lists:partition(fun(Item) -> filelib:is_dir(Item) end ,NewContents),
	get_files(Dirs, Files).

get_files([], Acc)  -> Acc;

get_files([FirstDir|RestDirs], Acc) ->
	{ok, Contents} = file:list_dir(FirstDir),
	NewContents = lists:map(fun
		(Item) ->
			FirstDir++"/"++Item
	end,Contents),

	{Dirs, Files} = lists:partition(fun(Item) -> filelib:is_dir(Item) end ,NewContents),
	get_files(Dirs++RestDirs, Acc ++ Files).


start_tar(DirName) ->
	{ok,Fd} = file:open("D:/test/1.tar", [write,binary]),
	Files = get_files("D:/test/1"),
	 ExampleFun = 
	     fun(write, {FHandler,Data}) ->  file:write(FHandler, Data);
	        (position, {FHandler,Pos}) -> file:position(FHandler, Pos);
	        (close, FHandler) -> file:close(FHandler)
	     end,
	{ok,TarDesc} = erl_tar:init(Fd, [write], ExampleFun),
	lists:foreach(fun(File)->
		erl_tar:add(TarDesc, File, File)
	end, Files),
	ok = erl_tar:close(TarDesc).
	%%tar(FileList, IO, Name).


tar([] , IO, _FileName) -> erl_tar:close(IO);
tar([F | R], IO, FileName) ->
	erl_tar:add(IO,F,[verbose]),
	tar(R, flush(IO,FileName) ,FileName).



flush(IO,FileName) ->	
	erl_tar:close(IO),
	case erl_tar:open(FileName, [write,compressed]) of
	 	{ok, NewIO}  ->
	 		NewIO;
	 	_ ->
	 		error
	 end .
