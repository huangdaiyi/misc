-module (file_tar).
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
	FileList = get_files(DirName),
	Name = "D:/test.tar.gz",
	{ok,IO} = erl_tar:open(Name , [write,compressed]),
	tar(FileList, IO, Name).


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

	
