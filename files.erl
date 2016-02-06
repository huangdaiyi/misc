-module (files).
-export ([filelist/1]).

filelist(DirName) ->
	Contents = file:list_dir(DirName),
	{Files, Dirs} = lists:partition(fun(Item) -> filelib:is_file(Item) end ,Contents),
	get_files(Dirs, Files);

get_files([], Acc)-> Acc;


get_files([FirstDir|RestDirs], Acc) ->

	{ok, Contents} = file:list_dir(FirstDir),
	{Files, Dirs} = lists:partition(fun(Item) -> filelib:is_file(Item) end ,Contents),

	NewDirs = lists:map(fun(Item)-> FirstDir++"/"++Item end, Dirs) ++ RestDirs,
	NewFiles = lists:map(fun(Item)-> FirstDir++"/"++Item end, Files),

	get_files(NewDirs, Acc ++ NewFiles).

