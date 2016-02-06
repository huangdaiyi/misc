-module (zipfile).
-export ([zip_start/2]).
-include_lib("kernel/include/file.hrl").

filelist(DirName) ->
	{ok, Contents} = file:list_dir(DirName),
	NewContents = lists:map(fun
		(Item) ->
			DirName++"/"++Item
	end,Contents),
	%io:format("~p~n",[NewContents]),
	{Dirs, Files} = lists:partition(fun(Item) -> filelib:is_dir(Item) end ,NewContents),
	%io:format("~p~n",[Dirs]),
	%io:format("~p~n",[Files]),
	get_files(Dirs, Files).

get_files([], Acc)  -> Acc;


get_files([FirstDir|RestDirs], Acc) ->
	%io:format("~p~n",[FirstDir]),	
	{ok, Contents} = file:list_dir(FirstDir),
	NewContents = lists:map(fun
		(Item) ->
			FirstDir++"/"++Item
	end,Contents),

	{Dirs, Files} = lists:partition(fun(Item) -> filelib:is_dir(Item) end ,NewContents),

	get_files(Dirs++RestDirs, Acc ++ Files).


zipfile()->
	receive
		{start,{From, FileName, FileList}}->
			    io:format("zipfile!!!!!!~p~n",[From]),
				From ! ok,
				%closed
			    zip:create(FileName,FileList),
				io:format("zipfile  end!!!!!!~p~n",[From]),
				From ! ended,
				zipfile();

		 {is_end,From} -> 
		 	From ! ended,
		 	zipfile();
		 %{get_file,FileName,From} ->
		 		
		 _ ->
		  nothing
		 	
	end.
	

zip_start(FileName, DirName) ->
	FileList = filelist(DirName),
	Pid = spawn(fun() ->
                zipfile()
            end),
	Pid ! {start ,{self(), FileName, FileList}},
	receive
		ok ->
			io:format("begin read file!!!!!!~n"),
			do_filehandler(FileName,Pid)
		  	%{ok, FileHandler} = file:open(FileName,[read]),
	end.

do_filehandler(FileName,Pid) ->
	%Pid!{get_file, FileName, self()},
	case filelib:is_regular(FileName) of 
		       true ->
			 	   	{ok,FileHandler} = file:open(FileName,[read]),
			 	   	readfile(FileHandler,FileName,Pid);
			 	false ->
			 	     do_filehandler(FileName,Pid) 
                 end.

		



readfile(FileHandler,FileName, Pid) ->
	io:format("read!~n"),	
	%Pid ! {is_end,self()},
	receive
		  	ended -> 
		  		io:format("zip end!~n"),
		  		Pid ! exit,
		  		read_no_wait(FileHandler,FileName)
	after 100 ->
		  	{ok, Facts} =file:read_file_info(FileName),
			io:format("file size ~p~n",[Facts#file_info.size]),
			Data = file:read(FileHandler,1024*1024*10),
			case Data of
				{ok,Bin} ->
					io:format("read wait length ~p~n",[length(Bin)]),
					readfile(FileHandler,FileName,Pid);
					eof ->
					io:format("read wait eof")
			end
	end.
	
	
read_no_wait(FileHandler,FileName) ->

	{ok, Facts} =file:read_file_info(FileName),
	io:format("file size ~p~n",[Facts#file_info.size]),
	Data = file:read(FileHandler,1024*1024*10),
	case Data of
		{ok,Bin} ->
			io:format("read_no_wait length ~p~n",[length(Bin)]),
			read_no_wait(FileHandler,FileName);
			eof ->
			io:format("read_no_wait file eof")
	end.


%process(Req, DocRoot) ->
   %Req:respond(),
%  ok.