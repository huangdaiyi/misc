-module (file_download).
-export ([process/2]).
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
			    zip:zip(FileName,FileList),
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
	



do_filehandler(FileName,Pid, Response) ->
	%Pid!{get_file, FileName, self()},
	case filelib:is_regular(FileName) of 
		       true ->
			 	   	{ok,FileHandler} = file:open(FileName,[read,binary]),
			 	   	readfile(FileHandler,FileName,Pid,Response);
			 	false ->
			 	     do_filehandler(FileName,Pid,Response) 
                 end.



readfile(FileHandler,FileName, Pid, Response) ->
	io:format("read!~n"),	
	%Pid ! {is_end,self()},
	receive
		  	ended -> 
		  		io:format("zip end!~n"),
		  		%Pid ! exit
		  		read_no_wait(FileHandler,FileName,Response)
	after 100 ->
		  	{ok, Facts} =file:read_file_info(FileName),
			io:format("file size ~p~n",[Facts#file_info.size]),
			Data = file:read(FileHandler,1024*1024*1),
			case Data of
				{ok,Bin} ->
					io:format("read wait length ~p~n",[byte_size(Bin)]),
					Response:write_chunk(Bin),
					readfile(FileHandler,FileName,Pid,Response);
					eof ->
					io:format("read wait eof"),
					Response:write_chunk(<<>>)
			end
	end.
	
	
read_no_wait(FileHandler,FileName,Response) ->

	{ok, Facts} =file:read_file_info(FileName),
	io:format("file size ~p~n",[Facts#file_info.size]),
	Data = file:read(FileHandler,1024*1024*1),
	case Data of
		{ok,Bin} ->
			io:format("read_no_wait length ~p~n",[byte_size(Bin)]),
			Response:write_chunk(Bin),
			read_no_wait(FileHandler,FileName,Response);
			eof ->
			io:format("read_no_wait file eof"),
			Response:write_chunk(<<>>)
	end.




process(Req, _DocRoot) ->
	Response = Req:respond({200,
		[{'Content-Type','application/octet-stream'},{'Content-Disposition', 'attachment;filename=read.zip'}],chunked}),
	DirName = "src",
	FileName = "./ziptest.zip",
    FileList = filelist(DirName),
	Pid = spawn(fun() ->
                zipfile()
            end),
	Pid ! {start ,{self(), FileName, FileList}},
	receive
		ok ->
			io:format("begin read file!!!!!!~n"),
			do_filehandler(FileName,Pid, Response)
		  	%{ok, FileHandler} = file:open(FileName,[read]),
	end.
  