-module (test).
-export ([add/1,delete/1,show/0,init/0]).

loop(Lists) ->
  receive
    {add,_From, Obj} -> 
	     _From ! added,
	     loop([ Obj | Lists] );

    {delete,_From,Obj} ->
	      _From ! deleted,	
	      loop(Lists --[Obj]);
    {show,_From}  ->
          io:format("current list:~p~n",Lists),
          loop(Lists)
    end.
    

add(Obj) ->
 	Pid = init(),
    Pid ! {add,self(), Obj},
    receive
      added -> ok;
      _ -> fail
    end.


delete(Obj) -> 
	Pid = init(),
    Pid ! {delete,self(),Obj},
    receive
    	deleted -> ok;
    	_ -> fail
    end.


show() ->
	Pid = init(),
	Pid ! {show,self()}. 


init() ->
  Pid = whereis(thelop),
  if is_pid(Pid) ->
  	 	Pid;
     true -> 
     	NewPid = spawn(fun() ->  loop([]) end),
	    register(thelop,NewPid ),
	    NewPid
 end.

     
