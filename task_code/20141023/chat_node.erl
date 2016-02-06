-module (chat_node).
-export ([init/0]).

init()->
 Name =string:strip(io:get_line("please input center node name:"),right,$\n),
 Ret= net_adm:ping(list_to_atom(Name)),
if
	Ret =:= pong ->
		io:format("connected!~n",[]),
        Pid = spawn(fun receive_msg/0),
		register(shell,Pid),
 	    UserName = string:strip(io:get_line("please input a user name:"),right,$\n),
 	    io:format("your username is ~p~n",[UserName]),
 	    send_msg(UserName);
    Ret =:= pang ->
       io:format("fail, please try again !~n",[]),
       	init()
end.

	
receive_msg() ->
	receive 
	  {From,Msg,SendUser} ->
	    io:format("~p ~p's message: ~p ~n",[From,SendUser,Msg]),
	    receive_msg()
	 end.


send_msg(UserName) ->
Msg =string:strip(io:get_line(""),right,$\n),
lists:foreach(
	fun(Node)-> {shell,Node}!{self(),Msg,UserName}
	 end,
	nodes()),
  io:format("your message:~p~n",[Msg]),
send_msg(UserName).
