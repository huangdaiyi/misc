
-module (chat_handler).
-export ([process/2]).

-define(TIMEOUT, 10000).

room(Users) ->
    receive
        {From, subscribe} ->
            From ! subscribed,
            io:format("~p~n",[[From | Users]]) ,
            room([From | Users]);
        {From, unsubscribe} ->
            From ! unsubscribed,
            io:format("~p~n",[Users -- [From]]) ,
            room(Users -- [From]);
        {From, post, Message} ->
            From ! posted,
            lists:foreach(fun(User) ->
                                                % broadcast the message
                                  User ! Message
                          end, Users),

            %% 所有等待的用户都会接收的消息，
            %% 从队列中去掉他们,避免过多的进程
            io:format("~n",[]),
             room([]);
        _Any ->
            room(Users)
    end.

get_the_room() ->
    Pid = whereis(theroom),
    if
        is_pid(Pid) ->
            io:format("registered  proccess ~p~n",[Pid]),
            Pid;
        true ->
            NewPid = spawn(fun() ->
                room([])
            end),
            register(theroom, NewPid),
              io:format("registering  proccess ~p~n",[NewPid]),
            NewPid
    end.

process(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "chat" ->
                    io:format("get ...~n",[]) ,
                    Room = get_the_room(),
                    Room ! {self(), subscribe},
                    receive
                        subscribed ->
                            receive
                                Message ->
                                    {Type, Message} = {ok, Message}
                            after ?TIMEOUT ->
                                % we waited too long
                                {Type, Message} = {error, <<"no message">>}
                            end
                    after 1000 ->
                        % subscription failed on time
                        {Type, Message} = {error, <<"subscribed timeout">>}
                    end,

                    case Type of
                        error ->
                            % we need to unsubscribe from the room
                            % because it failed somewhere
                            Room ! {self(), unsubscribe},
                            receive
                                unsubscribed ->
                                    % unsubscribed
                                    ok
                            after 1000 ->
                                % do not wait too long
                                ok
                            end;
                        ok ->
                            % everything went fine
                            ok
                    end,

                    % send back the JSON message
                    Req:ok({"application/json", mochijson2:encode({
                            struct, [
                                 {"type" ,Type}, {"message", Message}
                            ]
                        })
                    });

                 _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                "chat" ->
                    Data = Req:parse_post(),
                    Room = get_the_room(),
                    % post
                    io:format("message from post:~p~n",[proplists:get_value("message", Data)]) ,
                    Room ! {self(), post, list_to_binary(proplists:get_value("message", Data))},
                    receive
                        posted ->
                            % posted
                            Body = {ok, <<"posted">>}
                    after 1000 ->
                        % something went wrong
                        Body = {error, <<"timeout">>}
                    end,

                    % send back the JSON message
                    Req:ok({"application/json", mochijson2:encode({
                            struct, [
                                Body
                            ]
                        })
                    });
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.