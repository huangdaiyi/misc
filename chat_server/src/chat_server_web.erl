-module(chat_server_web).
-author('Hardy.D.Huang@newegg.com').

-export([start/1, stop/0, loop/2]).

-include ("chat_server_config.hrl").

%% External API
% Options Vaules: [{ip,{0,0,0,0}},{port,8080},{docroot,"d:/erlang/chat_server/priv/www"}]
start(Options) ->
    io:format("start Options ~p~n",[Options]),
    %State = room_gen_server:start_link(),  
    %io:format("room_gen_server State ~p~n",[State]),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,

    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).



loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
     io:format("~p~n",[Req]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            Query = Req:parse_qs(),
            Username = proplists:get_value("username",Query ,"Anonymous"),
            case Path of
                "chat" ->
                    
                   %%#chatinfo{message=Message,from=From} = get_last(),

                    %%chat_gen_server:
                    case  room_gen_server:get_user(Username) of
                        error ->
                             {Type,Message}={error,<<"not logon">>};
                        #userinfo{name=Name,mess_index=Index}  ->
                             {Type, Message} = chat_gen_server:get(Index,Name),
                             room_gen_server:stroe(Username, #userinfo{name=Name,mess_index=Index+length(Message)})
                    end,
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
            Data = Req:parse_post(),
            Username = proplists:get_value("username",Data ,"Anonymous"),
            case Path of
                "chat" ->
                      io:format("post data,~p~n",[Data]),
                    % print send message
                    % io:format("message from post:~p~n",[proplists:get_value("message", Data)]),
                    %?CHATROOM ! {self(), send, list_to_binary(proplists:get_value("message", Data))},
                    Body = chat_gen_server:add(#chatinfo{message=proplists:get_value("message", Data),from=Username},Username),
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                Body
                            ]
                        })
                    });
                 "logon" -> 
                    io:format("logon,~p~n",[Username]),
                    Stroe = room_gen_server:stroe(Username, #userinfo{name=Username,mess_index=1}) ,
                    io:format("logon,~p~n",[Stroe]),
                    case Stroe of
                          ok  -> 
                             case chat_gen_server:start_link(Username) of
                                 {ok,_}  ->
                                  State  = ok;
                                    _ ->
                                  State =  error
                             end;
                            _ ->
                              State = error 
                    end,

                    Req:ok({"application/json", mochijson2:encode({
                                struct, [
                                   State
                                ]
                            })
                    });
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


