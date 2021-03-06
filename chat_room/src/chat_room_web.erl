%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for chat_room.

-module(chat_room_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        %case Req:get(method) of
        %   Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        %        case Path of
        %            _ ->
        %                Req:serve_file(Path, DocRoot)
        %        end;
        %    'POST' ->
        %        case Path of
        %            _ ->
        %                Req:not_found()
        %        end;
        %    _ ->
        %        Req:respond({501, [], []})
        %end
        %io:format("Req values ~p~n",[Req]),
         case Path of
               "chat" ->
                    io:format("get ...~n",[]) ,
                    chat_handler:process(Req,DocRoot);

               "file" -> 
                io:format("download request!..."),
                file_download:process(Req,DocRoot);  

                _ ->
                   case Req:get(method) of
                        Method when Method =:= 'GET'; Method =:= 'HEAD' ->  
                             Req:serve_file(Path, DocRoot);
                        "POST" ->
                             Req :not_found();
                          _ -> 
                             Req : respond({501, [], []})
                   end  
          end        

    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
