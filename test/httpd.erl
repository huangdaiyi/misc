-module(httpd).

-export([start_link/1]).
-export([init/1]).


% %%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
% %%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Port) ->
    Opts = [binary,
        {reuseaddr, true},
        {packet, 0},
        {backlog, 8},
        {recbuf, 8192},
        {exit_on_close, false},
        {active, false},
        inet,
        {ip, {127, 0, 0, 1}}],
    {ok, Socket} = gen_tcp:listen(Port, Opts),
    proc_lib:spawn_link(?MODULE, init, [Socket]).

init(Socket)->
    case catch gen_tcp:accept(Socket) of
        {ok, AcceptSocket} ->
            gen_tcp:close(Socket),
            loop(AcceptSocket);
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            init(Socket);
        {error, esslaccept} ->
            exit(normal);
        Other ->
            exit({error, accept_failed})
    end.

loopN(Socket) ->
    ok = inet:setopts(Socket, [{packet, raw}]),
    ok = inet:setopts(Socket, [{active, false}]),
    {ok, Data} = gen_tcp:recv(Socket, 1024 *1024, 5000),
    io:format("Content : ~p~n", [Data]),
    gen_tcp:close(Socket).

loop(Socket) ->
    ok = inet:setopts(Socket, [{packet, http}]),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
         {Protocol, _, {http_request, Method, Path, Version}} ->
            ok = inet:setopts(Socket, [{packet, httph}]),
            headers(Socket, {Method, Path, Version}, [], 0);
        _ ->
            gen_tcp:close(Socket),
            exit(normal)
    after 120000 ->
        gen_tcp:close(Socket),
        exit(normal)
    end.

headers(Socket, Request, Headers, 1000) ->
    exit(normal);
headers(Socket, Request, Headers, HeaderCount) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {http, _, http_eoh} ->
            io:format("headers ~p ~n", [Headers]),
            ok = inet:setopts(Socket, [{packet, raw}]),
            io:format("~p~n", [inet:getopts(Socket, [packet, active])]),
            % ok = inet:setopts(Socket, [{active, false}]),
            {ok, Data} = gen_tcp:recv(Socket, 10, 5000),
            % io:format("Content : ~p~n", [Data]),
            ok = gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\nContent-Length:0\r\n\r\n">>),
            gen_tcp:close(Socket);
        {http, _, {http_header, _, Name, _, Value}} ->
            headers(Socket, Request, [{Name, Value}|Headers], 1 + HeaderCount);
        {tcp_closed, _} ->
            gen_tcp:close(Socket),
            exit(normal);
        Other ->
            gen_tcp:close(Socket),
            exit(normal)
    after 120000 ->
            gen_tcp:close(Socket)
    end.



