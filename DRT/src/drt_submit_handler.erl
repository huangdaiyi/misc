%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(drt_submit_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([multipart/2]).

-include("drt.hrl").

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2, KV} = multipart(Req, []),
	io:format("KV: ~p~n~n", [KV]),
	Data = process(KV, []),
	Email = element(2, lists:keyfind(email, 1, Data)),
	Req3 = cowboy_req:set_resp_cookie(<<"email">>, Email, [{max_age, 7*24*3600}], Req2),
	Ref = drt_db:open("../../report"),
	drt_db:insert(Ref, Data),
	drt_db:close(Ref),
	% drt_time_server:test_send(), 
	Body = <<"Submit successful!">>,
	{ok, Req4} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Body, Req3),
	{ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.

multipart(Req, KV) ->
	case cowboy_req:part(Req) of
		{ok, Headers, Req2} ->
			case cow_multipart:form_data(Headers) of
				{data, FielName} ->
					{ok, Data, Req3} = cowboy_req:part_body(Req2),
					NewKV = [{FielName, Data} | KV],
					multipart(Req3, NewKV)
			end;
		{done, Req2} ->
			{ok, Req2, KV}
	end.

process([], Data) ->
	Data;
process([H|T], Data) ->
	case H of
		{<<"content">>, Content} ->
			NewData = [{content, formatData(Content)} | Data],
			process(T, NewData);
		{<<"issue">>, Issue} ->
			NewData = [{issue, formatData(Issue)} | Data],
			process(T, NewData);
		{<<"next">>, Next} ->
			NewData = [{next, formatData(Next)} | Data],
			process(T, NewData);
		{<<"email">>, Email} ->
			NewData = [{email, binary_to_list(Email)} | Data],
			process(T, NewData);
		_ ->
			process(T, Data)
	end.

formatData(Data) ->
	re:replace(binary_to_list(Data), "\r*\n", "<br>", [global, {return, list}]).