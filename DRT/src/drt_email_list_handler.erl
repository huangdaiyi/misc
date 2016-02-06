-module(drt_email_list_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("drt.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{ok, Req3} = echo(Method, Req2),
	{ok, Req3, State}.

echo(<<"GET">>, Req) ->
	{CookieVal, Req2} = cowboy_req:cookie(<<"email">>, Req, <<"">>),
	io:format("Cookie: ~p~n", [binary_to_list(CookieVal)]),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], getBody(getEmailList(), [], binary_to_list(CookieVal)), Req2);
echo(_, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

getEmailList() ->
	DefaultEmail = ["Bill.L.Wang@newegg.com"],
	case application:get_env(drt, email, DefaultEmail) of
		[] -> DefaultEmail;
		Email -> Email
	end.

getBody([], Body, []) -> ["<option value=\"\" disabled selected>Select your email</option>"|Body];
getBody([], Body, _Cookie) -> ["<option value=\"\" disabled>Select your email</option>"|Body];
getBody([Email|Data], Body, Cookie) ->
	case string:equal(Email, Cookie) of
		true ->
			Body1 = "<option value=\""++ Email ++"\" selected>"++ Email ++"</option>",
			getBody(Data, [Body1|Body], Cookie);
		false ->
			Body1 = "<option value=\""++ Email ++"\">"++ Email ++"</option>",
			getBody(Data, [Body1|Body], Cookie)
	end.