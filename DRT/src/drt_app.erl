%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(drt_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
				{"/", cowboy_static, {priv_file, drt, "index.html"}},
				{"/css/[...]", cowboy_static, {priv_dir, drt, "css"}},
				{"/js/[...]", cowboy_static, {priv_dir, drt, "js"}},
				{"/fonts/[...]", cowboy_static, {priv_dir, drt, "fonts"}},
				{"/submit", drt_submit_handler, []},
				{"/email_list", drt_email_list_handler, []}
				]}
		]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
			{env, [{dispatch, Dispatch}]}
		]),
	drt_db:init(report, "../../report"), %% init a report dets.
	drt_sup:start_link().

stop(_State) ->
	ok.
