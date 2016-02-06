%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(drt_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Server = {drt_time_server, {drt_time_server, start_link, []}, permanent, 2000, worker, [drt_time_server]},
	Procs = [Server],
	{ok, {{one_for_one, 10, 10}, Procs}}.