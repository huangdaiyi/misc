%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the sqsdemo application.

-module(sqsdemo_sup).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    SQS = {sqs, {sqs, start_link, []}, permanent, brutal_kill, worker, [sqs]},
    DynamonDB = {dynamodb, {dynamodb, start_link, []}, permanent, brutal_kill, worker, [dynamodb]},
    SSS = {s3demo, {s3demo, start_link, []}, permanent, brutal_kill, worker, [s3demo]},
    Processes = [SQS, DynamonDB, SSS],
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, lists:flatten(Processes)}}.

% web_specs(Mod, Port) ->
%     WebConfig = [{ip, {0,0,0,0}},
%                  {port, Port},
%                  {docroot, sqsdemo_deps:local_path(["priv", "www"])}],
%     {Mod,
%      {Mod, start, [WebConfig]},
%      permanent, 5000, worker, dynamic}.
