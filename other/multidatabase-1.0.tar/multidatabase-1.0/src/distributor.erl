-module(distributor).
-export ([get/1, post/1,delete/1]).
-define (TABLE, <<"documents">>).

get(["rest", "api", "v1", "user"]) ->
    case dynamodb:get_all(?TABLE) of
        {ok, Items} ->
            {200, [], list_to_binary(rfc4627:encode([{obj, Item} || Item <- Items]))};
        {error, Msg} -> {500, [], list_to_binary(io_lib:format("~p", [Msg]))}
    end;

get(["rest", "api", "v1", "user", UserId]) ->
    case dynamodb:get(?TABLE, UserId) of
        {ok, Columns} -> {200, [], list_to_binary(rfc4627:encode({obj, Columns}))};
        {error, Msg} -> {500, [], list_to_binary(io_lib:format("~p", [Msg]))}
    end;
get(_) ->
    not_match.

post(["rest", "api", "v1", "user", UserId, Database, Region]) ->
    case dynamodb:put(?TABLE, [{id, list_to_binary(UserId)}, {database, list_to_integer(Database)}, {region, list_to_binary(Region)}]) of
        true -> {200, [], []};
        {error, Msg} -> {500, [], list_to_binary(io_lib:format("~p", [Msg]))}
    end;
post(_) ->
    not_match.

delete(["rest", "api", "v1", "user"]) ->
    case dynamodb:clear(?TABLE) of
        true -> {200, [], []};
        {error, Msg} -> {500, [], list_to_binary(io_lib:format("~p", [Msg]))}
    end;
delete(_) ->
    not_match.
