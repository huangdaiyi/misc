%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(drt_db).
-export([init/2, open/1, insert/2, close/1]).

init(TableName, Filename) ->
	dets:open_file(TableName, [{type, set}, {file, Filename}]),
	dets:close(TableName),
	ok.

open(Name) ->
	{ok, Ref} = dets:open_file(Name),
	Ref.

insert(Ref, Data) ->
	Email = element(2, lists:keyfind(email, 1, Data)),
	Content = element(2, lists:keyfind(content, 1, Data)),
	Issue = element(2, lists:keyfind(issue, 1, Data)),
	Next = element(2, lists:keyfind(next, 1, Data)),
	dets:insert(Ref, {Email, [{content, Content}, {issue, isnull(Issue)}, {next, isnull(Next)}]}),
	ok.

close(Ref) ->
	dets:close(Ref),
	ok.

isnull(Element) ->
	case Element of
		[] -> "N/A";
		_ -> Element
	end.