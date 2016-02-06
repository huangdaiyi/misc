-module (tut7).
-export ([reverse/1]).

reverse(List) ->
	reverse(List,[]).

reverse([Head|Rest],NewList) ->
	reverse(Rest,[Head|NewList]);
	reverse([],NewList) ->
	NewList.