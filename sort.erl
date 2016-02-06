-module (sort).
-export ([sort/1]).
-define(RANGE123, 10).
-define(RANGECODE1, 65-58 ).
-define(RANGEABC, 26).
-define(RANGECODE2, 97-91).
-define(RANGE_ABC, 26).
-define(RANGECODE3, 127-123).


sort(InputString) ->
	 io:format("~s",[sort(InputString, [])]).

sort([], Acct) -> Acct;
sort([F | R], Acct) ->
	 sort(R ,[getNewIndexOfChar(F)| Acct]).


getNewIndexOfChar(C) when C < 48; C > 126 ->
    C;
getNewIndexOfChar(C) when C =< 126, C > 122 ->
    C - ?RANGE123 - ?RANGE_ABC - ?RANGEABC;
getNewIndexOfChar(C) when C =< 122, C > 96 -> 
    C  + ?RANGECODE3;
getNewIndexOfChar(C) when C =< 96, C > 90 -> 
    C - ?RANGE123 - ?RANGEABC;
getNewIndexOfChar(C) when C =< 90, C > 64 -> 
    C + ?RANGECODE3 + ?RANGECODE2;
getNewIndexOfChar(C) when C =< 64, C > 57 -> 
    C - ?RANGE123;
getNewIndexOfChar(C) when C =< 57, C > 47 ->
    C + ?RANGECODE3 + ?RANGECODE2 + ?RANGECODE1.

    