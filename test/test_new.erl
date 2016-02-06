-module (test_new).
-export ([new/2, add/2, sub/2]).

new(M, Total) ->
    {?MODULE, M, Total}.

add(Num, {?MODULE, M, Total}) ->
    M:add(Num, Total).

sub(Num, {?MODULE, M, Total}) ->
    M:sub(Total, Num).
