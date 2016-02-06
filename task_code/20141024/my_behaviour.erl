-module (my_behaviour).
-export ([bingo/1]).
-export ([behaviour_info/1]).


behaviour_info(callbacks)->[{somefunc,1}];
behaviour_info(_Other) ->  undefined.  
  
bingo(Mod) -> Mod:somefunc(ok). 




