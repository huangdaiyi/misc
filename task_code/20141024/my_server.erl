-module (my_server).
-behaviour (my_behaviour).
-export ([somefunc/1]).

somefunc(Name)->Name.