-module(playground).
-export([main/0]).

main() ->
    X = 
        if(false) -> 10;
    true -> 20 end,
    io:fwrite("X is ~p~n", [X]).


