-module(playground).
-export([main/0]).

main() ->
    LoopFunction = 
        fun(Number) ->
            io:format("~p~n", [Number]),
            NewNumber = Number + 1,
            fun LoopFunction(NewNumber)
        end,
    LoopFunction(1).
    


