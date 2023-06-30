-module(playground).
-export([main/0]).

double(X) ->
    X*2.
main() ->
    io:fwrite("~p", [lists:map(fun double/1, [1,2,3])]).



