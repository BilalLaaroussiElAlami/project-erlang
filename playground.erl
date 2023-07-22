-module(playground).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, split/2, make_list/3]).

main() ->
    Split = split(100,10),
    io:format(" result ~p\n", [Split]),
    Sum41TO50 = lists:foldl(fun(N, Acc) -> N + Acc end, 0,  lists:nth(2,(lists:nth(5, Split)))),
    io:format(" sum 41 to 50 ~p\n", [Sum41TO50]).


split(N_users,N_servers) ->
    Part = N_users div N_servers,
    Servers = lists:seq(1, N_servers),
    Result = lists:map(
        fun(Server) ->
            Begin = (Server-1)*Part + 1,
            End   = Server*Part,
            io:format("begin: ~p end: ~p\n",[Begin,End]),
            [Server, lists:seq(round(Begin), round(End))] end,
        Servers
    ),
    Result.

make_list(Start,Stop, Acc) ->
    if
    (Start == Stop) -> Acc;
    true -> make_list(Start+1,Stop, [Start | Acc]) end.


