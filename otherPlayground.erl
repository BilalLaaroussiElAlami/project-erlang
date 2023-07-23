-module(otherPlayground).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, listener/0]).


main() ->
    Number = 1,
    Atom = list_to_atom("s" ++ integer_to_list(Number)),
    Spid = spawn_link(?MODULE, listener , []),
    register(Atom, Spid).

listener() ->
    receive 
        kill -> io:fwrite("killed");
        M -> io:format("message ~p~n", [M]), listener()
        end.
    

