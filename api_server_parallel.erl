-module(api_server_parallel).


get_timelines_parallel(UnisUsernames) ->
    io:format("input get_timelines_parallel~p~n", [UnisUsernames]),
    lists:foreach(
        fun([Uni,Usernames])->
            io:format("Uni: ~p Names:~p~n", [Uni,Usernames]) end,
            lists:foreach(fun(Username) -> ask_timeline(Unu, Username) -> end, Usernames) end,
            UnisUsernames
    ),
    N = lists:sum(lists:map(fun([_,Usernames])-> lists:length(Usernames) end, UnisUsernames)),
    receive_timelines(N).

ask_timeline(Pid, Username) ->
    Pid ! {self(), timeline, Username}


%N is the amount of timelines we need to receive
receive_timelines(N) ->
    receive {_Sender, timeline, _UserName, _TimeLine} ->
        if (N>0) -> receive_timelines(N-1) end
    end.    