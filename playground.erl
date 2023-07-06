-module(playground).
-export([main/0, group_by_spid/1]).

main() ->
    Followers = sets:from_list([["A", 2], ["C", 3] ,["B", 2], ["D", 3]]),
    Grouped = group_by_spid(Followers),
    lists:foreach(fun({Spid, Usernames}) -> 
        io:format("Spid ~p\nCorrespinding UserNames ~p \n", [Spid,Usernames])
        end, Grouped).


%!!!!!!!!!!!!!!!!!!!!!!!!!KEEP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%Groups a set of users Set of [Username, Spid], returns a list of tuples {Spid, Usernames} 
group_by_spid(Users) ->
    UsersGrouped = lists:foldl(
        fun([Username, Spid], Acc) ->
            case lists:keysearch(Spid, 1, Acc) of
                {value, {Spid, Usernames}} -> [{Spid, [Username | Usernames]} | lists:keydelete(Spid, 1, Acc)];
                false -> [{Spid, [Username]} | Acc]
            end
        end,
        [],
        sets:to_list(Users)),
    UsersGrouped.