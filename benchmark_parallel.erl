-module(benchmark_parallel).

-export([test_timeline/0,  pick_random_n/2, split/2, initialize_parallel_server/4,
     test_server_initialization/0, test_send_message_bilal/0,receive_timelines/1, test_timeline_sequential/0, test_timeline_parallel/0]).


%% Benchmark helpers
% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(fun (N) ->
        % Recommendation: to make the test fair, each run executes in its own,
        % newly created Erlang process. Otherwise, if all tests run in the same
        % process, the later tests start out with larger heap sizes and
        % therefore probably do fewer garbage collections. Also consider
        % restarting the Erlang emulator between each test.
        % Source: http://erlang.org/doc/efficiency_guide/profiling.html
        spawn_link(fun () ->
            run_benchmark_once(Name, Fun, N),
            ThisPid ! done
        end),
        receive done ->
            ok
        end
    end, lists:seq(1, Times)).

run_benchmark_once(Name, Fun, N) ->
    io:format("Starting benchmark ~s: ~p~n", [Name, N]),

    % Start timers
    % Tips:
    % * Wall clock time measures the actual time spent on the benchmark.
    %   I/O, swapping, and other activities in the operating system kernel are
    %   included in the measurements. This can lead to larger variations.
    %   os:timestamp() is more precise (microseconds) than
    %   statistics(wall_clock) (milliseconds)
    % * CPU time measures the actual time spent on this program, summed for all
    %   threads. Time spent in the operating system kernel (such as swapping and
    %   I/O) is not included. This leads to smaller variations but is
    %   misleading.
    StartTime = os:timestamp(), % Wall clock time
    %statistics(runtime),       % CPU time, summed for all threads

    % Run
    Fun(),

    % Get and print statistics
    % Recommendation [1]:
    % The granularity of both measurement types can be high. Therefore, ensure
    % that each individual measurement lasts for at least several seconds.
    % [1] http://erlang.org/doc/efficiency_guide/profiling.html
    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    %{_, CpuTime} = statistics(runtime),
    io:format("Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
    %io:format("CPU time = ~p ms~n", [CpuTime]),
    io:format("~s done~n", [Name]).


% Note that this code depends on the implementation of the server. You will need to
% change it if you change the representation of the data in the server.
initialize_central_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages) ->
    % Seed random number generator to get reproducible results.
    rand:seed_s(exsplus, {0, 0, 0}),
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n", [NumberOfUsers]),
    io:format("Number of subscriptions: ~p~n", [NumberOfSubscriptions]),
    io:format("Number of messages: ~p~n", [NumberOfMessages]),
    io:format("~n"),
    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    % Generate users dict.
    Users = dict:from_list(lists:map(fun (Name) ->
        % Random subscriptions.
        Subscriptions = [pick_random(UserNames) || _ <- lists:seq(1, NumberOfSubscriptions)],
        % Random messages.
        Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],
        User = {user, Name, sets:from_list(Subscriptions), Messages},
        {Name, User} % {key, value} for dict.
        end,
        UserNames)),
    ServerPid = server_centralized:initialize_with(Users),
    {ServerPid, UserNames}.


%---------------------------------BEGIN CODE PARALLEL INITIALISATION------------------------------------------
initialize_parallel_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages, NumberOfServers) ->
    % Seed random number generator to get reproducible results.
    rand:seed_s(exsplus, {0, 0, 0}),
    NUmberOfUsersPerServer = NumberOfUsers div NumberOfServers,
    % Users follow evenly over servers
    NumberOfSubscriptionsPerServer = NumberOfSubscriptions div NumberOfServers,
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n",             [NumberOfUsers]), 
    io:format("Number of servers: ~p~n",           [NumberOfServers]),
    io:format("Number of users per server : ~p~n", [NUmberOfUsersPerServer]),
    io:format("Number of subscriptions: ~p~n",     [NumberOfSubscriptions]),
    io:format("Number of subscriptions per server: ~p~n", [NumberOfSubscriptionsPerServer]),
    io:format("Number of messages: ~p~n",          [NumberOfMessages]),
    UnisUserNames = split(NumberOfUsers,NumberOfServers),
    server_parallel:initialize_alternative(UnisUserNames),
    everyone_follow(UnisUserNames, NumberOfSubscriptionsPerServer),
    send_messages(UnisUserNames, NumberOfMessages),
    timer:sleep(3000), %wait for the messages to arrive at recipients
    io:fwrite("finished initialisation benchmark tests can start ~n"),
    UnisUserNames.

%All users in all unis follow N random users per each uni
everyone_follow(UnisUsers, N) ->
    lists:foreach(
        fun(UniUsers) ->
            everyone_in_uni_follows(UniUsers, UnisUsers, N) end, 
        UnisUsers
    ).

%All users in one uni follow N random users per each uni        
everyone_in_uni_follows(UniUsers, UnisUsers, N) ->
    [Uni,Users] = UniUsers,
    lists:foreach( 
            fun(User) ->
                user_follow_random_n(Uni, User, UnisUsers, N) end,
            Users
    ). 
        
%One user in one uni follow N random users per each uni 
user_follow_random_n(PidFollower, UsernameFollower, UnisUsers, N) ->
    FolloweesPerUni = pick_random_users_over_unis(UnisUsers, N), 
    lists:foreach(  %iterate over unis
        fun(FolloweesUni) ->
            [Uni, Followees] = FolloweesUni,
            lists:foreach( 
                fun(Followee) ->
                    follow(PidFollower,UsernameFollower,Followee,Uni) end,
                    Followees
                )
            end,
            FolloweesPerUni
    ).

%picks random N Users per Uni, each associated with the uni 
pick_random_users_over_unis(UnisUsers, N) ->
    lists:map(
        fun(UniUsers) -> 
            [Uni, Users] = UniUsers,
            [Uni, pick_random_n(Users,N)] end,
            UnisUsers).

follow(PidFollower, UsernameFollower, UserNameFollowee, PidFollowee) ->
    %vub ! {self(), follow, "Alice", "Bob", vub},
    PidFollower ! {self(), follow, UsernameFollower, UserNameFollowee, PidFollowee}.
   
%assumes N_Users are evenly dividable betweeb N_servers
%returns unique serverids associated with unique usernames
%example  split(6,3) return [[1, [1,2]], [2, [3,4]], [3, [5,6]]
split(N_users,N_servers) ->
    Part = N_users div N_servers,
    Servers = lists:seq(1, N_servers),
    Result = lists:map(
        fun(Server) ->
            Begin = (Server-1)*Part + 1,
            End   = Server*Part,
            ServerActor = list_to_atom("s" ++ integer_to_list(Server)),
            [ServerActor, lists:seq(Begin, End)] end,
        Servers
    ),
    Result.

%This function will make every user send N messages
% To do to wait for confirmation for request otherwise not useful for benchmarking
send_messages(UnisUsernames, N_messages) ->
    lists:foreach(
        fun(UniUserNames) ->
            [Uni, UserNames] = UniUserNames,
            lists:foreach(
                fun(UserName) ->
                    lists:foreach(
                        fun(I) ->
                            Message = generate_message(Uni,UserName,I),
                            send_message(Uni,UserName,Message) end,
                            lists:seq(1,N_messages))
                end,
                UserNames
            )end,
            UnisUsernames
    ).

send_message(ServerPid, UserName, MessageText) ->
    ServerPid ! {self(), send_message, UserName, MessageText, os:system_time()}.

% Pick a random element from a list.
pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).

% Pick n random elements from list
pick_random_n(List, N) ->
    [pick_random(List) || _ <- lists:seq(1, N)].

% Generate a random message `I` for `UserName`.
generate_message(ServerPid, UserName, I) ->
    %io:format("ServerPid ~p UserName ~p I ~p ~n", [ServerPid,UserName,I]),
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName, %++ "located at" ++ ServerPid,
    "blablabla".
    %Text.

% Generate a random message `I` for `UserName`.
generate_message(UserName, I) ->
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

%--------------------------------- END CODE PARALLEL INITIALISATION -----------------------------------------

%----------------------------------EXPERIMENTS---------------------------------------------------------------
%--------------------------------- EXPERIMENT 1 -------------------------------------------------------------
%GOAL: measure the latency of send_message depending of the amount of erlang threads
test_send_message_bilal() ->
    NumberOfUsers = 10000, 
    NumberOfSubscriptions = 1,
    NumberOfMessages = 5,
    NumberOfServers = 5,
    UnisUserNames = initialize_parallel_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages, NumberOfServers),
    MessagesSentPerUser = 5,
    run_benchmark("send_message",
        fun () ->
            send_messages(UnisUserNames, MessagesSentPerUser)
        end,
        30).

%--------------------------------EXPERIMENT 2 & 3----------------------------------------------------------

-define(TestTimelineNumberOfUsers, 10000).
-define(TestTimelineNumberOfSubscriptions, 5).
-define(TestTimelineNumberOfMessages, 3).
-define(TestTimelineNumberOfServers, 3).

test_timeline() ->  "sequential and parallel tests are seperate functions".

test_timeline_sequential() ->
    test_timeline_sequential(?TestTimelineNumberOfUsers, ?TestTimelineNumberOfSubscriptions,?TestTimelineNumberOfMessages).

test_timeline_parallel() ->
    test_timeline_parallel(?TestTimelineNumberOfUsers, ?TestTimelineNumberOfSubscriptions,?TestTimelineNumberOfMessages, ?TestTimelineNumberOfServers).

% Get timeline of 10000 random users (repeated 30 times).
test_timeline_sequential(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages) ->
    {ServerPid, UserName} = initialize_central_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages),
    run_benchmark("testing timeline sequential implementation",
        fun () ->
            lists:foreach(fun (_) ->
                server:get_timeline(ServerPid, pick_random(UserName))
            end,
            lists:seq(1, 10000))
        end,
        30).

test_timeline_parallel(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages, NumberOfServers) ->
    UnisUserNames = initialize_parallel_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages, NumberOfServers),
    run_benchmark("testing timeline parallel implementation",
        fun () ->
            %lists:foreach(fun (UnisRandomSelectionUsernames) ->
            %    get_timelines_parallel(UnisRandomSelectionUsernames) end, %why did i do a randol selection
            %    pick_random_users_over_unis(UnisUserNames, NumberOfUsers div lists:length(UnisUserNames))) %this are just all users/ doesnt do anything?
                get_timelines_parallel(UnisUserNames) 
        end,
        30).


%sequentially, but just sends messages.
get_timelines_parallel(UnisUsernames) ->
    lists:foreach(
        fun([Uni,Usernames])->
            lists:foreach(fun(Username) -> Uni ! {self(), timeline, Username} end, Usernames) end,
        UnisUsernames
    ),
    N = lists:sum(lists:map(fun([_,Usernames])-> length(Usernames) end, UnisUsernames)),
    receive_timelines(N).

%N is the amount of timelines we need to receive
%Sequentially waits on messages. This function would take as much time as if we would do it in parallel because even in parallel we'd need to wait on the slowest receive.
receive_timelines(0) ->
    io:format("received all timelines~n");

receive_timelines(1) ->
    receive
    {_Sender, timeline, UserName, TimeLine} ->
        %io:format("timeline of one person ~p ~n ~p", [UserName, TimeLine]),
        receive_timelines(0)
    end;

receive_timelines(N) ->
    receive
        {_Sender, timeline, _UserName, TimeLine} ->
            receive_timelines(N - 1)
    end.

%--------------------TESTING INITIALISATION-----------------------
%This function tests t initialize_parallel_server function  because the initialization is a lot of steps 
%This is not a benchmarking test.
test_server_initialization() ->
    NumberOfUsers = 500, 
    NumberOfSubscriptions = 25,
    NumberOfMessages = 5,
    NumberOfServers = 5,
    initialize_parallel_server(NumberOfUsers,NumberOfSubscriptions,NumberOfMessages,NumberOfServers),  
    io:fwrite("messages ~n"),
    s1 ! {self(), ping},
    print_all_messages(s1),
    get_users(s1).
   

print_all_messages(SPID) ->
    receive 
        X -> io:format("got ~p~n", [X]),  print_all_messages(SPID)
        after timer:seconds(1) -> io:fwrite("received all messages ~n")
    end.

get_users(ServerPid) -> 
    io:fwrite("~p is at ~p~n", [ServerPid, whereis(ServerPid)]),
    ServerPid ! {self(), users},
    receive
        {_Sender, users, Users} -> print_users(Users) 
    end.

print_users(Users) ->
    UsersLst = lists:map(fun({Key,Value}) -> Value end, dict:to_list(Users)),
    lists:foreach(fun(User) -> print_user(User) end, UsersLst).

print_user(User) ->
     {user, Name, Subscriptions, Subscribers, Messages, Timeline} = User,
     io:fwrite("NAME USER: ~p~n", [Name]),
     io:fwrite("  Subscriptions:~n  ~p~n", [Subscriptions]),
     io:fwrite("  Subscribers:~n  ~p~n", [Subscribers]),
     io:fwrite("  Messages:~n  ~p~n", [Messages]),
     io:fwrite("  Timeline:~n  ~p~n", [Timeline]).
  
get_user(ServerPid, Username) ->
    io:fwrite("~p is at ~p", [ServerPid, whereis(ServerPid)]),
    ServerPid !  {self(), user, Username},
    receive 
        {_, users, User} ->
            io:fwrite("User ~p ~n", [User]) 
    end.



