-module(benchmarkBilal).

-export([test_fib/0, test_timeline/0, test_send_message/0]).

%% Fibonacci
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

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

%% Benchmarks
% Below are some example benchmarks. Extend these to test the best and worst
% case of your implementation, some typical scenarios you imagine, or some
% extreme scenarios.

test_fib() ->
    io:format("Parameters:~n"),
    io:format("~n"),
    run_benchmark("fib", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
    % Spawn 64 processes that each compute the 30th Fibonacci number.
    BenchmarkPid = self(),
    Pids = [spawn(fun () ->
        fib(30),
        BenchmarkPid ! done
    end) || _ <- lists:seq(1, 64)],
    lists:foreach(fun (_) ->
        receive done ->
            ok
        end
    end, Pids).

% Creates a server with 5000 users following 25 others and sending 10 messages.
%
% Note that this code depends on the implementation of the server. You will need to
% change it if you change the representation of the data in the server.

%5 server instances , 1000 users per server, every user follow 50 user, evenly distributed over each server instance (10 per server instance)
initialize_server() ->
    % Seed random number generator to get reproducible results.
    rand:seed_s(exsplus, {0, 0, 0}),
    % Parameters
    NumberOfServers = 5,
    NumberOfUsers = 5000,
    NUmberOfUsersPerServer = NumberOfUsers/NumberOfServers,
    NumberOfSubscriptions = 50,
    % Users follow evenly over servers
    NumberOfSubscriptionsPerServer = NumberOfSubscriptions div NumberOfServers
    NumberOfMessages = 10,
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n",             [NumberOfUsers]), 
    io:format("Number of servers: ~p~n",           [NumberOfServers]),
    io:format("Number of users per server : ~p~n", [NUmberOfUsersPerServer]),
    io:format("Number of subscriptions: ~p~n",     [NumberOfSubscriptions]),
    io:format("Number of messages: ~p~n",          [NumberOfMessages]),
    UnisUserNames = split(NumberOfUsers,NumberOfServers),
    server_centralized:initialize_alternative(UnisUserNames),
    lists:map(
        fun(UniUsernames) ->
            [Uni, Usernames] = UnisUserNames, 

    )

%picks random UsersPerUni Users per Uni, each associated with the uni
pick_random_users_over_unis(UnisUsers, UsersPerUni) ->
    lists:map(
        fun(UniUsers) -> 
            [Uni, Users] = UniUsers,
            [Uni, pick_random_n(Users,UsersPerUni)] end,
            UnisUsers).



follow(PidFollower, UsernameFollower, UserNameFollowee, PidFollowee) ->
    %vub ! {self(), follow, "Alice", "Bob", vub},
    PidFollower ! {self(), follow, UsernameFollower, UserNameFollowee, PidFollowee}
   

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
            [Server, lists:seq(Begin, End)] end,
        Servers
    ),
    Result.
% Pick a random element from a list.
pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).

% Pick n random elements from list
pick_random_n(List, N) ->
    [pick_random(List) || _ <- lists:seq(1, N)]

% Generate a random message `I` for `UserName`.
generate_message(UserName, I) ->
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

% Get timeline of 10000 users (repeated 30 times).
test_timeline() ->
    {ServerPid, UserName} = initialize_server(),
    run_benchmark("timeline",
        fun () ->
            lists:foreach(fun (_) ->
                server:get_timeline(ServerPid, pick_random(UserName))
            end,
            lists:seq(1, 10000))
        end,
        30).

% Send message for 10000 users.
test_send_message() ->
    {ServerPid, UserName} = initialize_server(),
    run_benchmark("send_message",
        fun () ->
            lists:foreach(fun (_) ->
                server:send_message(ServerPid, pick_random(UserName), "Test")
            end,
            lists:seq(1, 10000))
        end,
        30).
