%vscode change light ctr k ctr t

%% This is a simple implementation of the project, using one server instance.
%%
%% It will create one "server" actor that contains all internal state (users,
%% their subscriptions, and their messages).
%%
%% This implementation is provided with unit tests, however, these tests are
%% neither complete nor implementation independent, so be careful when reusing
%% them.
-module(server_parallel).

-include_lib("eunit/include/eunit.hrl").

-export([initialize/0, initialize_with/1,initialize_alternative/1, server_actor/1, typical_session_1/1,
    typical_session_2/1, timeline/3, get_messages_users/3, get_profile/4, follow_testi/0, updateFollowers/4, timeline_pull/3]).

%% Aide aux activités en ligne
%% Additional API Functions

% Start server.
initialize() ->
    initialize_with(dict:new()).

% Start server with an initial state.
% Useful for benchmarking.

%Unis : [ [serverName, DictionaryofUser],  [vub, UsersVubDict], [ulb, UsersUlbDict], ...]
% {user, Name, Subscriptions, Subscribers, Messages, TimeLine}  !Name is subscribed to Subscriptions,  Subscribers are subscribed to Name!
initialize_with(Unis) ->
    lists:map(
        fun(L) ->
            [UniName, UsersofUni] = L,
            Spid = spawn_link(?MODULE, server_actor, [UsersofUni]),
            catch(unregister(UniName)),
            register(UniName, Spid),
            Spid end, Unis).

%initializes with uni names and usernames
%Unis : [ [serverName, ListOfUsernames],  [vub, Usersnamesuvb], [ulb, UsernamesUlb], ...]
initialize_alternative(Unis) ->
    lists:map(
        fun(L) ->
            [UniName, UserNamesofUni] = L,
            Users =  lists:foldl(fun(UserName,OldDict) -> dict:store(UserName, create_user(UserName), OldDict) end, dict:new(), UserNamesofUni),    
            Spid = spawn_link(?MODULE, server_actor, [Users]),
            catch(unregister(UniName)),
            register(UniName, Spid),
            Spid end, Unis).

% The server actor works like a small database and encapsulates all state of
% this simple implementation.
% Users is a dictionary of user names to tuples of the form:
%     {user, Name, Subscriptions,Subscribers, Messages}
% TODO add a pid where the user is stored
% Subscriptions  example: [[john, 8.0.8], [doe, 1.1.1], ...,  [usernameFollowee, spidFollowee])
% Subscriptions is a set of usernames with their respective server keys i.e. where this username 'lives'.
% Messages is a list of messages, of the form:
%     {message, UserName, MessageText, SendTime}
server_actor(Users) ->
    receive
        %testing purposes
        {Sender, kill} ->
            "killed";
        {Sender, ping} ->
            Sender ! {self(), pong},
            server_actor(Users);
        {Sender, users} ->
            Sender ! {self(), users, Users},
            server_actor(Users);
        {Sender, user, UserName} ->
            io:fwrite("receive"),
            Sender  ! {self(), user, get_user(UserName, Users)},
            server_actor(Users);

        {Sender, register_user, UserName} ->
            NewUsers = dict:store(UserName, create_user(UserName), Users),
            Sender ! {self(), user_registered, UserName},
            server_actor(NewUsers);

        {Sender, log_in, _UserName} ->
            % This doesn't do anything, but you could use this operation if needed.
            Sender ! {self(), logged_in},
            server_actor(Users);

        %Assumes that for example Alice registered on vub server wants to
        %follow Bob registered on ulb server, Alice will send this request
        %to "her server": vub server
        {Sender, follow, UserName, UserNameToFollow, SpidToFollow} ->
            UpdatedUsers = follow(Users, UserName, UserNameToFollow, SpidToFollow),
            Sender ! {self(), followed},
            SpidToFollow ! {self(), followed_by, UserNameToFollow, UserName, self()},
            server_actor(UpdatedUsers);


         {_Sender, followed_by, UserNameWhoGetsFollowed, UserNameWhoFollows, SpidWhoFollows} ->
            UpdatedUsers = followed_by(Users, UserNameWhoGetsFollowed, UserNameWhoFollows, SpidWhoFollows),
            server_actor(UpdatedUsers);

        %can be (slightly) optimised by spawning new process for storing message as well (TODO?)
        {Sender, send_message, UserName, MessageText, Timestamp} ->
            UpdatedUsers = store_message(Users, {message, UserName, MessageText, Timestamp}),
            Sender ! {self(), message_sent},
            spawn_link(?MODULE, updateFollowers, [UpdatedUsers, UserName, self() , {message, UserName, MessageText,Timestamp}]),
            server_actor(UpdatedUsers);

        {_Sender, update_timelines, Usernames, Message} ->
            UpdatedUsers = updateTimelines(Users,Usernames,Message), %spawn in new process calculate and send result by message for speed gain
            server_actor(UpdatedUsers); 

        %OLD implementation -> will fetch the messages of all subscriptions
        %We will have a more correct result but we will get it slower. 
        {Sender, get_timeline_pull, UserName} ->
            spawn_link(?MODULE, timeline_pull, [Sender, Users, UserName]),
            Sender ! {self(), simple_message, getting_timeline_please_be_patient},
            server_actor(Users);
        
        %USED , faster implementation. Every user saves a timeline when a follower posts a message the timeline will be updated
        {Sender, timeline, UserName} ->
            User = get_user(UserName, Users),
            TimeLine = sort_messages(get_timeline(User)),
            Sender ! {self(), timeline, UserName, TimeLine},
            server_actor(Users);

        {Sender, get_messages_users, UserNames} ->
            spawn_link(?MODULE, get_messages_users, [Sender, Users,  UserNames]),
            server_actor(Users);

        {Sender, get_profile, UserName, SpidUser} ->
            spawn_link(?MODULE, get_profile, [Sender, Users, UserName, SpidUser]),
            Sender ! {self(), simple_message , getting_profile_please_be_patient},
            server_actor(Users)
    end.

%%
%% Internal Functions
%%

% Create a new user with `UserName`.
create_user(UserName) ->
    {user, UserName, sets:new(),    sets:new(),  [],       []}.
   %{user, UserName, Subscriptions, Subscribers, Messages, Timeline}
   %                            Subscribers" elements: [UserName, Spid]  %Timeline elements {message,Username,MessageText,TimeStamp}
% Get user with `UserName` in `Users`.
% Throws an exception if user does not exist (to help in debugging).
% In your project, you do not need specific error handling for users that do not exist;
% you can assume that all users that use the system exist.
get_user(UserName, Users) ->
    case dict:find(UserName, Users) of
        {ok, User} ->  User;
        error -> throw({user_not_found, UserName})
    end.

get_subscribers(User) ->
    {user, _Name, _Subscriptions, Subscribers, _Messages, _Timeline} = User,
    Subscribers.
get_followers(User) -> get_subscribers(User).
get_subscriptions(User) ->
    {user, _Name, Subscriptions, _Subscribers, _Messages, _Timeline} = User,
    Subscriptions.
get_messages(User) ->
    {user, _Name, _Subscriptions, _Subscribers, Messages, _Timeline} = User,
    Messages.
get_timeline(User) ->
    {user, _Name, _Subscriptions, _Subscribers, _Messages, Timeline} = User,
    Timeline.
get_name(User) ->
    {user, Name, _Subscriptions, _Subscribers, _Messages, _Timeline} = User,
    Name.

%The profile is stored for every user, so this operation doesn't calculate only fetches, so It should be fast
get_profile(Sender, Users, UserName,SpidUser) ->  
       case dict:find(UserName, Users) of
         {ok,_User} ->  Sender ! {self(), profile, UserName, sort_messages(get_messages(Users, UserName))};  %If user is stored at the local instance itself
         _ ->  SpidUser ! {Sender, get_profile, UserName, SpidUser}  %Is user is stored at another instance ->  simply reroute the request to to the right instance
    end.

% Update `Users` so `UserName` follows `UserNameToFollow`.
follow(Users, UserName, UserNameToFollow, SpidToFollow) ->
    {user, Name, Subscriptions, Subscribers , Messages, Timeline} = get_user(UserName, Users),
    UpdatedSubscriptions = sets:add_element([UserNameToFollow, SpidToFollow], Subscriptions),
    UpdatedUser = {user, Name, UpdatedSubscriptions, Subscribers, Messages, Timeline},
    dict:store(UserName, UpdatedUser, Users).

followed_by(Users, UserNameWhoGetsFollowed, UserNameWhoFollows, SpidWhoFollows) ->
   {user, Name, Subscriptions, Subscribers , Messages, Timeline } = get_user(UserNameWhoGetsFollowed, Users),
    UpdatedSubscribers = sets:add_element([UserNameWhoFollows, SpidWhoFollows], Subscribers),
    UpdatedUser = {user, Name, Subscriptions, UpdatedSubscribers, Messages, Timeline}, 
    dict:store(UserNameWhoGetsFollowed,UpdatedUser, Users).



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



%returns an updated Users list where the timelines of local followers are updated,
%It' also going to group the "remote" followers by spid, and inform that SPID to 
%update the timelines of the specific users.
%Spid is the Spid of UserName
% !! must return the updated users (local users are updated).
%were going to handle remote users by sending messages.
%updateFollowers(Users, UserName, Spid, Message) ->
%    Followers = get_followers(get_user(Username, Users)),
%    FollowersGrouped = group_by_spid(Followers),
%    {_, LocalFollowers} = lists:keyfind(Spid, 1, FollowersGrouped),
%    RemoteFollowers = list:keydelete(Spid, 1, FollowersGrouped), 
%    lists:foreach(fun(FollowerName) -> Users =  updateTimeline(Users, FollowerName, Message) end, LocalFollowers),
%    Users.

updateFollowers(Users, UserName, Spid, Message) ->
    Followers = get_followers(get_user(UserName, Users)),
    FollowersGrouped = group_by_spid(Followers), 
    %TODO shadowed variabel
    lists:foreach(fun({Spid,UserNames}) -> 
        Spid ! {self(), update_timelines, UserNames, Message} end, FollowersGrouped).
        

%assume message is already in right form (witht timestampt etc.
updateTimeline(Users, UserName, Message) ->
    {user, Name, Subscriptions, Subscribers, Messages, Timeline } = get_user(UserName, Users),
    Updated_Timeline = Timeline ++ [Message],
    UpdatedUser = {user,Name,Subscriptions,Subscribers,Messages, Updated_Timeline},
    UpdatedUsers = dict:store(UserName, UpdatedUser, Users),
    UpdatedUsers.

updateTimelines(Users,UserNames, Message)->
    %TODO Users shadowed
    UpdatedUsers = lists:foldl(fun(Username, Users) -> updateTimeline(Users,Username,Message) end,Users, UserNames), 
    UpdatedUsers.

    

% Modify `Users` to store `Message`.
store_message(Users, Message) ->
    {message, UserName, _MessageText, _Timestamp} = Message,
    {user, Name, Subscriptions, Subscribers, Messages, Timeline} = get_user(UserName, Users),
    NewUser = {user, Name, Subscriptions, Subscribers, Messages ++ [Message], Timeline},
    dict:store(UserName, NewUser, Users).

% Get all messages by `UserName`.
get_messages(Users, UserName) ->
    {user, _, _, _Subscribers, Messages, _TimeLine} = get_user(UserName, Users),
    Messages.

%Recipient is the Spid that needs the result
get_messages_users(Recipient, Users, UserNames) -> 
    Messages = lists:flatten(lists:map(fun(UserName) -> get_messages(Users, UserName) end, UserNames)),
    Recipient ! {self(), messages,  Messages}.


%The timeline for every user is stored, like get_profile timeline is a fetch operation so it should also be fast
timeline(Sender, Users, UserName) ->
    User = get_user(UserName, Users),
    TimeLine = get_timeline(User),
    Sender ! {self(), timeline, TimeLine}.

%------------------------------------------------------------------------------------------------------------
%OLD IMPLEMENTATION but can still be used when we really want the most up to date timeline with high certainety , 

% Generate timeline for `UserName`.
% 1) Get followees of Username
% 2) Divide into local followees and remote followees
% 3) Messegas local followees: get_messages function easy,  Messages remote followees: Don't know yet
% Sender is pid of process who needs to receive the timeline

%This function will NOT be used, it will get all the messages for Username, (pull based methode)
timeline_pull(Sender, Users, UserName) ->
    {user, _Name, Subscriptions, _Subscribers, _Messages, _Timeline} = get_user(UserName, Users),
    LocalSubscriberNames = [Username || [Username, _Pid] <- sets:to_list(Subscriptions), dict:is_key(Username, Users)],
    RemoteSubscribers    = [[Username, Spid] || [Username, Spid] <- sets:to_list(Subscriptions), not lists:member(Username, LocalSubscriberNames)],
    MessagesLocalSubscribers = 
        lists:foldl(fun(FollowedUserName, AllMessages) ->
                        AllMessages ++ get_messages(Users, FollowedUserName)
                    end,
                    [],
                    LocalSubscriberNames),
    RemoteSubscribersGrouped = lists:foldl(
        fun([Username, Spid], Acc) ->
            case lists:keysearch(Spid, 1, Acc) of
                {value, {Spid, Usernames}} -> [{Spid, [Username | Usernames]} | lists:keydelete(Spid, 1, Acc)];
                false -> [{Spid, [Username]} | Acc]
            end
        end,
        [],
        RemoteSubscribers),

    MessagesRemoteSubscribers = 
        lists:foldl(
            fun({Spid, UserNames}, Acc) ->
                Spid ! {self(), get_messages_users, UserNames},
                %efficiency gain possible by making new processes and accumulating them but any delay here won't have an impact on the whole system, just the user will need to wait some more                
                receive {_Send, messages,  Messages} -> Messages ++ Acc;
                        X -> error("Should not happen, go look in timeline function", [X])
                 end 
            end,
            [],
            RemoteSubscribersGrouped),

    AllMessagesSorted = sort_messages(MessagesLocalSubscribers ++ MessagesRemoteSubscribers),
    Sender ! {self(), timeline_pull, UserName, AllMessagesSorted}.
%--------------------------------------------------------------------------------------------------------------------------------

% Sort `Messages` from most recent to oldest.
sort_messages(Messages) ->
    % Sort on the 4th element of the message tuple (= timestamp, this uses 1-based
    % indexing), and then reverse to put most recent first.
    lists:reverse(lists:keysort(4, Messages)).

%%
%% Tests
%%
% These tests are for this specific implementation. They are a partial
% definition of the semantics of the provided interface but also make certain
% assumptions of the implementation. You can re-use them, but you might need to
% modify them.


bilal_initialize_test() ->
    UsersVub =  [{user, "Alice", [], []}, {user, "Bob", [], [] ,[]}],
    UsersUlb =  [{user, "Charlie", [], []}, {user, "David", [], [], []}, {user, "Eve", [], [], []}],
    initialize_with( [[vub,UsersVub], [ulb,UsersUlb]]),
    io:fwrite("ok\n"),
    ulb ! {self(), users},
    receive 
        {_, users, Users}  ->  io:format("ulb users: ~p\n",[Users]),  ?assertMatch(Users,UsersUlb);
        _ -> erlang:error(unexpected_message_received) end,
    vub ! {self(), users},
    receive 
        {_, users, Users2} -> io:format("vub users: ~p\n", [Users2]), ?assertMatch(Users2,UsersVub);
         _ -> erlang:error(unexpected_message_received) end,
    ?assertMatch(ok,ok).

bilal_initialize_alternative_test() -> 
    UsersVub = lists:foldl(fun(UserName,OldDict) -> dict:store(UserName, create_user(UserName), OldDict) end, dict:new(), ["Alice", "Bob"]),
    UsersUlb = lists:foldl(fun(UserName,OldDict) -> dict:store(UserName, create_user(UserName), OldDict) end, dict:new(), ["Charlie", "David", "Eve"]),    

    initialize_alternative([[vub,["Alice", "Bob"]], [ulb,["Charlie", "David", "Eve"]]]),
    ulb ! {self(), users},
    receive 
        {_, users, Users}  ->  
            io:format("ULB users: ~p\n",[Users]),
            io:format("vub users: ~p\n", [Users]), 
            io:format("Charlie: ~p\n",[get_user("Charlie",Users)]),
            io:format("David: ~p\n",[get_user("David",Users)]),
            io:format("Eve: ~p\n",[get_user("Eve",Users)]),
            ?assertMatch(Users,UsersUlb);
        _ -> erlang:error(unexpected_message_received) end,
    vub ! {self(), users},
    receive 
        {_, users, Users2} -> 
            io:format("VUB users: ~p\n",[Users2]),
            io:format("Alice: ~p\n",[get_user("Alice",Users2)]),
            io:format("Bob: ~p\n",[get_user("Bob",Users2)]),       
            ?assertMatch(Users2,UsersVub);
         _ -> erlang:error(unexpected_message_received) end,
    ?assertMatch(ok,ok).




follow_testi() ->
    initialize_with([[vub, dict:new()], [ulb,dict:new()]]),
     io:format("vub ~p, ulb: ~p\n", [whereis(vub), whereis(ulb)]),
    vub ! {self(), register_user, "Alice"},
    vub ! {self(), register_user, "Bob"},
    ulb ! {self(), register_user, "Charlie"},


    vub ! {self(), follow, "Alice", "Bob", vub},
    vub ! {self(), follow, "Alice", "Charlie", ulb},
    ulb ! {self(), follow, "Charlie", "Bob", vub},

    %Bob gets followed by alice and charlie
    %Alice follows Bob and Charlie
    %Charlie follows Bob 
    %Charlie gets followed by Alice

    timer:sleep(1000),

    vub ! {self(), user, "Alice"},
    ulb ! {self(), user, "Charlie"},
    vub ! {self(), user , "Bob"},

    receive 
        {_Sender, user, User} ->
         io:format("\n~p follows: ~p \n ~p gets followed by:\n ~p\n\n", [get_name(User), sets:to_list(get_subscriptions(User)), get_name(User), sets:to_list(get_followers(User))])
       end,
    receive 
        {_SSender, user,User2} ->
          io:format("\n~p follows: ~p \n ~p gets followed by: ~p\n\n", [get_name(User2), sets:to_list(get_subscriptions(User2)), get_name(User2), sets:to_list(get_followers(User2))])
        end,
    receive 
        {_SSSender, user, User3} ->
          io:format("\n~p follows: ~p \n ~p gets followed by: ~p\n\n", [get_name(User3), sets:to_list(get_subscriptions(User3)), get_name(User3),sets:to_list(get_followers(User3))])
        end.
    
bilal_timeline_test() ->
    initialize_with([[vub, dict:new()], [ulb,dict:new()]]),

    io:format("vub ~p, ulb: ~p\n", [whereis(vub), whereis(ulb)]),
    vub ! {self(), register_user, "Alice"},
    vub ! {self(), register_user, "Bob"},
    ulb ! {self(), register_user, "Charlie"},
    ulb ! {self(), register_user, "Dave"}, 
    vub ! {self(), follow, "Alice", "Bob", vub},
    vub ! {self(), follow, "Alice", "Charlie", ulb},
    vub ! {self(), follow, "Alice", "Dave", ulb},
    timer:sleep(1000), %makes sure follower lists are updated
    vub ! {self(), send_message, "Bob", "Let's build something amazing together! #BobTheBuilder #ConstructionLife", "07h30"},
    vub ! {self(), send_message, "Bob", "It was a productive work day", "18h00"}, 
    ulb ! {self(), send_message, "Charlie", "I like chocolate", "14h00"},  
    ulb ! {self(), send_message, "Charlie", "Hooray, It is my birthday ", "19h00"},
    ulb ! {self(), send_message, "Dave", "I am the best rapper in the UK", "00h30"},
  
    timer:sleep(1000),
    vub ! {self(), timeline, "Alice"},  
    io:format("TESTING PUSH BASED IMPLEMENTATION\n"),    
    receive 
        {_, timeline, Username, TimeLine} -> 
            io:format("Timeline ~p:\n~p\n",[Username,TimeLine]),
            ?assertMatch(TimeLine, 
                [{message,"Charlie","Hooray, It is my birthday ","19h00"},
                 {message,"Bob","It was a productive work day","18h00"},
                 {message,"Charlie","I like chocolate","14h00"},
                 {message,"Bob","Let's build something amazing together! #BobTheBuilder #ConstructionLife","07h30"},
                 {message,"Dave","I am the best rapper in the UK","00h30"}])
        end,
    
    vub ! {self(), get_timeline_pull, "Alice"},  
    io:format("TESTING PULL BASED IMPLEMENTATION, RESULT SHOULD BE THE SAME!\n"),    
    receive 
        {_, timeline_pull, Usernname, TimeLinePulled} -> 
            io:format("Timeline Pulled ~p:\n~p\n",[Usernname,TimeLinePulled]),
            ?assertMatch(TimeLinePulled, 
                [{message,"Charlie","Hooray, It is my birthday ","19h00"},
                 {message,"Bob","It was a productive work day","18h00"},
                 {message,"Charlie","I like chocolate","14h00"},
                 {message,"Bob","Let's build something amazing together! #BobTheBuilder #ConstructionLife","07h30"},
                 {message,"Dave","I am the best rapper in the UK","00h30"}])
        end.

bilal_get_profile_test() ->
    initialize_with([[vub, dict:new()], [ulb,dict:new()]]),
    vub ! {self(), register_user, "Bob"},
    vub ! {self(), send_message, "Bob", "Let's build something amazing together! #BobTheBuilder #ConstructionLife", "07h30"},
    vub ! {self(), send_message, "Bob", "It was a productive work day", "18h00"}, 

    vub ! {self(), get_profile, "Bob", vub}, %get Bob's timeline by sending request to vub server (server where Bob is registered)
    receive 
        {_, profile, _, Profile} ->
            io:format("Bob's profile:\n ~p\n", [Profile]),
        ?assertMatch(Profile, [{message,"Bob","It was a productive work day","18h00"}, {message,"Bob","Let's build something amazing together! #BobTheBuilder #ConstructionLife","07h30"}]) end,
    
     ulb ! {self(), get_profile, "Bob", vub}, %get Bob's timeline by sending request to ulb server 
    receive 
        {_, profile, _, ProfileB} ->
            io:format("Bob's profile:\n ~p\n", [ProfileB]),
        ?assertMatch(ProfileB, [{message,"Bob","It was a productive work day","18h00"}, {message,"Bob","Let's build something amazing together! #BobTheBuilder #ConstructionLife","07h30"}]) end.



% Test initialize function
initialize_test() ->
    catch unregister(server_actor),
    initialize().

% Initialize server and test user registration of 4 users.
% Returns list of user names to be used in subsequent tests.
register_user_test() ->
    initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "A")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "B")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "C")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "D")),
    ["A", "B", "C", "D"].

% Test log in.
log_in_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    ?assertMatch({_Server1, logged_in}, server:log_in(server_actor, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(server_actor, UserName2)).
    % Note: returned pids _Server1 and _Server2 do not necessarily need to be
    % the same.

% Test follow: user 1 will follow 2 and 3.
follow_test() ->
    [UserName1, UserName2, UserName3 | _ ] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_actor, UserName1),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName2)),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName3)),
    {UserName1, Server1, [UserName2, UserName3]}.

% Test sending a message.
send_message_test() ->
    {UserName1, Server1, Subscriptions} = follow_test(),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, "Hello!")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, "How is everyone?")),
    {UserName1, Server1, Subscriptions}.

% Test getting a timeline.
get_timeline_test() ->
    {UserName1, Server1, [UserName2, UserName3]} = follow_test(),

    % When nothing has been sent, the timeline is empty.
    ?assertMatch([], server:get_timeline(Server1, UserName1)),

    ?assertMatch(message_sent,
        server:send_message(Server1, UserName2, "Hello I'm B!")),

    % One message in the timeline.
    ?assertMatch([
        {message, UserName2, "Hello I'm B!", _TimeB1}
    ], server:get_timeline(Server1, UserName1)),

    ?assertMatch(message_sent,
        server:send_message(Server1, UserName2, "How is everyone?")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName3, "Hello I'm C!")),

    % All three messages in the timeline, newest first.
    ?assertMatch([
        {message, UserName3, "Hello I'm C!", _TimeC1},
        {message, UserName2, "How is everyone?", _TimeB2},
        {message, UserName2, "Hello I'm B!", _TimeB1}
    ], server:get_timeline(Server1, UserName1)),

    % User 2 does not follow any so gets an empty timeline.
    ?assertMatch([], server:get_timeline(Server1, UserName2)).

% Test getting the profile.
get_profile_test() ->
    {UserName1, Server1, [UserName2 | _]} = send_message_test(),
    % Most recent message is returned first.
    ?assertMatch([
        {message, UserName1, "How is everyone?", _TimeA2},
        {message, UserName1, "Hello!", _TimeA1}
    ], server:get_profile(Server1, UserName1)),
    % User 2 hasn't sent any messages.
    ?assertMatch([], server:get_profile(Server1, UserName2)).

% A "typical" session.
typical_session_test() ->
    initialize_test(),
    Session1 = spawn_link(?MODULE, typical_session_1, [self()]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self()]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid) ->
    {_, user_registered} = server:register_user(server_actor, "Alice@vub.be"),
    {Server, logged_in} = server:log_in(server_actor, "Alice@vub.be"),
    message_sent = server:send_message(Server, "Alice@vub.be", "Hello!"),
    message_sent = server:send_message(Server, "Alice@vub.be", "How is everyone?"),
    % Check own profile
    [{message, "Alice@vub.be", "How is everyone?", Time2},
     {message, "Alice@vub.be", "Hello!", Time1}] =
        server:get_profile(Server, "Alice@vub.be"),
    ?assert(Time1 =< Time2),
    TesterPid ! {self(), ok}.

typical_session_2(TesterPid) ->
    {_, user_registered} = server:register_user(server_actor, "Bob@vub.be"),
    {Server, logged_in} = server:log_in(server_actor, "Bob@vub.be"),

    % Sleep one second, while Alice sends messages.
    timer:sleep(1000),

    [] = server:get_timeline(Server, "Bob@vub.be"),
    followed = server:follow(Server, "Bob@vub.be", "Alice@vub.be"),
    [{message, "Alice@vub.be", "How is everyone?", Time2},
     {message, "Alice@vub.be", "Hello!", Time1}] =
        server:get_timeline(Server, "Bob@vub.be"),
    ?assert(Time1 =< Time2),

    TesterPid ! {self(), ok}.
