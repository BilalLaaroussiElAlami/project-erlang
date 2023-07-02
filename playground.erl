-module(playground).
-export([main/0]).

main() ->
    {Sender, messages, Messages} 
    = 
    { 0990,
        messages,
                        [{message,"Charlie",
                                                       "I like chocolate",
                                                       "14h00"},
                                                      {message,"Charlie",
                                                       "Hooray, It is my birthday ",
                                                       "19h00"}]},
    io:format(" ~p \n ~p \n ~p", [Sender, messages, Messages]).



