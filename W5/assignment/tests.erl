%
% tests.erl
%

% start-up person processes
Andrzej = facein:start(andrzej).
Jen = facein:start(jen).
Jessica = facein:start(jessica).
Ken = facein:start(ken).
Reed = facein:start(reed).
Susan = facein:start(susan).
Tony = facein:start(tony).

% construct network graph
facein:add_friend(Andrzej, Ken).
facein:add_friend(Andrzej, Susan).

facein:add_friend(Jen, Jessica).
facein:add_friend(Jen, Susan).
facein:add_friend(Jen, Tony).

facein:add_friend(Jessica, Jen).

facein:add_friend(Ken, Andrzej).

facein:add_friend(Reed, Jessica).
facein:add_friend(Reed, Tony).

facein:add_friend(Susan, Andrzej).
facein:add_friend(Susan, Jen).
facein:add_friend(Susan, Jessica).
facein:add_friend(Susan, Reed).

% friend list tests
io:format("Andrzej's friends: ~w~n", [facein:friends(Andrzej)]).
io:format("Jen's friends: ~w~n", [facein:friends(Jen)]).
io:format("Jessica's friends: ~w~n", [facein:friends(Jessica)]).
io:format("Ken's friends: ~w~n", [facein:friends(Ken)]).
io:format("Reed's friends: ~w~n", [facein:friends(Reed)]).
io:format("Susan's friends: ~w~n", [facein:friends(Susan)]).
io:format("Tony's friends: ~w~n", [facein:friends(Tony)]).

facein:broadcast(Ken, "Martin and Casper will probably get an A.", 1).
facein:broadcast(Andrzej, "Really? Do you think Martin and Casper should get
an A for the exam?", 1).
facein:broadcast(Ken, "Oh, maybe. But I meant for this assignment. It's good really good!", 1).

facein:broadcast(Susan, "I heard Martin and Casper are getting an A for the
exam, even though it's not even released yet!", 1).
facein:broadcast(Jen, "Say what!?", 1).
facein:broadcast(Jessica, "That's cheating!", 1).
facein:broadcast(Reed, "Are you kidding me?!", 1).

facein:broadcast(Andrzej, "Oh, man...", 1).
facein:broadcast(Ken, "What?", 1).
facein:broadcast(Andrzej, "Rumour has it you're giving them an A at the exam.", 1).
facein:broadcast(Ken, "People of graph G! I have said no such thing!", 10).

facein:broadcast(Jessica, "Susan is a liar...", 1).
facein:broadcast(Jen, "Yeah, Susan cheated!", 2).

facein:broadcast(Andrzej, "I heard Susan has cheated!", 1).
facein:broadcast(Ken, "Really?", 1).
facein:broadcast(Andrzej, "Yeah!", 1).
facein:broadcast(Susan, "Aw man... :(", 0).

facein:broadcast(Tony, "Meh, I don't give a damn. Leave me be!", 10).

io:format("Andrzej's messages:~n~p~n", [facein:received_messages(Andrzej)]).
io:format("Jen's messages:~n~p~n", [facein:received_messages(Jen)]).
io:format("Jessica's messages:~n~p~n", [facein:received_messages(Jessica)]).
io:format("Ken's messages:~n~p~n", [facein:received_messages(Ken)]).
io:format("Reed's messages:~n~p~n", [facein:received_messages(Reed)]).
io:format("Susan's messages:~n~p~n", [facein:received_messages(Susan)]).
io:format("Tony's messages:~p~n", [facein:received_messages(Tony)]).

