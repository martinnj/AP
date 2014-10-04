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
io:format("Andrzej's friends:~n~w~n", [facein:friends(Andrzej)]).
io:format("Jen's friends:~n~w~n", [facein:friends(Jen)]).
io:format("Jessica's friends:~n~w~n", [facein:friends(Jessica)]).
io:format("Ken's friends:~n~w~n", [facein:friends(Ken)]).
io:format("Reed's friends:~n~w~n", [facein:friends(Reed)]).
io:format("Susan's friends:~n~w~n", [facein:friends(Susan)]).
io:format("Tony's friends:~n~w~n", [facein:friends(Tony)]).

facein:broadcast(Reed, "Hello!", 10).

io:format("Reed's messages:~n~w~n", [facein:received_messages(Tony)]).

