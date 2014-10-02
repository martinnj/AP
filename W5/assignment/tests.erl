%
% tests.erl
%

%-import(facein, [start/1,add_friend/2,friends/1,broadcast/3,received_messages/1]).

% please note, this list is sorted xD
Andrzej = facein:start(andrzej).
Jen = facein:start(jen).
Jessica = facein:start(jessica).
Ken = facein:start(ken).
Reed = facein:start(reed).
Susan = facein:start(susan).
Tony = facein:start(tony).

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

io:format("Jen's friends:~n~w~n", [facein:friends(Jen)]).
io:format("Susan's friends:~n~w~n", [facein:friends(Susan)]).
io:format("Tony's friends:~n~w~n", [facein:friends(Tony)]).

facein:broadcast(Jessica, "Hello!", 1).
