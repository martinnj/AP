/** Assignment 4 for Advanced Programming 2014
 *  Written by Martin Jrgensen, tzk173
 *  and Casper B. Hansen, xxx111
 */

:- style_check(-singleton). % shut the fuck up...

/** Demo graph */
g([person(susan, [reed, jen, andrzej, jessica]),
   person(reed, [tony, jessica]),
   person(jessica, [jen]),
   person(tony, []),
   person(ken, [andrzej]),
   person(jen, [tony, susan, jessica]),
   person(andrzej, [susan, ken])]).

graph :- g(G).

% member_of(X, L)
member_of(X, [X | T]).
member_of(X, [H | T]) :- member_of(X, T).

%member_of(X, [person(X, XS) | T]).
%member_of(X, [person(H, HS) | T]) :- member_of(X, T).

% friends_of(G, X, XS)
friends_of([person(X, []) | T], X, []).
friends_of([person(X, XS) | T], X, XS).
friends_of([person(H, HS) | T], X, XS) :- friends_of(T, X, XS).

% goodfriends(G, X, Y)
%
% we say that X and Y are good friends if they are on each others' friend
% lists.
goodfriends(G, X, Y) :-
    member_of(person(X,_), G),
    member_of(person(Y,_), G),
    friends_of(G, X, XS),
    friends_of(G, Y, YS),
    member_of(X, YS),
    member_of(Y, XS).

/*
goodfriends(G, X, Y) :-
    friends(G, X, Y),
    friends(G, Y, X)
 */
