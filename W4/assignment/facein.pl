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
   person(andrzej, [susan, ken]),
   person(martin, [casper, erik]),
   person(casper, [martin, erik]),
   person(erik, [martin, casper])]).

graph :- g(G).

% member_of(X, L)
member_of(X, [X | T]).
member_of(X, [H | T]) :- member_of(X, T).

% friends_of(G, X, XS)
friends_of([person(X, []) | T], X, []).
friends_of([person(X, XS) | T], X, XS).
friends_of([person(H, HS) | T], X, XS) :- friends_of(T, X, XS).

% goodfriends(G, X, Y)
goodfriends(G, X, Y) :-
    member_of(person(X,_), G), % prevent infinite recursion
    member_of(person(Y,_), G), % prevent infinite recursion
    friends_of(G, X, XS),
    friends_of(G, Y, YS),
    member_of(X, YS),
    member_of(Y, XS).

% Recursively check if X is good friends with a list of names.
friendCheck(G, X, [H |[]]) :- goodfriends(G,X,H).
friendCheck(G, X, [H | T]) :- goodfriends(G,X,H) , friendCheck(G, X, T).

% Checks if everyone in a list of names are "good" friends,
% list must have at least 2 names.
clique(G, [H |[M | _]]) :- friendCheck(G, H, [T]).
clique(G, [H | T]) :- friendCheck(G, H, T) , clique(G, T).

% 1. lav liste over alle i G
% 2. hjælpe funktion til at fjerne X i XS
% 3. wannabe' til at vedligeholde en liste af manglende X'er i XS

% A wannabe is someone who transitively is friends with everyone.
wannabe(G, X).
