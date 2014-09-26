/** Assignment 4 for Advanced Programming 2014
 *  Written by Martin Jrgensen, tzk173
 *  and Casper B. Hansen, xxx111
 */

% :- style_check(-singleton). % shut the fuck up...

/** Demo graph */
g([person(susan, [reed, jen, andrzej, jessica]),
   person(reed, [tony, jessica]),
   person(jessica, [jen]),
   person(tony, []),
   person(ken, [andrzej]),
   person(jen, [tony, susan, jessica]),
   person(andrzej, [susan, ken]),
   person(martin, [casper, erik, niels]),
   person(casper, [martin, erik, niels]),
   person(niels, [martin, erik, casper]),
   person(erik, [martin, casper, niels])]).

% member_of(X, L)
% returns true iff. X is a member of L
member_of(X, [H | T]) :- X = H; member_of(X, T).

% goodfriends(G, X, Y)
% returns true iff. X is a member of F(Y) and Y is a member of F(x),
% where F(Z) is the list of Z's friends.
goodfriends(G, X, Y) :-
    member_of(person(X, XS), G),
    member_of(person(Y, YS), G),
    member_of(X, YS),
    member_of(Y, XS).

% Recursively check if X is good friends with a list of names.
friendCheck(G, X, [H |[]]) :- goodfriends(G,X,H).
friendCheck(G, X, [H | T]) :- goodfriends(G,X,H) , friendCheck(G, X, T).

% clique(G, L)
% Checks if everyone in a list of names are "good" friends,
% list must have at least 2 names.
clique(G, [H, M | _]) :- friendCheck(G, H, [M]).
clique(G, [H | T]) :- friendCheck(G, H, T), clique(G, T).

transitive(G, A, C) :-
    member_of(person(A, AS), G),
    member_of(person(B, BS), G),
    member_of(B, AS), % aRb
    (member_of(C, BS); member_of(C, AS)). % bRc v aRc

%trans(G, A, C, []).
trans(G, A, C, []) :-
    % [H|T] = list of persons in G
    member_of(person(A, AS), G),
    member_of(person(B, BS), G),
    member_of(person(C, CS), G),
    
    member_of(B, AS), % aRb
    (member_of(C, BS); /* bRc */ trans(G, B, C, [])). % aRb ^ ?(bRd ^ dRc)

% A wannabe is someone who transitively is friends with everyone.
% wannabe(G, X).
