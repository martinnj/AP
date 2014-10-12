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
   person(andrzej, [susan, ken])]).
   %person(martin, [casper, erik, niels]),
   %person(casper, [martin, erik, niels]),
   %person(niels, [martin, erik, casper]),
   %person(erik, [martin, casper, niels])]).

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

% returns true iff. B is a friend of A
friend(G, A, B) :-
    member_of(person(A, AS), G),
    member_of(person(B, _), G),
    member_of(B, AS).

myselect(X, [X|L], L).
myselect(X, [X1|L1], [X1|L2]) :- myselect(X, L1, L2).

% Checks if there is a transitive relation between A and C.
transitive(_, I, I).
transitive(G, A, C) :-
    myselect(person(A, _), G, R),
    friend(G, A, B),
    transitive(R, B, C).

% helper rule for wannabe
is_wannabe(_, [], _).
is_wannabe(G, [person(Y,_) | R], X) :-
    transitive(G, X, Y),
    is_wannabe(G, R, X).

% A wannabe is someone who transitively is friends with everyone.
wannabe(G, X) :- is_wannabe(G, G, X).

% helper rule for idol
is_idol(_, [], _).
is_idol(G, [person(Y,_) | R], X) :-
    transitive(G, Y, X),
    is_idol(G, R, X).

% An idol is a person who is transitively liked by everyone.
idol(G, X) :- is_idol(G, G, X).

% directional relationship
drel(G, D, A, B) :-
    member_of(person(X, _), G),
    member_of(person(Y, YS), G),
    ((D = <-, X=A, Y=B) ; (D = ->, X=B, Y=A)),
    member_of(X, YS).

% Holds if P is a path from X to Y in G.
ispath(G, A, Y, [A, D, Y | []]) :- drel(G, D, A, Y).
ispath(G, A, Y, [A, D, B | T]) :- drel(G, D, A, B) , ispath(G, B, Y, [B|T]).
ispath(G, X, Y, [X, D, B | T]) :- drel(G, D, X, B) , ispath(G, B, Y, [B|T]).
