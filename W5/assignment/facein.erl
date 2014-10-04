%
% facein module
% {fvx507,tzk173}@alumni.ku.dk
%

-module(facein).
-export([start/1,add_friend/2,friends/1,broadcast/3,received_messages/1]).

start(N) -> spawn(fun() -> loop({N,[],[]}) end).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

add_friend(P, F) ->
    rpc(F, {add, P}).

friends(P) ->
    rpc(P, friends).

broadcast(P, M, R) ->
    P ! {self(), {broadcast, make_ref(), P, M, R}}.

received_messages(P) ->
    rpc(P, messages).

pass_msg(UID, FS, P, M, R) ->
    case FS of
        [{_,F}|[]]  -> F ! {self(), {broadcast, UID, P, M, R}};
        [{_,F}|T]   -> F ! {self(), {broadcast, UID, P, M, R}},
                       pass_msg(UID, T, P, M, R)
    end.

loop({N, L, MSG}) ->
    io:format('Person: ~w~nFriends: ~w~nMessages: ~w~n', [N, L, MSG]),
    receive
        % b) adds a friend
        {From, {add, P}} ->
            P ! {self(), {name, N}},
            receive
                {P, ok}                 -> From ! {self(), ok};
                {P, {error, Reason}}    -> From ! {self(), {error, Reason}}
            end,
            loop({N, L, MSG});

        {From, {name, F}} ->
            case lists:member({F, From}, L) of
                true    -> From ! {self(), {error, 'Already on friend list'}},
                           loop({N, L, MSG});
                false   -> From ! {self(), ok},
                           loop({N, [{F, From}|L], MSG})
            end;

        % c) retrives the friend list
        {From, friends} ->
            From ! {self(), L},
            loop({N, L, MSG});

        % d) broadcast a message M from person P within radius R
        {_, {broadcast, UID, P, M, 0}} ->
            self() ! {P, {message, UID, M}},
            loop({N, L, MSG});
        {_, {broadcast, UID, P, M, R}} ->
            self() ! {P, {message, UID, M}},
            case L of
                []  -> loop({N, L, MSG});
                L   -> pass_msg(UID, L, P, M, R-1),
                       loop({N, L, MSG})
            end;


        % adds a message, if it's not already added
        {From, {message, UID, M}} ->
            case lists:member({UID, From, M}, MSG) of
                true  -> loop({N, L, MSG});
                false -> loop({N, L, [{UID, From, M}|MSG]})
            end;

        % e) retrieves the received messages
        {From, messages} ->
            Messages = lists:map ( fun({_, F, M}) -> {F, M} end, MSG),
            From ! {self(), Messages},
            loop({N, L, MSG});

        % handle any other occurrences
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop({N, L, MSG})
    end.
