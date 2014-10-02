%
% facein module
% {fvx507,tzk173}@alumni.ku.dk
%

-module(facein).
-export([start/1,loop/1,add_friend/2,friends/1,broadcast/3,received_messages/1]).

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

broadcast(P, M, R) -> P ! {self(), {broadcast, make_ref(), P, M, R}}.


pass_msg(UID, F, M, R) ->
    case F of
        [H|[]]  -> H ! {self(), {broadcast, UID, H, M, R-1}};
        [H|T]   -> H ! {self(), {broadcast, UID, H, M, R-1}},
                   pass_msg(UID, T, M, R-1)
    end.

received_messages(P) ->
    rpc(P, messages).

loop({N, L, MSG}) ->
    receive
        % a) something descriptive here
        {From, {add, P}} ->
            P ! {self(), {name, N}},
            receive
                {P, ok} -> From ! {self(), ok}
            end,
            loop({N, L, MSG});
        
        % b) baah :)
        {From, {name, F}} ->
            From ! {self(), ok},
            loop({N, [{F, From}|L], MSG});
        
        % c) retrives the friends
        {From, friends} ->
            From ! {self(), L},
            loop({N, L, MSG});
        
        % d) ...
        {_, {broadcast, UID, P, M, 0}} ->
            P ! {self(), {message, UID, M}},
            loop({N, L, MSG});
        
        {_, {broadcast, UID, P, M, R}} ->
            P ! {self(), {message, UID, M}},
            F = friends(P),
            if not(F == []) -> pass_msg(UID, F, M, R) end,
            loop({N, L, MSG});
        
        {From, {message, UID, M}} ->
            case lists:member({UID, From, M}, MSG) of
                true  -> loop({N, L, MSG});
                false -> loop({N, L, [{UID, From, M}|MSG]})
            end;
        
        % e) ...
        {From, messages} ->
            Messages = lists:map ( fun({_, F, M}) -> {F, M} end, MSG ),
            From ! {self(), Messages},
            loop({N, L, MSG});
             
        % handle any other occurrences
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop({N, L, MSG})
    end.

