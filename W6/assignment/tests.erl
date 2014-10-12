%
% tests.erl
%

{ok, MR}   = mr:start(3).

% expected: 1+2+3+4+5+6+7+8+9+10 = 55
{ok, Sum} = mr:job(MR,
                   fun(X) -> X end,
                   fun(X,Acc) -> X+Acc end,
                   0, lists:seq(1,10)).

% expected: 1*2*3*4*5*6*7*8*9*10 = 10! = 3.628.800
{ok, Fact} = mr:job(MR,
                    fun(X) -> X end,
                    fun(X,Acc) -> X*Acc end,
                    1, lists:seq(1,10)).

mr:stop(MR).

io:format("Sum: ~p~n", [Sum]).
io:format("Fact: ~p~n", [Fact]).

