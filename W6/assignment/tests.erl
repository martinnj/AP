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

%mr:stop(MR).

io:format("Sum: ~p~n", [Sum]).
io:format("Fact: ~p~n", [Fact]).

%%% Part 2 tests start here.

{Words, Tracks} = read_mxm:from_file('data/mxm_dataset_test.txt').
% Words is a list of strings.
% Tracks is a list of binaries.
%TRK = hd(Tracks).
%{V1, V2, V3} = read_mxm:parse_track(TRK).
%R = part2:t1mapfunc(TRK).

{ok, NoOfWords} = mr:job(MR,
                         fun(Track) ->
                                 {_, _, WL} = read_mxm:parse_track(Track),
                                 Counts = lists:map(fun ({_, C}) -> C end,WL),
                                 part2:lsum(Counts,0) end,
                         fun(X,Acc) -> X + Acc end,
                         0,
                         Tracks).

io:format("Number of words in all songs: ~p~n", [NoOfWords]).


{ok, {AvgDiff, AvgTotal}} = mr:job(MR,
                                   fun(Track) ->
                                           %lol
                                   end,
                                   fun({Diff, Total},{Acc1, Acc2}) ->
                                           %lol
                                   end,
                                   {0,0},
                                   Trakcs).

mr:stop(MR).
