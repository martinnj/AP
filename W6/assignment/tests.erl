%
% tests.erl
%

{ok, MR} = mr:start(3).

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

% Read track data.
{Words, Tracks} = read_mxm:from_file('data/mxm_dataset_test.txt'). % Little dataset (~27K songs)
%{Words, Tracks} = read_mxm:from_file('data/mxm_dataset_train.txt'). % Big ass dataset.

% Count the total number of words in all the songs.
{ok, NoOfWords} = mr:job(MR,
                         fun(Track) ->
                                 {_, _, WL} = read_mxm:parse_track(Track),
                                 Counts = lists:map(fun ({_, C}) -> C end,WL),
                                 part2:lsum(Counts,0) end,
                         fun(X,Acc) -> X + Acc end,
                         0,
                         Tracks).

io:format("Number of words in all songs: ~p~n", [NoOfWords]).

% Calculate the average number of different words, and the average number of words in total,
% per song.
{ok, {AvgDiff, AvgTotal, _}} = mr:job(MR,
                                   fun(Track) ->
                                       {_, _, WL} = read_mxm:parse_track(Track),
                                       DiffWords = length(WL),
                                       Counts = lists:map(fun ({_, C}) -> C end,WL),
                                       WordCount = part2:lsum(Counts,0),
                                       {DiffWords, WordCount, 0}
                                   end,
                                   fun({Diff, Total, _},{Acc1, Acc2, N}) ->
                                       %--New average = old average * (n-1)/n + new value /n
                                       NAv = Acc1 * (N-1)/N + Diff/N,
                                       NTa = Acc2 * (N-1)/N + Total/N,
                                       {NAv, NTa, N + 1}
                                   end,
                                   {0,0,1},
                                   Tracks).
io:format("Average number of different words per song: ~p~n", [AvgDiff]).
io:format("Average number of words per song: ~p~n", [AvgTotal]).

% mr:stop(MR).

%% Testing grep.
% Returns an empty list.
NoIDs = part2:grep("VeryLongWordItWontFind.").
io:format("MSD Id's for tracks with the word 'VeryLongWordItWontFind.': ~p~n", [NoIDs]).

% Returns a few hits.
IDs = part2:grep("harri").
io:format("MSD Id's for tracks with the word 'harri': ~p~n", [IDs]).

%% calculate reverse index
%{ok, RevIndex} = mr:job(MR,
%                        fun(Word) ->
%                            {Word, part2:grep(Word)}
%                        end,
%                        fun({Word, Tracks}, Acc) ->
%                            dict:append(Word, Tracks, Acc)
%                        end,
%                        dict:new(),
%                        Words).
%
%LookUp = "onc".
%io:format("Look-up of '~p': ~p~n", [LookUp,dict:find(LookUp,RevIndex)]).

mr:stop(MR).
