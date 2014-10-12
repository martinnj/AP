% Functions and tests for Assignment 6, Part 2: MusicXMatch Dataset in AP2014
% Written by Martin Jrgensen, tzk173
%        and Casper B Hansen, fvx507

-module(part2).
-export([lsum/2, grep/1]).


 lsum([]   , Sum) -> Sum;
 lsum([H|T], Sum) -> lsum(T, H + Sum).

inWords(_, _, []) -> false;
inWords(Word, Words, [{Wid,_} | T]) ->
    AW = lists:nth(Wid,Words),
    (AW == Word) or inWords(Word, Words, T).
            
    

% Gets a list of MSD Id's where the given word is found in the lyrics.
grep(Word) ->
    {Words, Tracks} = read_mxm:from_file('data/mxm_dataset_test.txt'),
    %{Words, Tracks} = read_mxm:from_file('data/mxm_dataset_train.txt'),
    
    Input = lists:map(fun(T) -> {Word, T} end,Tracks),
    {ok, MR} = mr:start(3),
    {ok, MIDs} = mr:job(MR,
                        fun({Wrd, Track}) ->
                            {MSDId,_,LWords} = read_mxm:parse_track(Track),
                            case inWords(Wrd, Words, LWords) of
                                true -> [MSDId];
                                false -> []
                            end
                        end,
                        fun(NewTrack,Acc) ->
                            lists:append(NewTrack,Acc)
                        end,
                        [],
                        Input),
    mr:stop(MR),
    MIDs.

