% Functions and tests for Assignment 6, Part 2: MusixXMatch Dataset in AP2014
% Written by Martin Jrgensen, tzk173
%        and Casper B Hansen, fvx507

-module(part2).
-export([lsum/2, grep/1]).


 lsum([]   , Sum) -> Sum;
 lsum([H|T], Sum) -> lsum(T, H + Sum).

% inWords(String, [String], [{Wid, Count}])
inWords(_, _, []) -> false;
inWords(Word, Words, [{Wid,_} | T]) ->
    %io:format("Much io, such waste~n",[]),
    AW = lists:nth(Wid,Words),
    (AW == Word) or inWords(Word, Words, T).
    %case AW == Word of
    %    true -> true;
    %    false -> inWords(Word, Words, T)
    %end.
            
    

% Gets a list of MSD Id's where the given word is found in the lyrics.
grep(Word) ->
    {Words, Tracks} = read_mxm:from_file('data/mxm_dataset_test.txt'),
    %{Words, Tracks} = read_mxm:from_file('data/mxm_dataset_train.txt'),
    
    % Input = [{Word, TrackXXX},...]
    Input = lists:map(fun(T) -> {Word, T} end,Tracks),
    %io:format("Much io, less waste. ~p~p~n", [Word,hd(Input)]),
    {ok, MR} = mr:start(3),
    %io:format("MR started, sending job.~n"),
    {ok, MIDs} = mr:job(MR,
                        %fun (X) -> X end,
                        %fun (X, Acc) -> [X|Acc] end,
                        % THe MSD id, is the first id when a track is parsed.
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

