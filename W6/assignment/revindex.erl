%% calculate reverse index
{ok, RevIndex} = mr:job(MR,
                        fun(Word) ->
                            {Word, part2:grep(Word)}
                        end,
                        fun({Word, Tracks}, Acc) ->
                            dict:append(Word, Tracks, Acc)
                        end,
                        dict:new(),
                        Words).
