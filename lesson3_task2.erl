-module(lesson3_task2).
-export([words/1]).

words(Bin) ->
    words(Bin, <<>>, []).

words(<<>>, Word, Acc) when Word =/= <<>> ->  reverse([Word | Acc]);
words(<<>>, _, Acc) ->  reverse(Acc);
words(<<32, Rest/binary>>, <<>>, Acc) ->  words(Rest, <<>>, Acc);
words(<<32, Rest/binary>>, Word, Acc) ->  words(Rest, <<>>, [Word | Acc]);

words(Bin, Word, Acc) ->
    case next_utf8_char(Bin) of
        {Char, Rest} ->
            words(Rest, <<Word/binary, Char/binary>>, Acc)
    end.

next_utf8_char(<<Char1, Rest/binary>>) when Char1 band 128 =:= 0 ->   {<<Char1>>, Rest};
next_utf8_char(<<Char1, Char2, Rest/binary>>) when Char1 band 224 =:= 192 ->   {<<Char1, Char2>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Rest/binary>>) when Char1 band 240 =:= 224 ->    {<<Char1, Char2, Char3>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Char4, Rest/binary>>) when Char1 band 248 =:= 240 ->    {<<Char1, Char2, Char3, Char4>>, Rest}.

reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([Head|Tail], Acc) -> reverse(Tail, [Head|Acc]).