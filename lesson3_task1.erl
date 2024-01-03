-module(lesson3_task1).
-export([first_word/1]).

first_word(BinText) ->
    first_word(BinText, <<>>).

first_word(<<>>, Acc) ->
    Acc;
first_word(BinText, Acc) ->
    {Char, Rest} = next_utf8_char(BinText),
    case Char of
        <<32>> -> 
            Acc;
        _ ->
            first_word(Rest, <<Acc/binary, Char/binary>>)
    end.

% Helper function for extracting the next UTF-8 character in binary format.
next_utf8_char(<<Char1, Rest/binary>>) when Char1 band 128 =:= 0 ->   {<<Char1>>, Rest};
next_utf8_char(<<Char1, Char2, Rest/binary>>) when Char1 band 224 =:= 192 ->  {<<Char1, Char2>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Rest/binary>>) when Char1 band 240 =:= 224 ->   {<<Char1, Char2, Char3>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Char4, Rest/binary>>) when Char1 band 248 =:= 240 ->  {<<Char1, Char2, Char3, Char4>>, Rest}.
