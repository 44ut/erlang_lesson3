-module(lesson3_task3).
-export([split/2]).

%
% Function split (<input line>,<separator>)
% both <input line>,<separator> can be utf8 string
%

split(BinText, SeparatorString) ->
    % Determine the types of BinText and SeparatorString
    {BinTextType, ProcessedBinText} = determine_type(BinText),
    {SeparatorType, ProcessedSeparator} = determine_type(SeparatorString),

    % Convert to binary format if it's a list
    FinalBinText = case BinTextType of
        list -> string_to_binary(ProcessedBinText);
        _ -> ProcessedBinText
    end,
    FinalSeparator = case SeparatorType of
        list -> string_to_binary(ProcessedSeparator);
        _ -> ProcessedSeparator
    end,

    % Perform the splitting
    split(FinalBinText, FinalSeparator, <<>>, []).

% Main function for splitting binary text into substrings using a separator.
split(<<>>, _, Acc, Parts) ->   reverse([Acc | Parts]); 
split(BinText, Separator, Acc, Parts) when is_binary(Separator) ->
    {NewAcc, RestBinText, NewSeparator} = process_text(BinText, Separator, Acc, Separator, <<>>),
    case RestBinText of
        <<>> -> reverse([NewAcc | Parts]);
        _ -> split(RestBinText, NewSeparator, <<>>, [NewAcc | Parts])
    end.

% Helper function for processing text and separators.
process_text(BinText, OriginalSeparator, Acc, Separator, PartialMatch) ->   process_char(BinText, OriginalSeparator, Acc, Separator, PartialMatch).

process_char(<<>>, _, Acc, _, PartialMatch) ->  {<<Acc/binary, PartialMatch/binary>>, <<>>, <<>>};

process_char(BinText, OriginalSeparator, Acc, Separator, PartialMatch) ->
    {Char, RestBinText} = next_utf8_char(BinText),
    {SepChar, RestSeparator} = next_utf8_char(Separator),
    % process_char handling current character and separator.
    case Char =:= SepChar of
        true ->
            % Characters match. Handling match case.
            if RestSeparator =:= <<>> ->
                % Separator matched. Return accumulated and remaining text.
                {Acc, RestBinText, OriginalSeparator};
            true ->
                % Partial match found. Continue with rest of the separator.
                process_char(RestBinText, OriginalSeparator, Acc, RestSeparator, <<PartialMatch/binary, Char/binary>>)
            end;
        false ->
            % Characters do not match. Handling mismatch case.
            case PartialMatch of
                <<>> ->
                    % No partial match. Continue with next character.
                    process_char(RestBinText, OriginalSeparator, <<Acc/binary, Char/binary>>, OriginalSeparator, <<>>);
                _ ->
                    % Partial match exists. Reset and retry with BinText.
                    process_char(BinText, OriginalSeparator, <<Acc/binary, PartialMatch/binary>>, OriginalSeparator, <<>>)
            end
    end.

% Converts a string into binary format.
string_to_binary(String) -> string_to_binary(String, <<>>).

string_to_binary([], Acc) -> Acc;
string_to_binary([Char | Rest], Acc) ->
    Utf8Char = utf8_char_to_binary(Char),
    string_to_binary(Rest, <<Acc/binary, Utf8Char/binary>>).
        
% Converts a character to its UTF-8 binary representation.
utf8_char_to_binary(Char) when Char < 128 ->
    <<Char>>;
utf8_char_to_binary(Char) when Char < 2048 ->
    Byte1 = (Char bsr 6) bor 192,
    Byte2 = Char band 63 bor 128,
    <<Byte1, Byte2>>;
utf8_char_to_binary(Char) when Char < 65536 ->
    Byte1 = (Char bsr 12) bor 224,
    Byte2 = (Char bsr 6) band 63 bor 128,
    Byte3 = Char band 63 bor 128,
    <<Byte1, Byte2, Byte3>>;
utf8_char_to_binary(Char) ->
    Byte1 = (Char bsr 18) bor 240,
    Byte2 = (Char bsr 12) band 63 bor 128,
    Byte3 = (Char bsr 6) band 63 bor 128,
    Byte4 = Char band 63 bor 128,
    <<Byte1, Byte2, Byte3, Byte4>>.

% Helper function for extracting the next UTF-8 character in binary format.
next_utf8_char(<<Char1, Rest/binary>>) when Char1 band 128 =:= 0 ->   {<<Char1>>, Rest};
next_utf8_char(<<Char1, Char2, Rest/binary>>) when Char1 band 224 =:= 192 ->  {<<Char1, Char2>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Rest/binary>>) when Char1 band 240 =:= 224 ->   {<<Char1, Char2, Char3>>, Rest};
next_utf8_char(<<Char1, Char2, Char3, Char4, Rest/binary>>) when Char1 band 248 =:= 240 ->  {<<Char1, Char2, Char3, Char4>>, Rest}.

% Helper function for reversing a list.
reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([Head | Tail], Acc) -> reverse(Tail, [Head | Acc]).

% Determine the type of a variable: list or binary string
determine_type(Value) ->
    try
        % Check if the value is a binary string
        _ = <<Value/binary>>,
        {binary, Value}
    catch
        % If the previous check fails, consider it a list
        _:_ -> {list, Value}
    end.
