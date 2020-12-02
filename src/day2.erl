-module(day2).
-mode(compile).
-export([
    main/1
]).
-define(DEFAULT_CHUNK_SIZE, "32").


%% ---------------------- %%
%% Via iteration (part 1) %%
%% ---------------------- %%


parse_input_line(<<>>)
->
    []
;

parse_input_line(Line)
->
    % Split row into columns:
    [Range, <<Letter,":">>, Password] = binary:split(Line, <<" ">>, [global]),
    % Split range column into min & max:
    [Min, Max] = binary:split(Range, <<"-">>),
    % Return tuple of columns:
    [{binary_to_integer(Min), binary_to_integer(Max), <<Letter>>, Password}]
.

is_invalid_password_row({Min, Max, Needle, Haystack})
->
    Matches = binary:matches(Haystack, [Needle]),
    case length(Matches) of
        N when N >= Min, N =< Max ->
            true
        ;_ ->
            false
    end
.

find_invalid_rows()
->
    {ok, Lines} = helper:load_input_lines(<<"day2">>),
    Rows = lists:flatmap(fun parse_input_line/1, Lines),
    lists:filter(fun is_invalid_password_row/1, Rows)
.


%% -------------------------- %%
%% Via state machine (part 1) %%
%% -------------------------- %%


%% The parser iterates through the input data byte-by-byte. For each input line, it:
%% - Acquires the Min, Max & Needle values into state.
%% - Scans the rest of the line (the password/haystack) for Needle.
%% - Decrements Min & Max whenever Needle is found.
%% - At the end of the line, checks Min & Max to see if a valid amount of Needles have been seen.
%% - Increments the invalid row counter if an invalid number of Needles have been seen.
%% - Clears Min, Max & Needle state to continue with next line.

%% As input is read byte-by-byte, this could consume a continuous stream of input data,
%% such as from a socket, and will never need to accumulate more memory than is needed to hold
%% the Min, Max, Needle, ChunkSize and InvalidRows count.

% Entrypoint to init state machine state:
parse_io(IoDevice, ChunkSize)
->
    parse_io(
        IoDevice,   % The IO device (file handle) we read input data from when we need it.
        ChunkSize,  % How many bytes of input data we read each time (min 1 byte).
        <<>>,       % Input data we have loaded (init as empty to trigger first read).
        <<>>,       % The min times a needle must be found (resets for each new line).
        unknown,    % The max times a needle must be found (resets for each new line).
        unknown,    % The needle being searched for (resets for each new line).
        0           % Running count of how many invalid entries have been discovered.
    )
.

%% Everything below here is a single function with many clauses:
%% - Each clause matches a particular state of the function arguments.
%% - The first clause that matches is the clause that gets executed.
%% - As clause matching is ordered, the most specific clauses tend to be first.
%%   A generic clause at the top of your function definition can make others unreachable.
%% - Every clause is tail-recursive - the function is continuously calling itself
%%   in order to loop until an end condition is reached (in this case EOF of the input data file).

% This clause only matches when we have no input data left (denoted by <<>> which means an empty binary value).
% Attempt to get some more:
parse_io(IoDevice, ChunkSize, <<>>, Min, Max, Needle, InvalidRows)
->
    % Read chunk of bytes from file handle:
    case file:read(IoDevice, ChunkSize) of
        {ok, Data} ->
            % Acquired new input, continue:
            parse_io(IoDevice, ChunkSize, Data, Min, Max, Needle, InvalidRows)
        ;eof ->
            % No more input - return result:
            InvalidRows
    end
;

%%
%% Control characters
%%

% This clause matches when we see a new line: we know we have been through the whole haystack on this line:
% This clause also only matches if we didn't find the needle enough times to cross our Min & Max threshold values:
parse_io(Io, Cs, <<"\n", RemainderInputBytes/binary>>, Min, Max, _, InvalidRows) when Min =< 0, Max >= 0
->
    % Log to STDOUT:
    io:format("\n\033[F~p invalid passwords found", [InvalidRows + 1]),
    % Increment InvalidRows counter & reset state to begin next row:
    parse_io(Io, Cs, RemainderInputBytes, <<>>, unknown, unknown, InvalidRows + 1)
;
% This clause also matches if we see a new line, but did not match the Min/Max clause above, implying that we
% did not find the needle enough times to cross our Min & Max threshold values:
parse_io(Io, Cs, <<"\n", RemainderInputBytes/binary>>, _, _, _, InvalidRows)
->
    % Reset state to begin next row:
    parse_io(Io, Cs, RemainderInputBytes, <<>>, unknown, unknown, InvalidRows)
;

% This clause matches on seeing a dash, we know we have captured all of Min input.
% The recursive call will move use on to getting Max:
parse_io(Io, Cs, <<"-", RemainderInputBytes/binary>>, Min, unknown, unknown, InvalidRows)
->
    parse_io(
        Io,
        Cs,
        RemainderInputBytes,
        binary_to_integer(Min), % Convert string representation of Min number to actual integer:
        <<>>, % Set Max to an empty binary that next input will be gathered into.
        unknown,
        InvalidRows
    )
;

% This clause matches on seeing a space while needle is still unknown: We know we have captured all of Max input.
% The recursive call will move us on to getting the Needle:
parse_io(Io, Cs, <<" ", RemainderInputBytes/binary>>, Min, Max, unknown, InvalidRows)
->
    parse_io(
        Io,
        Cs,
        RemainderInputBytes,
        Min,
        binary_to_integer(Max), % Convert string representation of Max number to actual integer:
        <<>>, % Set Needle to an empty binary that next input will be gathered into.
        InvalidRows
    )
;

% This clause matches on seeing a colon: we know we have captured all of Needle input.
% The recursive call will move us on to searching rest of line as haystack:
parse_io(Io, Cs, <<":", RemainderInputBytes/binary>>, Min, Max, Needle, InvalidRows)
->
    parse_io(Io, Cs, RemainderInputBytes, Min, Max, Needle, InvalidRows)
;

%%
%% Haystack search
%%

% This clause matches when we have a populated Needle value (which due to order of logic also implies we have Min & Max too).
% The input byte is therefore considered part of the password/haystack so we check our needle against it:
parse_io(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, Min, Max, Needle = <<NeedleByte>>, InvalidRows)
->
    case InputByte of
        NeedleByte ->
            % This clause matches when a needle is found in haystack:
            % Decrementing the Min & Max lets us check how many times we have seen
            % the needle by the time we reach the end of the line.
            parse_io(Io, Cs, RemainderInputBytes, Min - 1, Max - 1, Needle, InvalidRows)
        ;_ ->
            % This clause matches when a needle isn't found in haystack:
            parse_io(Io, Cs, RemainderInputBytes, Min, Max, Needle, InvalidRows)
    end
;

%%
%% Criteria discovery
%%

% This clause matches when Max & Needle are unknown.
% Continue to gather input into Min value (will continue until finding next control character):
parse_io(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, Min, unknown, unknown, InvalidRows)
->
    % Append input to existing Min value:
    parse_io(Io, Cs, RemainderInputBytes, <<Min/binary, InputByte>>, unknown, unknown, InvalidRows)
;

% This clause matches when Needle is unknown.
% Continue to gather input into Max value (will continue until finding next control character):
parse_io(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, Min, Max, unknown, InvalidRows)
->
    % Append input to existing Max value:
    parse_io(Io, Cs, RemainderInputBytes, Min, <<Max/binary, InputByte>>, unknown, InvalidRows)
;

% This clause will match when no other clauses match.
% Continue to gather input into Needle value (will continue until finding next control character):
parse_io(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, Min, Max, Needle, InvalidRows)
->
    % Append input to existing Needle value:
    parse_io(Io, Cs, RemainderInputBytes, Min, Max, <<Needle/binary, InputByte>>, InvalidRows)
.


%% -------------------------- %%
%% Via state machine (part 2) %%
%% -------------------------- %%


parse_io_pt2(IoDevice, ChunkSize)
->
    parse_io_pt2(IoDevice, ChunkSize, <<>>, <<>>, unknown, unknown, 0)
.

parse_io_pt2(IoDevice, ChunkSize, <<>>, Min, Max, Needle, ValidRows)
->
    case file:read(IoDevice, ChunkSize) of
        {ok, Data} ->
            parse_io_pt2(IoDevice, ChunkSize, Data, Min, Max, Needle, ValidRows)
        ;eof ->
            ValidRows
    end
;

%% Control characters

parse_io_pt2(Io, Cs, <<"\n", RemainderInputBytes/binary>>, PosA, PosB, _, ValidRows) when PosA + PosB == -1
->
    io:format("\n\033[F~p valid passwords found", [ValidRows + 1]),
    parse_io_pt2(Io, Cs, RemainderInputBytes, <<>>, unknown, unknown, ValidRows + 1)
;

parse_io_pt2(Io, Cs, <<"\n", RemainderInputBytes/binary>>, _, _, _, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, <<>>, unknown, unknown, ValidRows)
;

parse_io_pt2(Io, Cs, <<"-", RemainderInputBytes/binary>>, PosA, unknown, unknown, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, binary_to_integer(PosA), <<>>, unknown, ValidRows)
;

parse_io_pt2(Io, Cs, <<" ", RemainderInputBytes/binary>>, PosA, PosB, unknown, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, PosA, binary_to_integer(PosB), <<>>, ValidRows)
;

parse_io_pt2(Io, Cs, <<":", RemainderInputBytes/binary>>, PosA, PosB, Needle, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, PosA, PosB, Needle, ValidRows)
;

parse_io_pt2(Io, Cs, <<" ", RemainderInputBytes/binary>>, PosA, PosB, Needle, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, PosA, PosB, Needle, ValidRows)
;

%% Haystack search

parse_io_pt2(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, PosA, PosB, Needle = <<NeedleByte>>, ValidRows)
->
    NewPosA = case {PosA, InputByte} of
        {-1, _} -> -1
        ;{0, _} -> 0
        ;{1, NeedleByte} -> -1
        ;{1, _} -> 0
        ;{_, _} -> PosA - 1
    end,

    NewPosB = case {PosB, InputByte} of
        {-1, _} -> -1
        ;{0, _} -> 0
        ;{1, NeedleByte} -> -1
        ;{1, _} -> 0
        ;{_, _} -> PosB - 1
    end,

    parse_io_pt2(Io, Cs, RemainderInputBytes, NewPosA, NewPosB, Needle, ValidRows)
;

%% Criteria discovery

parse_io_pt2(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, PosA, unknown, unknown, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, <<PosA/binary, InputByte>>, unknown, unknown, ValidRows)
;

parse_io_pt2(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, PosA, PosB, unknown, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, PosA, <<PosB/binary, InputByte>>, unknown, ValidRows)
;

parse_io_pt2(Io, Cs, <<InputByte, RemainderInputBytes/binary>>, PosA, PosB, Needle, ValidRows)
->
    parse_io_pt2(Io, Cs, RemainderInputBytes, PosA, PosB, <<Needle/binary, InputByte>>, ValidRows)
.


%% ------- %%
%% Helpers %%
%% ------- %%


print_result(Result)
->
    io:format("\n~p\n", [Result])
.

%% ---------- %%
%% Entrypoint %%
%% ---------- %%

main(["sensible"])
->
    InvalidRows = find_invalid_rows(),
    print_result(length(InvalidRows))
;

main(["silly", ChunkSize])
->
    InputFilePath = helper:get_input_path(<<"day2">>),
    {ok, InputFileHandle} = file:open(InputFilePath, [read, binary]),
    InvalidRowCount = parse_io(InputFileHandle, list_to_integer(ChunkSize)),
    file:close(InputFileHandle),

    print_result(InvalidRowCount)
;

main(["silly2", ChunkSize])
->
    InputFilePath = helper:get_input_path(<<"day2">>),
    {ok, InputFileHandle} = file:open(InputFilePath, [read, binary]),
    InvalidRowCount = parse_io_pt2(InputFileHandle, list_to_integer(ChunkSize)),
    file:close(InputFileHandle),

    print_result(InvalidRowCount)
;

main(["silly"])
->
    main(["silly", ?DEFAULT_CHUNK_SIZE])
;

main(["silly2"])
->
    main(["silly2", ?DEFAULT_CHUNK_SIZE])
;

main([])
->
    main(["silly"])
;

main(_)
->
    io:format("Unexpected input\n"),
    halt(1)
.
