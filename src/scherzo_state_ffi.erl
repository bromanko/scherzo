-module(scherzo_state_ffi).
-export([append_line/3, append_lines/3, fold_lines/3, with_ledger_lock/2, system_time_millisecond/0]).

append_line(Path, Line, Fsync) ->
    LineBin = to_binary(Line),
    append_lines(Path, <<LineBin/binary, "\n">>, Fsync).

append_lines(Path, Contents, Fsync) ->
    try
        PathList = to_list(Path),
        ContentsBin = to_binary(Contents),
        case file:open(PathList, [append, binary]) of
            {ok, IoDevice} ->
                Result = write_and_maybe_sync(IoDevice, ContentsBin, Fsync),
                CloseResult = file:close(IoDevice),
                case {Result, CloseResult} of
                    {ok, ok} -> {ok, nil};
                    {{error, Reason}, _} -> {error, reason_to_binary(Reason)};
                    {ok, {error, Reason}} -> {error, reason_to_binary(Reason)}
                end;
            {error, Reason} -> {error, reason_to_binary(Reason)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

fold_lines(Path, Initial, Step) ->
    try
        PathList = to_list(Path),
        case file:open(PathList, [read, binary]) of
            {ok, IoDevice} ->
                try
                    case fold_lines_loop(IoDevice, Initial, Step, none, 1) of
                        {ok, Acc} -> {ok, Acc};
                        {error, Reason} -> {error, reason_to_binary(Reason)}
                    end
                catch
                    InnerClass:InnerReason -> {error, format_error(InnerClass, InnerReason)}
                after
                    _ = file:close(IoDevice)
                end;
            {error, Reason} -> {error, reason_to_binary(Reason)}
        end
    catch
        OuterClass:OuterReason -> {error, format_error(OuterClass, OuterReason)}
    end.

with_ledger_lock(Key, Operation) ->
    global:trans({scherzo_ledger, to_binary(Key)}, Operation).

system_time_millisecond() ->
    erlang:system_time(millisecond).

write_and_maybe_sync(IoDevice, ContentsBin, Fsync) ->
    case file:write(IoDevice, ContentsBin) of
        ok ->
            case Fsync of
                true -> file:sync(IoDevice);
                _ -> ok
            end;
        {error, Reason} -> {error, Reason}
    end.

fold_lines_loop(IoDevice, Acc, Step, none, NextLineNumber) ->
    case file:read_line(IoDevice) of
        eof -> {ok, Acc};
        {ok, Line} ->
            fold_lines_loop(
                IoDevice,
                Acc,
                Step,
                {strip_trailing_newline(Line), NextLineNumber},
                NextLineNumber + 1
            );
        {error, Reason} -> {error, Reason}
    end;
fold_lines_loop(IoDevice, Acc, Step, {PreviousLine, PreviousLineNumber}, NextLineNumber) ->
    case file:read_line(IoDevice) of
        eof -> {ok, Step(Acc, PreviousLine, PreviousLineNumber, true)};
        {ok, Line} ->
            NextAcc = Step(Acc, PreviousLine, PreviousLineNumber, false),
            fold_lines_loop(
                IoDevice,
                NextAcc,
                Step,
                {strip_trailing_newline(Line), NextLineNumber},
                NextLineNumber + 1
            );
        {error, Reason} -> {error, Reason}
    end.

strip_trailing_newline(<<>>) -> <<>>;
strip_trailing_newline(Line) ->
    Size = byte_size(Line),
    case binary:at(Line, Size - 1) of
        $\n -> binary:part(Line, 0, Size - 1);
        _ -> Line
    end.

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) ->
    unicode:characters_to_binary(io_lib:format("~p", [Reason])).

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value;
to_list(Value) -> unicode:characters_to_list(io_lib:format("~p", [Value])).
