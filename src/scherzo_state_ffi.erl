-module(scherzo_state_ffi).
-include_lib("kernel/include/file.hrl").
-export([append_line/3, append_lines/3, fold_lines/3, with_ledger_lock/2, system_time_millisecond/0, file_fingerprint/1]).

append_line(Path, Line, Fsync) ->
    LineBin = to_binary(Line),
    append_lines(Path, <<LineBin/binary, "\n">>, Fsync).

append_lines(Path, Contents, Fsync) ->
    try
        PathList = to_list(Path),
        ContentsBin = to_binary(Contents),
        case file:open(PathList, [append, binary]) of
            {ok, IoDevice} -> append_open_device(IoDevice, ContentsBin, Fsync);
            {error, Reason} -> {error, tagged_error(open, reason_to_binary(Reason))}
        end
    catch
        Class:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(Class, CatchReason))}
    end.

append_open_device(IoDevice, ContentsBin, Fsync) ->
    Result = write_and_maybe_sync(IoDevice, ContentsBin, Fsync),
    CloseResult = file:close(IoDevice),
    case {Result, CloseResult} of
        {ok, ok} -> {ok, nil};
        {{error, Phase, Reason}, _} -> {error, tagged_error(Phase, reason_to_binary(Reason))};
        {ok, {error, Reason}} -> {error, tagged_error(close, reason_to_binary(Reason))}
    end.

fold_lines(Path, Initial, Step) ->
    try
        PathList = to_list(Path),
        case file:open(PathList, [read, binary]) of
            {ok, IoDevice} -> fold_open_device(IoDevice, Initial, Step);
            {error, Reason} -> {error, tagged_error(open, reason_to_binary(Reason))}
        end
    catch
        Class:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(Class, CatchReason))}
    end.

fold_open_device(IoDevice, Initial, Step) ->
    Result = fold_lines_loop(IoDevice, Initial, Step, none, 1),
    CloseResult = file:close(IoDevice),
    case {Result, CloseResult} of
        {{ok, Acc}, ok} -> {ok, Acc};
        {{error, Phase, Reason}, _} -> {error, tagged_error(Phase, reason_to_binary(Reason))};
        {{ok, _Acc}, {error, Reason}} -> {error, tagged_error(close, reason_to_binary(Reason))}
    end.

with_ledger_lock(Key, Operation) ->
    global:trans({{scherzo_ledger, to_binary(Key)}, self()}, Operation).

system_time_millisecond() ->
    erlang:system_time(millisecond).

file_fingerprint(Path) ->
    try
        PathList = to_list(Path),
        case file:read_file_info(PathList, [{time, posix}]) of
            {ok, Info} ->
                {ok, {
                    true,
                    Info#file_info.size,
                    Info#file_info.mtime * 1000,
                    Info#file_info.ctime * 1000,
                    Info#file_info.inode
                }};
            {error, enoent} -> {ok, {false, 0, 0, 0, 0}};
            {error, Reason} -> {error, reason_to_binary(Reason)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

write_and_maybe_sync(IoDevice, ContentsBin, Fsync) ->
    case file:write(IoDevice, ContentsBin) of
        ok ->
            case Fsync of
                true ->
                    case file:sync(IoDevice) of
                        ok -> ok;
                        {error, Reason} -> {error, sync, Reason}
                    end;
                _ -> ok
            end;
        {error, Reason} -> {error, write, Reason}
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
        {error, Reason} -> {error, read, Reason}
    end;
fold_lines_loop(IoDevice, Acc, Step, {PreviousLine, PreviousLineNumber}, NextLineNumber) ->
    case file:read_line(IoDevice) of
        eof -> call_step(Step, Acc, PreviousLine, PreviousLineNumber, true);
        {ok, Line} ->
            case call_step(Step, Acc, PreviousLine, PreviousLineNumber, false) of
                {ok, NextAcc} ->
                    fold_lines_loop(
                        IoDevice,
                        NextAcc,
                        Step,
                        {strip_trailing_newline(Line), NextLineNumber},
                        NextLineNumber + 1
                    );
                {error, step, Reason} -> {error, step, Reason}
            end;
        {error, Reason} -> {error, read, Reason}
    end.

call_step(Step, Acc, Line, LineNumber, IsLast) ->
    try {ok, Step(Acc, Line, LineNumber, IsLast)}
    catch
        Class:Reason -> {error, step, format_error(Class, Reason)}
    end.

strip_trailing_newline(<<>>) -> <<>>;
strip_trailing_newline(Line) ->
    Size = byte_size(Line),
    case binary:at(Line, Size - 1) of
        $\n -> binary:part(Line, 0, Size - 1);
        _ -> Line
    end.

tagged_error(Phase, Reason) when is_atom(Phase) ->
    PhaseBin = atom_to_binary(Phase, utf8),
    ReasonBin = reason_to_binary(Reason),
    <<PhaseBin/binary, ":", ReasonBin/binary>>.

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
