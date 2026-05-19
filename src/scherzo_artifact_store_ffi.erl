-module(scherzo_artifact_store_ffi).
-export([write_atomic/2, read_file/1, write_immutable/2]).

write_atomic(FinalPath, Contents) ->
    try
        case validate_final_path(FinalPath) of
            {ok, Final} -> write_atomic_checked(Final, to_binary(Contents));
            {error, Error} -> {error, Error}
        end
    catch
        CatchClass:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(CatchClass, CatchReason))}
    end.

read_file(Path) ->
    try
        case safe_to_list(Path) of
            {ok, ""} -> {error, tagged_error(read, <<"enoent">>)};
            {ok, SafePath} ->
                case file:read_file(SafePath) of
                    {ok, Contents} -> {ok, Contents};
                    {error, Reason} -> {error, tagged_error(read, reason_to_binary(Reason))}
                end;
            error -> {error, tagged_error(read, <<"invalid_path">>)}
        end
    catch
        CatchClass:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(CatchClass, CatchReason))}
    end.

write_immutable(FinalPath, Contents) ->
    try
        case validate_final_path(FinalPath) of
            {ok, Final} -> write_immutable_checked(Final, to_binary(Contents));
            {error, Error} -> {error, Error}
        end
    catch
        CatchClass:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(CatchClass, CatchReason))}
    end.

write_atomic_checked(Final, Bytes) ->
    Temp = unique_temp_path(Final),
    case file:open(Temp, [write, binary, exclusive]) of
        {ok, IoDevice} -> write_sync_close_rename(Final, Temp, IoDevice, Bytes);
        {error, eexist} -> write_atomic_checked(Final, Bytes);
        {error, Reason} -> {error, tagged_error(open_temp, reason_to_binary(Reason))}
    end.

write_immutable_checked(Final, Bytes) ->
    ensure_parent_dir(Final),
    case file:read_file(Final) of
        {ok, Existing} when Existing =:= Bytes -> {ok, <<"existing">>};
        {ok, _Different} -> {ok, <<"conflict">>};
        {error, enoent} -> write_immutable_once(Final, Bytes);
        {error, Reason} -> {error, tagged_error(read, reason_to_binary(Reason))}
    end.

write_immutable_once(Final, Bytes) ->
    case file:open(Final, [write, binary, exclusive]) of
        {ok, IoDevice} ->
            case write_sync_close_keep(IoDevice, Bytes) of
                ok ->
                    case sync_parent_dir(Final) of
                        ok -> {ok, <<"written">>};
                        {error, Reason} -> {error, tagged_error(sync_parent, reason_to_binary(Reason))}
                    end;
                {error, Error} -> {error, Error}
            end;
        {error, eexist} ->
            case file:read_file(Final) of
                {ok, Existing} when Existing =:= Bytes -> {ok, <<"existing">>};
                {ok, _Different} -> {ok, <<"conflict">>};
                {error, Reason} -> {error, tagged_error(read, reason_to_binary(Reason))}
            end;
        {error, Reason} -> {error, tagged_error(open_temp, reason_to_binary(Reason))}
    end.

write_sync_close_keep(IoDevice, Bytes) ->
    Write = file:write(IoDevice, Bytes),
    Sync = case Write of
        ok -> file:sync(IoDevice);
        {error, _} -> skipped
    end,
    Close = file:close(IoDevice),
    case {Write, Sync, Close} of
        {ok, ok, ok} -> ok;
        {{error, Reason}, _, _} -> {error, tagged_error(write_temp, reason_to_binary(Reason))};
        {_, {error, Reason}, _} -> {error, tagged_error(sync_temp, reason_to_binary(Reason))};
        {_, _, {error, Reason}} -> {error, tagged_error(close_temp, reason_to_binary(Reason))}
    end.

write_sync_close_rename(Final, Temp, IoDevice, Bytes) ->
    Write = file:write(IoDevice, Bytes),
    Sync = case Write of
        ok -> file:sync(IoDevice);
        {error, _} -> skipped
    end,
    Close = file:close(IoDevice),
    case {Write, Sync, Close} of
        {ok, ok, ok} -> rename_and_sync_parent(Final, Temp);
        {{error, Reason}, _, _} -> cleanup_or_error(Temp, tagged_error(write_temp, reason_to_binary(Reason)));
        {_, {error, Reason}, _} -> cleanup_or_error(Temp, tagged_error(sync_temp, reason_to_binary(Reason)));
        {_, _, {error, Reason}} -> cleanup_or_error(Temp, tagged_error(close_temp, reason_to_binary(Reason)))
    end.

rename_and_sync_parent(Final, Temp) ->
    case file:rename(Temp, Final) of
        ok ->
            case sync_parent_dir(Final) of
                ok -> {ok, nil};
                {error, Reason} -> {error, tagged_error(sync_parent, reason_to_binary(Reason))}
            end;
        {error, Reason} -> cleanup_or_error(Temp, tagged_error(rename, reason_to_binary(Reason)))
    end.

cleanup_or_error(Temp, OriginalError) ->
    case file:delete(Temp) of
        ok -> {error, OriginalError};
        {error, enoent} -> {error, OriginalError};
        {error, Reason} -> {error, tagged_error(cleanup_temp, <<OriginalError/binary, "; cleanup:", (reason_to_binary(Reason))/binary>>)}
    end.

sync_parent_dir(Final) ->
    Dir = filename:dirname(Final),
    case file:open(Dir, [read]) of
        {ok, IoDevice} ->
            Result = file:sync(IoDevice),
            _ = file:close(IoDevice),
            Result;
        {error, _Unsupported} -> ok
    end.

ensure_parent_dir(Final) ->
    _ = filelib:ensure_dir(Final),
    ok.

validate_final_path(FinalPath) ->
    case safe_to_list(FinalPath) of
        {ok, ""} -> {error, tagged_error(invalid_path, <<"empty">>)};
        {ok, Final} -> {ok, Final};
        error -> {error, tagged_error(invalid_path, <<"not_string">>)}
    end.

unique_temp_path(Final) ->
    Dir = filename:dirname(Final),
    Base = filename:basename(Final),
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    filename:join(Dir, "." ++ Base ++ ".scherzo-" ++ Unique ++ ".tmp").

tagged_error(Tag, Reason) when is_atom(Tag) ->
    TagBin = atom_to_binary(Tag, utf8),
    ReasonBin = reason_to_binary(Reason),
    <<TagBin/binary, ":", ReasonBin/binary>>.

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])).

safe_to_list(Value) when is_binary(Value) -> {ok, binary_to_list(Value)};
safe_to_list(Value) when is_list(Value) -> {ok, Value};
safe_to_list(_Value) -> error.
