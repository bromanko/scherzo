-module(scherzo_lock_ffi).
-export([acquire/2, release/2]).

acquire(Path, Body) ->
    try
        PathList = to_list(Path),
        BodyBin = unicode:characters_to_binary(Body),
        case file:open(PathList, [write, exclusive]) of
            {ok, IoDevice} -> write_lock_body(IoDevice, PathList, BodyBin);
            {error, eexist} -> {error, <<"exists">>};
            {error, Reason} -> {error, tagged_error(open, reason_to_binary(Reason))}
        end
    catch
        Class:CatchReason -> {error, tagged_error(unexpected_ffi_failure, format_error(Class, CatchReason))}
    end.

write_lock_body(IoDevice, PathList, BodyBin) ->
    case file:write(IoDevice, BodyBin) of
        ok -> {ok, IoDevice};
        {error, Reason} ->
            _ = file:close(IoDevice),
            _ = file:delete(PathList),
            {error, tagged_error(write, reason_to_binary(Reason))}
    end.

release(IoDevice, Path) ->
    _ = file:close(IoDevice),
    _ = file:delete(to_list(Path)),
    nil.

tagged_error(Phase, Reason) when is_atom(Phase) ->
    PhaseBin = atom_to_binary(Phase, utf8),
    ReasonBin = reason_to_binary(Reason),
    <<PhaseBin/binary, ":", ReasonBin/binary>>.

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
