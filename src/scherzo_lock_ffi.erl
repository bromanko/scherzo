-module(scherzo_lock_ffi).
-export([acquire/2, release/2]).

acquire(Path, Body) ->
    PathList = to_list(Path),
    BodyBin = unicode:characters_to_binary(Body),
    case file:open(PathList, [write, exclusive]) of
        {ok, IoDevice} ->
            case file:write(IoDevice, BodyBin) of
                ok -> {ok, IoDevice};
                {error, Reason} ->
                    _ = file:close(IoDevice),
                    _ = file:delete(PathList),
                    {error, atom_to_binary(Reason, utf8)}
            end;
        {error, eexist} -> {error, <<"exists">>};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

release(IoDevice, Path) ->
    _ = file:close(IoDevice),
    _ = file:delete(to_list(Path)),
    nil.

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
