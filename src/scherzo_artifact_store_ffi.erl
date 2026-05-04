-module(scherzo_artifact_store_ffi).
-export([write_atomic/2]).

write_atomic(FinalPath, Contents) ->
    Final = to_list(FinalPath),
    Temp = Final ++ ".tmp",
    Bytes = to_binary(Contents),
    try
        case file:open(Temp, [write, binary]) of
            {ok, IoDevice} ->
                Write = file:write(IoDevice, Bytes),
                Sync = case Write of ok -> file:sync(IoDevice); Error -> Error end,
                Close = file:close(IoDevice),
                case {Write, Sync, Close} of
                    {ok, ok, ok} ->
                        case file:rename(Temp, Final) of
                            ok ->
                                _ = sync_parent_dir(Final),
                                {ok, nil};
                            {error, Reason} -> {error, reason_to_binary(Reason)}
                        end;
                    {{error, Reason}, _, _} -> {error, reason_to_binary(Reason)};
                    {_, {error, Reason}, _} -> {error, reason_to_binary(Reason)};
                    {_, _, {error, Reason}} -> {error, reason_to_binary(Reason)}
                end;
            {error, Reason} -> {error, reason_to_binary(Reason)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

sync_parent_dir(Final) ->
    Dir = filename:dirname(Final),
    case file:open(Dir, [read]) of
        {ok, IoDevice} ->
            Result = file:sync(IoDevice),
            _ = file:close(IoDevice),
            Result;
        {error, _} -> ok
    end.

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])). 

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value;
to_list(Value) -> unicode:characters_to_list(io_lib:format("~p", [Value])). 
