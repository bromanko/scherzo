-module(scherzo_control_ffi).

-export([
    dynamic_to_json/1,
    listen/2,
    accept/1,
    connect/3,
    send_line/3,
    recv_line/2,
    close_socket/1,
    close_listener/1,
    bound_port/1,
    generate_token/1,
    chmod_private/1,
    getenv/1
]).

-define(MAX_CONTROL_LINE_BYTES, 8388608).

dynamic_to_json(Value) -> json:encode(Value).

listen(Host, Port) ->
    try
        case parse_loopback_host(Host) of
            {ok, Ip} ->
                case gen_tcp:listen(Port, [
                    binary,
                    {active, false},
                    {packet, line},
                    {packet_size, ?MAX_CONTROL_LINE_BYTES},
                    {ip, Ip},
                    {reuseaddr, true},
                    {send_timeout, 5000},
                    {send_timeout_close, true}
                ]) of
                    {ok, Listener} -> {ok, Listener};
                    {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
                end;
            {error, Reason} -> {error, Reason}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

accept(Listener) ->
    try
        case gen_tcp:accept(Listener) of
            {ok, Socket} -> {ok, Socket};
            {error, closed} -> {error, <<"closed">>};
            {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

connect(Host, Port, TimeoutMs) ->
    try
        Timeout = normalize_timeout(TimeoutMs),
        case parse_loopback_host(Host) of
            {ok, Ip} ->
                case gen_tcp:connect(Ip, Port, [
                    binary,
                    {active, false},
                    {packet, line},
                    {packet_size, ?MAX_CONTROL_LINE_BYTES},
                    {send_timeout, Timeout},
                    {send_timeout_close, true}
                ], Timeout) of
                    {ok, Socket} -> {ok, Socket};
                    {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
                end;
            {error, Reason} -> {error, Reason}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

send_line(Socket, Line, _TimeoutMs) ->
    try
        case gen_tcp:send(Socket, [Line, <<"\n">>]) of
            ok -> {ok, nil};
            {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

recv_line(Socket, TimeoutMs) ->
    try
        Timeout = normalize_timeout(TimeoutMs),
        recv_line_acc(Socket, Timeout, <<>>)
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

recv_line_acc(Socket, Timeout, Acc) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Chunk} ->
            Next = <<Acc/binary, Chunk/binary>>,
            case byte_size(Next) > ?MAX_CONTROL_LINE_BYTES of
                true -> {error, <<"line_too_long">>};
                false ->
                    case ends_with_newline(Chunk) of
                        true -> {ok, trim_newline(Next)};
                        false -> recv_line_acc(Socket, Timeout, Next)
                    end
            end;
        {error, timeout} -> {error, <<"timeout">>};
        {error, closed} -> {error, <<"closed">>};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

close_socket(Socket) ->
    try
        catch gen_tcp:close(Socket),
        nil
    catch
        _Class:_Reason -> nil
    end.

close_listener(Listener) ->
    try
        catch gen_tcp:close(Listener),
        nil
    catch
        _Class:_Reason -> nil
    end.

bound_port(Listener) ->
    try
        case inet:sockname(Listener) of
            {ok, {_Ip, Port}} -> Port;
            _ -> 0
        end
    catch
        _Class:_Reason -> 0
    end.

generate_token(Bytes) ->
    try
        Count = case Bytes of
            N when is_integer(N), N > 0 -> N;
            _ -> 32
        end,
        Token = base64:encode(crypto:strong_rand_bytes(Count)),
        {ok, Token}
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

chmod_private(Path) ->
    try
        case file:change_mode(to_list(Path), 8#600) of
            ok -> {ok, nil};
            {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

getenv(Name) ->
    try
        case os:getenv(to_list(Name)) of
            false -> {error, <<"not_found">>};
            Value -> {ok, unicode:characters_to_binary(Value)}
        end
    catch
        Class:CatchReason -> {error, format_error(Class, CatchReason)}
    end.

parse_loopback_host(Host) ->
    case to_list(Host) of
        "127.0.0.1" -> {ok, {127, 0, 0, 1}};
        "localhost" -> {ok, {127, 0, 0, 1}};
        _ -> {error, <<"non_loopback_host_rejected">>}
    end.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

ends_with_newline(<<>>) -> false;
ends_with_newline(Line) when is_binary(Line) ->
    binary:at(Line, byte_size(Line) - 1) =:= $\n.

trim_newline(Line) when is_binary(Line) ->
    trim_byte(trim_byte(Line, $\n), $\r).

trim_byte(Line, Byte) ->
    Size = byte_size(Line),
    case Size > 0 andalso binary:at(Line, Size - 1) =:= Byte of
        true -> binary:part(Line, 0, Size - 1);
        false -> Line
    end.

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value;
to_list(Value) -> unicode:characters_to_list(io_lib:format("~p", [Value])).
