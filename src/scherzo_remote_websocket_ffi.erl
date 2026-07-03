-module(scherzo_remote_websocket_ffi).

-export([
    websocket_connect/3,
    websocket_send_text/3,
    websocket_recv_text/2,
    websocket_close/1,
    start_fake_ui_server/3,
    stop_fake_ui_server/1,
    fake_ui_server_port/1
]).

-define(WS_GUID, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>).

websocket_connect(Url, Credential, TimeoutMs) ->
    try
        Timeout = normalize_timeout(TimeoutMs),
        case parse_ws_url(Url) of
            {ok, #{scheme := Scheme, host := Host, port := Port, path := Path}} ->
                case open_socket(Scheme, Host, Port, Timeout) of
                    {ok, {Transport, Socket}} ->
                        case websocket_handshake(Transport, Socket, Scheme, Host, Port, Path, Credential, Timeout) of
                            ok -> {ok, {websocket, Transport, Socket}};
                            {error, Reason} ->
                                close_transport(Transport, Socket),
                                {error, Reason}
                        end;
                    {error, Reason} -> {error, Reason}
                end;
            {error, Reason} -> {error, Reason}
        end
    catch
        CatchClass:CatchReason -> {error, format_error(CatchClass, CatchReason)}
    end.

websocket_send_text({websocket, Transport, Socket}, Payload, TimeoutMs) ->
    try
        Timeout = normalize_timeout(TimeoutMs),
        Frame = encode_client_frame(1, unicode:characters_to_binary(Payload)),
        send_transport(Transport, Socket, Frame, Timeout)
    catch
        CatchClass:CatchReason -> {error, format_error(CatchClass, CatchReason)}
    end.

websocket_recv_text({websocket, Transport, Socket}, TimeoutMs) ->
    try
        Timeout = normalize_timeout(TimeoutMs),
        recv_text_frame(Transport, Socket, Timeout)
    catch
        CatchClass:CatchReason -> {error, format_error(CatchClass, CatchReason)}
    end.

websocket_close({websocket, Transport, Socket}) ->
    close_transport(Transport, Socket),
    nil;
websocket_close(_) -> nil.

start_fake_ui_server(Credential, TranscriptPath, StatusSubject) ->
    try
        case gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {ip, {127,0,0,1}}, {reuseaddr, true}]) of
            {ok, Listener} ->
                {ok, {_Ip, Port}} = inet:sockname(Listener),
                Pid = spawn(fun() -> fake_server_loop(Listener, Credential, TranscriptPath, StatusSubject, 0) end),
                {ok, {Pid, Port}};
            {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
        end
    catch
        CatchClass:CatchReason -> {error, format_error(CatchClass, CatchReason)}
    end.

stop_fake_ui_server({Pid, _Port}) ->
    exit(Pid, kill),
    nil;
stop_fake_ui_server(_) -> nil.

fake_ui_server_port({_Pid, Port}) -> Port;
fake_ui_server_port(_) -> 0.

websocket_handshake(Transport, Socket, Scheme, Host, Port, Path, Credential, Timeout) ->
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    HostHeader = host_header(Scheme, Host, Port),
    Request = iolist_to_binary([
        <<"GET ">>, Path, <<" HTTP/1.1\r\n">>,
        <<"Host: ">>, HostHeader, <<"\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Version: 13\r\n">>,
        <<"Sec-WebSocket-Key: ">>, Key, <<"\r\n">>,
        <<"Authorization: Bearer ">>, Credential, <<"\r\n\r\n">>
    ]),
    case send_transport(Transport, Socket, Request, Timeout) of
        {ok, _} ->
            case recv_http_message(Transport, Socket, Timeout) of
                {ok, HeadersBin, Body} ->
                    validate_handshake_response(HeadersBin, Body, Key);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

validate_handshake_response(HeadersBin, Body, Key) ->
    case binary:split(HeadersBin, <<"\r\n">>, [global]) of
        [StatusLine | HeaderLines] ->
            case StatusLine of
                <<"HTTP/1.1 101", _/binary>> ->
                    ExpectedAccept = base64:encode(crypto:hash(sha, <<Key/binary, ?WS_GUID/binary>>)),
                    case header_value(HeaderLines, <<"sec-websocket-accept">>) of
                        {ok, ExpectedAccept} -> ok;
                        {ok, _} -> {error, <<"websocket_bad_accept">>};
                        error -> {error, <<"websocket_missing_accept">>}
                    end;
                _ -> {error, websocket_status_error(StatusLine, Body)}
            end;
        _ -> {error, <<"websocket_bad_handshake">>}
    end.

recv_text_frame(Transport, Socket, Timeout) ->
    case recv_exact(Transport, Socket, 2, Timeout) of
        {ok, <<Fin:1, _Rsv:3, OpCode:4, Masked:1, Len0:7>>} ->
            case recv_payload_length(Transport, Socket, Len0, Timeout) of
                {ok, PayloadLen} ->
                    case control_frame_payload_ok(OpCode, PayloadLen) of
                        true ->
                            case recv_masking_key(Transport, Socket, Masked, Timeout) of
                                {ok, MaskingKey} ->
                                    case recv_exact(Transport, Socket, PayloadLen, Timeout) of
                                        {ok, Payload0} ->
                                            Payload = maybe_unmask_payload(Payload0, MaskingKey),
                                            case {Fin, OpCode} of
                                                {_Fin, 1} -> {ok, binary_to_utf8(Payload)};
                                                {_Fin, 8} -> {error, websocket_close_error(Payload)};
                                                {_Fin, 9} ->
                                                    _ = send_transport(Transport, Socket, encode_client_frame(10, Payload), Timeout),
                                                    recv_text_frame(Transport, Socket, Timeout);
                                                {_Fin, 10} -> recv_text_frame(Transport, Socket, Timeout);
                                                _ -> recv_text_frame(Transport, Socket, Timeout)
                                            end;
                                        {error, Reason} -> {error, Reason}
                                    end;
                                {error, Reason} -> {error, Reason}
                            end;
                        false -> {error, <<"websocket_control_frame_too_large">>}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

websocket_close_error(<<Code:16/big-unsigned-integer, Reason/binary>>) ->
    ReasonText = truncate_binary(binary_to_utf8(Reason), 200),
    <<"websocket_close:", (integer_to_binary(Code))/binary, ":", ReasonText/binary>>;
websocket_close_error(_) -> <<"closed">>.

control_frame_payload_ok(OpCode, PayloadLen) when OpCode >= 8, PayloadLen =< 125 -> true;
control_frame_payload_ok(OpCode, _PayloadLen) when OpCode >= 8 -> false;
control_frame_payload_ok(_OpCode, _PayloadLen) -> true.

websocket_status_error(StatusLine, Body) ->
    Code = status_code(StatusLine),
    BodyText = truncate_binary(binary_to_utf8(Body), 200),
    <<"websocket_http_status:", Code/binary, ":", BodyText/binary>>.

status_code(<<"HTTP/1.1 ", Code:3/binary, _/binary>>) -> Code;
status_code(<<"HTTP/1.0 ", Code:3/binary, _/binary>>) -> Code;
status_code(_) -> <<"unknown">>.

truncate_binary(Bin, Max) when byte_size(Bin) =< Max -> Bin;
truncate_binary(Bin, Max) -> <<(binary:part(Bin, 0, Max))/binary, "...">>.

recv_payload_length(Transport, Socket, 126, Timeout) ->
    case recv_exact(Transport, Socket, 2, Timeout) of
        {ok, <<Len:16/big-unsigned-integer>>} -> {ok, Len};
        {error, Reason} -> {error, Reason}
    end;
recv_payload_length(Transport, Socket, 127, Timeout) ->
    case recv_exact(Transport, Socket, 8, Timeout) of
        {ok, <<Len:64/big-unsigned-integer>>} -> {ok, Len};
        {error, Reason} -> {error, Reason}
    end;
recv_payload_length(_Transport, _Socket, Len, _Timeout) -> {ok, Len}.

recv_masking_key(Transport, Socket, 1, Timeout) -> recv_exact(Transport, Socket, 4, Timeout);
recv_masking_key(_Transport, _Socket, 0, _Timeout) -> {ok, none}.

maybe_unmask_payload(Payload, none) -> Payload;
maybe_unmask_payload(Payload, MaskingKey) -> xor_mask(Payload, MaskingKey).

encode_client_frame(OpCode, Payload) ->
    MaskingKey = crypto:strong_rand_bytes(4),
    MaskedPayload = xor_mask(Payload, MaskingKey),
    Header = frame_header(1, OpCode, 1, byte_size(Payload)),
    <<Header/binary, MaskingKey/binary, MaskedPayload/binary>>.

encode_server_text_frame(Payload) ->
    Header = frame_header(1, 1, 0, byte_size(Payload)),
    <<Header/binary, Payload/binary>>.

frame_header(Fin, OpCode, Masked, Len) when Len < 126 ->
    <<Fin:1, 0:3, OpCode:4, Masked:1, Len:7>>;
frame_header(Fin, OpCode, Masked, Len) when Len < 65536 ->
    <<Fin:1, 0:3, OpCode:4, Masked:1, 126:7, Len:16/big-unsigned-integer>>;
frame_header(Fin, OpCode, Masked, Len) ->
    <<Fin:1, 0:3, OpCode:4, Masked:1, 127:7, Len:64/big-unsigned-integer>>.

xor_mask(Payload, MaskingKey) ->
    xor_mask(Payload, MaskingKey, 0, <<>>).

xor_mask(<<>>, _MaskingKey, _Index, Acc) -> Acc;
xor_mask(<<Byte, Rest/binary>>, MaskingKey, Index, Acc) ->
    MaskByte = binary:at(MaskingKey, Index rem 4),
    xor_mask(Rest, MaskingKey, Index + 1, <<Acc/binary, (Byte bxor MaskByte)>>).

open_socket(<<"ws">>, Host, Port, Timeout) ->
    case gen_tcp:connect(to_list(Host), Port, [binary, {active, false}, {packet, raw}, {send_timeout, Timeout}, {send_timeout_close, true}], Timeout) of
        {ok, Socket} -> {ok, {tcp, Socket}};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end;
open_socket(<<"wss">>, Host, Port, Timeout) ->
    _ = application:ensure_all_started(ssl),
    case ssl:connect(to_list(Host), Port, [{active, false}, {mode, binary}], Timeout) of
        {ok, Socket} -> {ok, {ssl, Socket}};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end;
open_socket(_, _Host, _Port, _Timeout) -> {error, <<"unsupported_websocket_scheme">>}.

send_transport(tcp, Socket, Payload, _Timeout) ->
    case gen_tcp:send(Socket, Payload) of
        ok -> {ok, nil};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end;
send_transport(ssl, Socket, Payload, _Timeout) ->
    case ssl:send(Socket, Payload) of
        ok -> {ok, nil};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end.

recv_transport(tcp, Socket, Count, Timeout) ->
    case gen_tcp:recv(Socket, Count, Timeout) of
        {ok, Data} -> {ok, Data};
        {error, timeout} -> {error, <<"timeout">>};
        {error, closed} -> {error, <<"closed">>};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end;
recv_transport(ssl, Socket, Count, Timeout) ->
    case ssl:recv(Socket, Count, Timeout) of
        {ok, Data} -> {ok, Data};
        {error, timeout} -> {error, <<"timeout">>};
        {error, closed} -> {error, <<"closed">>};
        {error, Reason} -> {error, normalize_transport_error(Reason)}
    end.

close_transport(tcp, Socket) -> catch gen_tcp:close(Socket), ok;
close_transport(ssl, Socket) -> catch ssl:close(Socket), ok.

recv_exact(_Transport, _Socket, 0, _Timeout) -> {ok, <<>>};
recv_exact(Transport, Socket, Count, Timeout) ->
    case recv_transport(Transport, Socket, Count, Timeout) of
        {ok, Data} when byte_size(Data) =:= Count -> {ok, Data};
        {ok, Data} when byte_size(Data) < Count ->
            case recv_exact(Transport, Socket, Count - byte_size(Data), Timeout) of
                {ok, Rest} -> {ok, <<Data/binary, Rest/binary>>};
                {error, Reason} -> {error, Reason}
            end;
        {ok, Data} -> {ok, binary:part(Data, 0, Count)};
        {error, Reason} -> {error, Reason}
    end.

recv_http_message(Transport, Socket, Timeout) ->
    recv_http_message(Transport, Socket, Timeout, <<>>).

recv_http_message(Transport, Socket, Timeout, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        {Pos, 4} ->
            HeadersSize = Pos + 4,
            Headers = binary:part(Acc, 0, HeadersSize),
            Rest = binary:part(Acc, HeadersSize, byte_size(Acc) - HeadersSize),
            BodyLength = content_length(Headers),
            case byte_size(Rest) >= BodyLength of
                true ->
                    Body = binary:part(Rest, 0, BodyLength),
                    {ok, Headers, Body};
                false ->
                    case recv_exact(Transport, Socket, BodyLength - byte_size(Rest), Timeout) of
                        {ok, Extra} -> {ok, Headers, <<Rest/binary, Extra/binary>>};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        nomatch ->
            case recv_transport(Transport, Socket, 0, Timeout) of
                {ok, Data} -> recv_http_message(Transport, Socket, Timeout, <<Acc/binary, Data/binary>>);
                {error, Reason} -> {error, Reason}
            end
    end.

content_length(Headers) ->
    HeaderLines = binary:split(Headers, <<"\r\n">>, [global]),
    case header_value(HeaderLines, <<"content-length">>) of
        {ok, Value} -> binary_to_integer(string:trim(Value));
        error -> 0
    end.

header_value([], _Name) -> error;
header_value([Line | Rest], Name) ->
    case split_once(Line, <<":">>) of
        {ok, Key, Value} ->
            case lowercase_binary(string:trim(Key)) =:= Name of
                true -> {ok, string:trim(Value)};
                false -> header_value(Rest, Name)
            end;
        error -> header_value(Rest, Name)
    end.

host_header(<<"ws">>, Host, Port) ->
    case Port =:= 80 of
        true -> Host;
        false -> <<Host/binary, ":", (integer_to_binary(Port))/binary>>
    end;
host_header(<<"wss">>, Host, Port) ->
    case Port =:= 443 of
        true -> Host;
        false -> <<Host/binary, ":", (integer_to_binary(Port))/binary>>
    end.

parse_ws_url(Url) ->
    try
        Map = uri_string:parse(to_list(Url)),
        Scheme = map_get_binary(scheme, Map),
        Host = map_get_binary(host, Map),
        RawPath = maps:get(path, Map, <<>>),
        Path0 = to_binary(RawPath),
        Path = case Path0 of <<>> -> <<"/">>; _ -> Path0 end,
        Query = case maps:get(query, Map, undefined) of
            undefined -> <<>>;
            QueryValue -> <<"?", (to_binary(QueryValue))/binary>>
        end,
        Port = case maps:get(port, Map, undefined) of
            undefined -> default_port(Scheme);
            PortValue -> PortValue
        end,
        {ok, #{scheme => Scheme, host => Host, port => Port, path => <<Path/binary, Query/binary>>}}
    catch
        _:_ -> {error, <<"remote_client_endpoint_invalid">>}
    end.

map_get_binary(Key, Map) ->
    to_binary(maps:get(Key, Map)).

default_port(<<"wss">>) -> 443;
default_port(_) -> 80.

binary_to_utf8(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(Bin).

normalize_transport_error({tls_alert, {Code, Detail}}) ->
    unicode:characters_to_binary(io_lib:format("tls_alert:~p:~p", [Code, Detail]));
normalize_transport_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
normalize_transport_error(Reason) when is_binary(Reason) -> Reason;
normalize_transport_error(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value;
to_list(Value) -> unicode:characters_to_list(io_lib:format("~p", [Value])).

lowercase_binary(Value) -> unicode:characters_to_binary(string:lowercase(to_list(Value))).

append_transcript(Path, Line) ->
    _ = file:write_file(to_list(Path), [Line, <<"\n">>], [append]),
    ok.

fake_server_loop(Listener, Credential, TranscriptPath, StatusSubject, Stage) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            NextStage = handle_fake_connection(Socket, Credential, TranscriptPath, StatusSubject, Stage),
            fake_server_loop(Listener, Credential, TranscriptPath, StatusSubject, NextStage);
        {error, closed} -> ok;
        {error, _} -> ok
    end.

handle_fake_connection(Socket, Credential, TranscriptPath, StatusSubject, Stage) ->
    case recv_http_message(tcp, Socket, 1000) of
        {ok, Headers, Body} ->
            case parse_request_line(Headers) of
                {<<"POST">>, <<"/api/daemons/pairing-exchanges">>} ->
                    append_transcript(TranscriptPath, <<"pairing_exchange_body=", Body/binary>>),
                    ResponseBody = iolist_to_binary([<<"{\"credentialId\":\"cred-1\",\"credential\":\"">>, Credential, <<"\"}">>]),
                    _ = send_transport(tcp, Socket, http_response(201, ResponseBody), 1000),
                    close_transport(tcp, Socket),
                    Stage;
                {<<"GET">>, <<"/api/daemons">>} ->
                    ResponseBody = fake_daemons_response(TranscriptPath),
                    _ = send_transport(tcp, Socket, http_response(200, ResponseBody), 1000),
                    close_transport(tcp, Socket),
                    Stage;
                {<<"GET">>, <<"/api/daemons/ws">>} ->
                    Authorization = case header_value(binary:split(Headers, <<"\r\n">>, [global]), <<"authorization">>) of
                        {ok, Value} -> Value;
                        error -> <<"">>
                    end,
                    append_transcript(TranscriptPath, <<"authorization=", Authorization/binary>>),
                    case Stage of
                        0 ->
                            fake_ws_accept(Socket, Headers, TranscriptPath, StatusSubject, first),
                            1;
                        1 ->
                            append_transcript(TranscriptPath, <<"outage_attempt=closed_before_handshake">>),
                            close_transport(tcp, Socket),
                            2;
                        _ ->
                            fake_ws_accept(Socket, Headers, TranscriptPath, StatusSubject, revoke),
                            3
                    end;
                _ ->
                    _ = send_transport(tcp, Socket, http_response(404, <<"not found">>), 1000),
                    close_transport(tcp, Socket),
                    Stage
            end;
        {error, _} ->
            close_transport(tcp, Socket),
            Stage
    end.

fake_ws_accept(Socket, Headers, TranscriptPath, StatusSubject, Mode) ->
    Key = case header_value(binary:split(Headers, <<"\r\n">>, [global]), <<"sec-websocket-key">>) of
        {ok, Value} -> Value;
        error -> <<>>
    end,
    Accept = base64:encode(crypto:hash(sha, <<Key/binary, ?WS_GUID/binary>>)),
    Handshake = iolist_to_binary([
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Accept, <<"\r\n\r\n">>
    ]),
    _ = send_transport(tcp, Socket, Handshake, 1000),
    case Mode of
        first ->
            _ = send_transport(tcp, Socket, encode_server_text_frame(<<"{\"type\":\"server_hello\",\"heartbeatIntervalMs\":25}">>), 1000),
            record_client_frames(Socket, TranscriptPath, StatusSubject, 3),
            close_transport(tcp, Socket);
        revoke ->
            append_transcript(TranscriptPath, <<"server_frame=credential_revoked">>),
            _ = send_transport(tcp, Socket, encode_server_text_frame(<<"{\"type\":\"credential_revoked\",\"reason\":\"credential revoked\"}">>), 1000),
            timer:sleep(50),
            close_transport(tcp, Socket)
    end.

record_client_frames(_Socket, _TranscriptPath, _StatusSubject, 0) -> ok;
record_client_frames(Socket, TranscriptPath, StatusSubject, Remaining) ->
    case recv_text_frame(tcp, Socket, 1000) of
        {ok, Payload} ->
            append_transcript(TranscriptPath, <<"client_frame=", Payload/binary>>),
            notify_daemons_status(StatusSubject, fake_daemons_response(TranscriptPath)),
            record_client_frames(Socket, TranscriptPath, StatusSubject, Remaining - 1);
        {error, _} -> ok
    end.

notify_daemons_status({subject, Pid, Tag}, StatusBody) ->
    Pid ! {Tag, StatusBody},
    ok;
notify_daemons_status(_StatusSubject, _StatusBody) -> ok.

fake_daemons_response(TranscriptPath) ->
    {LastKnownState, LastEvent} = fake_daemons_status(TranscriptPath),
    case {LastKnownState, LastEvent} of
        {undefined, undefined} -> <<"{\"daemons\":[]}">>;
        _ ->
            LastKnownStateJson = json_or_null(LastKnownState),
            LastEventJson = json_or_null(LastEvent),
            <<"{\"daemons\":[{\"lastKnownState\":", LastKnownStateJson/binary,
              ",\"lastEvent\":", LastEventJson/binary, "}]}">>
    end.

fake_daemons_status(TranscriptPath) ->
    case file:read_file(to_list(TranscriptPath)) of
        {ok, Contents} -> fake_daemons_status_lines(binary:split(Contents, <<"\n">>, [global]), undefined, undefined);
        {error, _} -> {undefined, undefined}
    end.

fake_daemons_status_lines([], LastKnownState, LastEvent) -> {LastKnownState, LastEvent};
fake_daemons_status_lines([<<"client_frame=", Payload/binary>> | Rest], LastKnownState, LastEvent) ->
    NextState = case json_object_after_key(Payload, <<"\"state\":">>) of
        {ok, StateJson} -> StateJson;
        none -> LastKnownState
    end,
    NextEvent = case json_object_after_key(Payload, <<"\"event\":">>) of
        {ok, EventJson} -> EventJson;
        none -> LastEvent
    end,
    fake_daemons_status_lines(Rest, NextState, NextEvent);
fake_daemons_status_lines([_Line | Rest], LastKnownState, LastEvent) ->
    fake_daemons_status_lines(Rest, LastKnownState, LastEvent).

json_or_null(undefined) -> <<"null">>;
json_or_null(Json) -> Json.

json_object_after_key(Payload, Key) ->
    case binary:match(Payload, Key) of
        {Pos, Len} ->
            Start = Pos + Len,
            case binary:part(Payload, Start, byte_size(Payload) - Start) of
                <<"{", Rest/binary>> -> take_json_object(Rest, 1, <<"{">>);
                _ -> none
            end;
        nomatch -> none
    end.

take_json_object(<<>>, _Depth, _Acc) -> none;
take_json_object(<<Byte, Rest/binary>>, Depth, Acc) ->
    NextAcc = <<Acc/binary, Byte>>,
    NextDepth = case Byte of
        ${ -> Depth + 1;
        $} -> Depth - 1;
        _ -> Depth
    end,
    case NextDepth =:= 0 of
        true -> {ok, NextAcc};
        false -> take_json_object(Rest, NextDepth, NextAcc)
    end.

parse_request_line(Headers) ->
    case binary:split(Headers, <<"\r\n">>, [global]) of
        [RequestLine | _] ->
            case split_once(RequestLine, <<" ">>) of
                {ok, Method, Rest} ->
                    case split_once(Rest, <<" ">>) of
                        {ok, Path, _Version} -> {Method, Path};
                        error -> {<<>>, <<>>}
                    end;
                error -> {<<>>, <<>>}
            end;
        _ -> {<<>>, <<>>}
    end.

http_response(Status, Body) ->
    Reason = case Status of
        201 -> <<"Created">>;
        404 -> <<"Not Found">>;
        _ -> <<"OK">>
    end,
    iolist_to_binary([
        <<"HTTP/1.1 ">>, integer_to_binary(Status), <<" ">>, Reason, <<"\r\n">>,
        <<"content-type: application/json\r\n">>,
        <<"content-length: ">>, integer_to_binary(byte_size(Body)), <<"\r\n">>,
        <<"connection: close\r\n\r\n">>,
        Body
    ]).

split_once(Line, Needle) ->
    case binary:match(Line, Needle) of
        {Pos, Len} ->
            Left = binary:part(Line, 0, Pos),
            RightStart = Pos + Len,
            Right = binary:part(Line, RightStart, byte_size(Line) - RightStart),
            {ok, Left, Right};
        nomatch -> error
    end.
