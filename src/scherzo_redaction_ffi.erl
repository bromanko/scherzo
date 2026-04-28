-module(scherzo_redaction_ffi).
-export([redact_raw_json/3, redact_raw_json_fail_closed/4]).

redact_raw_json_fail_closed(Raw, Secrets, MaxBytes, FailurePlaceholder) ->
    try
        redact_raw_json(Raw, Secrets, MaxBytes)
    catch
        _:_ -> fallback(FailurePlaceholder, MaxBytes)
    end.

redact_raw_json(Raw, Secrets, MaxBytes) ->
    Encoded =
        try
            Parsed = json:decode(Raw),
            iolist_to_binary(json:encode(redact_value(Parsed, Secrets)))
        catch
            _:_ -> iolist_to_binary(json:encode(<<"[unavailable malformed raw json]">>))
        end,
    truncate(Encoded, MaxBytes).

redact_value(Value, Secrets) when is_map(Value) ->
    maps:from_list([
        {Key, redact_map_value(Key, Val, Secrets)}
        || {Key, Val} <- maps:to_list(Value)
    ]);
redact_value(Value, Secrets) when is_list(Value) ->
    [redact_value(Item, Secrets) || Item <- Value];
redact_value(Value, Secrets) when is_binary(Value) ->
    replace_secrets(Value, Secrets);
redact_value(Value, _Secrets) ->
    Value.

redact_map_value(Key, Value, Secrets) ->
    case is_binary(Key) andalso is_sensitive_key(Key) of
        true -> <<"[REDACTED]">>;
        false -> redact_value(Value, Secrets)
    end.

is_sensitive_key(Key) ->
    Lower = lowercase_binary(Key),
    binary:match(Lower, <<"token">>) =/= nomatch orelse
    binary:match(Lower, <<"api_key">>) =/= nomatch orelse
    binary:match(Lower, <<"authorization">>) =/= nomatch orelse
    binary:match(Lower, <<"secret">>) =/= nomatch.

lowercase_binary(Value) ->
    unicode:characters_to_binary(string:lowercase(unicode:characters_to_list(Value))).

replace_secrets(Value, []) ->
    Value;
replace_secrets(Value, [Secret | Rest]) ->
    SecretBin = to_binary(Secret),
    Redacted = case SecretBin of
        <<>> -> Value;
        _ -> binary:replace(Value, SecretBin, <<"[REDACTED]">>, [global])
    end,
    replace_secrets(Redacted, Rest).

truncate(Value, MaxBytes) when byte_size(Value) > MaxBytes ->
    {binary:part(Value, 0, MaxBytes), true};
truncate(Value, _MaxBytes) ->
    {Value, false}.

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value).

fallback(FailurePlaceholder, MaxBytes) ->
    SafeMaxBytes = safe_max_bytes(MaxBytes),
    Placeholder =
        try
            to_binary(FailurePlaceholder)
        catch
            _:_ -> <<"[unavailable redaction failed]">>
        end,
    truncate(Placeholder, SafeMaxBytes).

safe_max_bytes(MaxBytes) when is_integer(MaxBytes), MaxBytes > 0 -> MaxBytes;
safe_max_bytes(_) -> 16384.
