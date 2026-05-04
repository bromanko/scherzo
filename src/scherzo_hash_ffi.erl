-module(scherzo_hash_ffi).
-export([sha256_hex/1]).

sha256_hex(Value) ->
    Bytes = to_binary(Value),
    Digest = crypto:hash(sha256, Bytes),
    Hex = binary:encode_hex(Digest, lowercase),
    Hex.

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])). 
