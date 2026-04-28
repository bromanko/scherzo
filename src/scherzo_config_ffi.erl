-module(scherzo_config_ffi).
-export([getenv/1, home/0, tmpdir/0, dirname/1, absname/1]).

getenv(Name) ->
    case os:getenv(to_list(Name)) of
        false -> {error, nil};
        "" -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

home() ->
    case os:getenv("HOME") of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

tmpdir() ->
    Value = case os:getenv("TMPDIR") of
        false -> "/tmp";
        V -> V
    end,
    {ok, unicode:characters_to_binary(Value)}.

dirname(Path) ->
    {ok, unicode:characters_to_binary(filename:dirname(to_list(Path)))}.

absname(Path) ->
    {ok, unicode:characters_to_binary(filename:absname(to_list(Path)))}.

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
