-module(scherzo_test_ffi).
-export([set_cwd/1, getenv/1]).

set_cwd(Path) ->
    case file:set_cwd(Path) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

getenv(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.
