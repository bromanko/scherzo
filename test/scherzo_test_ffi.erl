-module(scherzo_test_ffi).
-export([set_cwd/1, getenv/1, setenv/2, unsetenv/1, pid_alive/1]).

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

setenv(Name, Value) ->
    true = os:putenv(binary_to_list(Name), binary_to_list(Value)),
    {ok, nil}.

unsetenv(Name) ->
    true = os:unsetenv(binary_to_list(Name)),
    nil.

pid_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("kill -0 " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end;
pid_alive(_Pid) -> false.
