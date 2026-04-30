-module(scherzo_port_ffi).

-export([start/2, start_with_env/3, send_line/2, read_stdout_line/2, read_diagnostics/1, terminate/1, await_exit/2]).

-define(MAX_LINE, 10000000).

start(Command, Cwd) ->
    start_with_env(Command, Cwd, []).

start_with_env(Command, Cwd, Env) ->
    try
        Cmd = to_list(Command),
        Dir = to_list(Cwd),
        case filelib:is_dir(Dir) of
            true ->
                ErrPath = stderr_path(),
                Wrapper = iolist_to_binary(["exec 2> ", shell_quote(ErrPath), "\n", Cmd]),
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {line, ?MAX_LINE},
                    {args, ["-lc", Wrapper]},
                    {cd, Dir},
                    {env, normalize_env(Env)}
                ]),
                {ok, {scherzo_process, Port, ErrPath}};
            false ->
                {error, <<"cwd_not_directory">>}
        end
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

send_line({scherzo_process, Port, _ErrPath}, Line) ->
    try
        true = erlang:port_command(Port, [Line, <<"\n">>]),
        {ok, nil}
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

read_stdout_line({scherzo_process, Port, _ErrPath}, TimeoutMs) ->
    Timeout = normalize_timeout(TimeoutMs),
    receive
        {Port, {data, {eol, Line}}} -> {ok, Line};
        {Port, {data, {noeol, _Line}}} -> {error, <<"line_too_long">>};
        {Port, {exit_status, Status}} -> {error, <<"exit_status:", (integer_to_binary(Status))/binary>>};
        {'EXIT', Port, _Reason} -> {error, <<"closed">>}
    after Timeout ->
        {error, <<"timeout">>}
    end.

read_diagnostics({scherzo_process, _Port, ErrPath}) ->
    case file:read_file(ErrPath) of
        {ok, Bytes} -> {ok, Bytes};
        {error, enoent} -> {ok, <<>>};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

terminate({scherzo_process, Port, _ErrPath}) ->
    try
        catch erlang:port_close(Port),
        {ok, nil}
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

await_exit({scherzo_process, Port, _ErrPath}, TimeoutMs) ->
    Timeout = normalize_timeout(TimeoutMs),
    receive
        {Port, {exit_status, Status}} -> {ok, Status};
        {'EXIT', Port, _Reason} -> {ok, 0}
    after Timeout ->
        case erlang:port_info(Port) of
            undefined -> {ok, 0};
            _ -> {error, <<"timeout">>}
        end
    end.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

stderr_path() ->
    Base = case os:getenv("TMPDIR") of
        false -> "/tmp";
        Value -> Value
    end,
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    filename:join(Base, "scherzo-port-" ++ Unique ++ ".stderr").

shell_quote(Value) when is_binary(Value) -> shell_quote(binary_to_list(Value));
shell_quote(Value) ->
    [$', lists:flatmap(fun($') -> "'\\''"; (C) -> [C] end, Value), $'].

normalize_env(Env) ->
    [{to_list(Key), to_list(Value)} || {Key, Value} <- Env].

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
