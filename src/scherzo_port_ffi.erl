-module(scherzo_port_ffi).

-export([start/2, start_with_env/3, start_argv/4, send_line/2, read_stdout_line/2, read_diagnostics/1, terminate/1, await_exit/2]).

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

start_argv(Executable, Args, Cwd, Env) ->
    try
        Exe = to_list(Executable),
        ArgList = [to_list(Arg) || Arg <- Args],
        Dir = to_list(Cwd),
        case filelib:is_dir(Dir) of
            true ->
                ErrPath = stderr_path(),
                Wrapper = "exec 2> \"$1\"; shift; exec \"$@\"",
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", Wrapper, "scherzo-argv", ErrPath, Exe | ArgList]},
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
    Key = {scherzo_port_stdout_state, Port},
    State = get_stdout_state(Key),
    case pop_stdout_state(State) of
        {line, Line, NextState} ->
            put_stdout_state(Key, NextState),
            {ok, Line};
        line_too_long ->
            erase_stdout_state(Key),
            {error, <<"line_too_long">>};
        {exit_status, Status} ->
            erase_stdout_state(Key),
            {error, <<"exit_status:", (integer_to_binary(Status))/binary>>};
        closed ->
            erase_stdout_state(Key),
            {error, <<"closed">>};
        wait ->
            read_stdout_line_loop(Port, Key, State, Timeout)
    end.

read_stdout_line_loop(Port, Key, State, Timeout) ->
    receive
        {Port, {data, Bytes}} ->
            Buffer = maps:get(buffer, State, <<>>),
            NextState = State#{buffer => <<Buffer/binary, Bytes/binary>>},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erase_stdout_state(Key),
                    {error, <<"line_too_long">>};
                {exit_status, Status} ->
                    erase_stdout_state(Key),
                    {error, <<"exit_status:", (integer_to_binary(Status))/binary>>};
                closed ->
                    erase_stdout_state(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Timeout)
            end;
        {Port, {exit_status, Status}} ->
            NextState = State#{status => {exit_status, Status}},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erase_stdout_state(Key),
                    {error, <<"line_too_long">>};
                {exit_status, ExitStatus} ->
                    erase_stdout_state(Key),
                    {error, <<"exit_status:", (integer_to_binary(ExitStatus))/binary>>};
                closed ->
                    erase_stdout_state(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Timeout)
            end;
        {'EXIT', Port, _Reason} ->
            NextState = State#{status => closed},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erase_stdout_state(Key),
                    {error, <<"line_too_long">>};
                {exit_status, Status} ->
                    erase_stdout_state(Key),
                    {error, <<"exit_status:", (integer_to_binary(Status))/binary>>};
                closed ->
                    erase_stdout_state(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Timeout)
            end
    after Timeout ->
        put_stdout_state(Key, State),
        {error, <<"timeout">>}
    end.

get_stdout_state(Key) ->
    case erlang:get(Key) of
        undefined -> #{buffer => <<>>, status => running};
        State -> State
    end.

put_stdout_state(Key, #{buffer := <<>>, status := running}) ->
    erlang:erase(Key),
    ok;
put_stdout_state(Key, State) ->
    erlang:put(Key, State),
    ok.

erase_stdout_state(Key) ->
    erlang:erase(Key),
    ok.

pop_stdout_state(State) ->
    Buffer = maps:get(buffer, State, <<>>),
    case binary:match(Buffer, <<"\n">>) of
        {Position, 1} when Position > ?MAX_LINE ->
            line_too_long;
        {Position, 1} ->
            Line = binary:part(Buffer, 0, Position),
            RestStart = Position + 1,
            RestSize = byte_size(Buffer) - RestStart,
            Rest = binary:part(Buffer, RestStart, RestSize),
            {line, Line, State#{buffer => Rest}};
        nomatch ->
            case byte_size(Buffer) > ?MAX_LINE of
                true -> line_too_long;
                false -> pop_stdout_status(State, Buffer)
            end
    end.

pop_stdout_status(State, <<>>) ->
    case maps:get(status, State, running) of
        {exit_status, Status} -> {exit_status, Status};
        closed -> closed;
        running -> wait
    end;
pop_stdout_status(State, Buffer) ->
    case maps:get(status, State, running) of
        {exit_status, _Status} -> {line, Buffer, State#{buffer => <<>>}};
        closed -> {line, Buffer, State#{buffer => <<>>}};
        running -> wait
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
