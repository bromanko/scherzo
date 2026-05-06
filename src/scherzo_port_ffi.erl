-module(scherzo_port_ffi).

-export([start/2, start_with_env/3, start_argv/4, send_line/2, read_stdout_line/2, read_diagnostics/1, terminate/1, await_exit/2]).

-define(MAX_LINE, 10000000).
-define(TERM_GRACE_MS, 300).
-define(KILL_GRACE_MS, 700).
-define(CHILD_PID_WAIT_MS, 200).
-define(POLL_MS, 25).

start(Command, Cwd) ->
    start_with_env(Command, Cwd, []).

start_with_env(Command, Cwd, Env) ->
    try
        Cmd = to_list(Command),
        Dir = to_list(Cwd),
        case filelib:is_dir(Dir) of
            true ->
                ErrPath = stderr_path(),
                ChildPidPath = child_pid_path(),
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", shell_launch_wrapper(), "scherzo-shell", ErrPath, ChildPidPath, Cmd]},
                    {cd, Dir},
                    {env, normalize_env(Env)}
                ]),
                {ok, {scherzo_process, Port, ErrPath, port_os_pid(Port), ChildPidPath}};
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
                ChildPidPath = child_pid_path(),
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", argv_launch_wrapper(), "scherzo-argv", ErrPath, ChildPidPath, Exe | ArgList]},
                    {cd, Dir},
                    {env, normalize_env(Env)}
                ]),
                {ok, {scherzo_process, Port, ErrPath, port_os_pid(Port), ChildPidPath}};
            false ->
                {error, <<"cwd_not_directory">>}
        end
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

shell_launch_wrapper() ->
    "exec 2> \"$1\"\n"
    "child_pid_path=\"$2\"\n"
    "shift 2\n"
    "if set -m 2>/dev/null; then :; fi\n"
    "/bin/bash -lc \"$1\" <&0 &\n"
    "child_pid=$!\n"
    "set +m 2>/dev/null || true\n"
    "printf '%s\\n' \"$child_pid\" > \"$child_pid_path\"\n"
    "wait \"$child_pid\"\n"
    "status=$?\n"
    "exit \"$status\"\n".

argv_launch_wrapper() ->
    "exec 2> \"$1\"\n"
    "child_pid_path=\"$2\"\n"
    "shift 2\n"
    "if set -m 2>/dev/null; then :; fi\n"
    "\"$@\" <&0 &\n"
    "child_pid=$!\n"
    "set +m 2>/dev/null || true\n"
    "printf '%s\\n' \"$child_pid\" > \"$child_pid_path\"\n"
    "wait \"$child_pid\"\n"
    "status=$?\n"
    "exit \"$status\"\n".

send_line(Process, Line) ->
    Port = process_port(Process),
    try
        true = erlang:port_command(Port, [Line, <<"\n">>]),
        {ok, nil}
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

read_stdout_line(Process, TimeoutMs) ->
    Port = process_port(Process),
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

read_diagnostics(Process) ->
    ErrPath = process_err_path(Process),
    case file:read_file(ErrPath) of
        {ok, Bytes} -> {ok, Bytes};
        {error, enoent} -> {ok, <<>>};
        {error, Reason} -> {error, atom_to_binary(Reason, utf8)}
    end.

terminate(Process) ->
    Port = process_port(Process),
    OsPid = process_os_pid(Process),
    ChildPidPath = process_child_pid_path(Process),
    try
        terminate_launched_process(OsPid, ChildPidPath),
        catch erlang:port_close(Port),
        {ok, nil}
    catch
        Class:Reason -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

await_exit(Process, TimeoutMs) ->
    Port = process_port(Process),
    OsPid = process_os_pid(Process),
    ChildPidPath = process_child_pid_path(Process),
    Timeout = normalize_timeout(TimeoutMs),
    Deadline = now_ms() + Timeout,
    receive
        {Port, {exit_status, Status}} ->
            await_os_exit(Status, OsPid, ChildPidPath, remaining_ms(Deadline));
        {'EXIT', Port, _Reason} ->
            await_os_exit(0, OsPid, ChildPidPath, remaining_ms(Deadline))
    after Timeout ->
        case erlang:port_info(Port) of
            undefined -> await_os_exit(0, OsPid, ChildPidPath, 0);
            _ -> {error, <<"timeout">>}
        end
    end.

await_os_exit(Status, OsPid, ChildPidPath, Timeout) ->
    case wait_for_launched_process_gone(OsPid, ChildPidPath, Timeout) of
        ok -> {ok, Status};
        timeout -> {error, <<"timeout">>}
    end.

terminate_launched_process(undefined, ChildPidPath) ->
    case read_child_pid_with_wait(ChildPidPath, ?CHILD_PID_WAIT_MS) of
        {ok, ChildPid} ->
            terminate_child_targets({ok, ChildPid}, [ChildPid]);
        none -> ok
    end;
terminate_launched_process(OsPid, ChildPidPath) ->
    ChildPidResult = read_child_pid_with_wait(ChildPidPath, ?CHILD_PID_WAIT_MS),
    ChildPids = case ChildPidResult of
        {ok, ChildPid} -> [ChildPid | process_tree_pids(ChildPid)];
        none -> []
    end,
    TreePids = process_tree_pids(OsPid),
    Targets = safe_pids(TreePids ++ ChildPids),
    NonRootTargets = [Pid || Pid <- Targets, Pid =/= OsPid],
    terminate_child_targets(ChildPidResult, NonRootTargets),
    safe_kill_pid("TERM", OsPid),
    ok.

terminate_child_targets(ChildPidResult, Targets) ->
    case ChildPidResult of
        {ok, TermChildPid} -> safe_kill_group("TERM", TermChildPid);
        none -> ok
    end,
    signal_pids("TERM", lists:reverse(Targets)),
    case wait_for_targets_gone(ChildPidResult, Targets, ?TERM_GRACE_MS) of
        ok -> ok;
        timeout ->
            case ChildPidResult of
                {ok, KillChildPid} -> safe_kill_group("KILL", KillChildPid);
                none -> ok
            end,
            signal_pids("KILL", lists:reverse(Targets)),
            _ = wait_for_targets_gone(ChildPidResult, Targets, ?KILL_GRACE_MS),
            ok
    end.

wait_for_launched_process_gone(OsPid, ChildPidPath, Timeout) ->
    Deadline = now_ms() + Timeout,
    wait_for_launched_process_gone_until(OsPid, ChildPidPath, Deadline).

wait_for_launched_process_gone_until(OsPid, ChildPidPath, Deadline) ->
    case launched_process_alive(OsPid, ChildPidPath) of
        false -> ok;
        true ->
            case now_ms() >= Deadline of
                true -> timeout;
                false ->
                    sleep_until_next_poll(Deadline),
                    wait_for_launched_process_gone_until(OsPid, ChildPidPath, Deadline)
            end
    end.

launched_process_alive(OsPid, ChildPidPath) ->
    root_alive(OsPid) orelse child_target_alive(read_child_pid(ChildPidPath)).

root_alive(undefined) -> false;
root_alive(Pid) -> pid_alive(Pid).

child_target_alive({ok, ChildPid}) ->
    pid_alive(ChildPid) orelse process_group_alive(ChildPid);
child_target_alive(none) -> false.

wait_for_targets_gone(ChildPidResult, Targets, Timeout) ->
    Deadline = now_ms() + Timeout,
    wait_for_targets_gone_until(ChildPidResult, safe_pids(Targets), Deadline).

wait_for_targets_gone_until(ChildPidResult, Targets, Deadline) ->
    case targets_alive(ChildPidResult, Targets) of
        false -> ok;
        true ->
            case now_ms() >= Deadline of
                true -> timeout;
                false ->
                    sleep_until_next_poll(Deadline),
                    wait_for_targets_gone_until(ChildPidResult, Targets, Deadline)
            end
    end.

targets_alive(ChildPidResult, Targets) ->
    child_target_alive(ChildPidResult) orelse any_pid_alive(Targets).

any_pid_alive([]) -> false;
any_pid_alive([Pid | Rest]) ->
    case pid_alive(Pid) of
        true -> true;
        false -> any_pid_alive(Rest)
    end.

process_tree_pids(undefined) -> [];
process_tree_pids(Pid) when is_integer(Pid), Pid > 1 ->
    Pairs = process_table(),
    Descendants = descendant_pids([Pid], Pairs, []),
    safe_pids([Pid | Descendants]);
process_tree_pids(_Pid) -> [].

descendant_pids([], _Pairs, Seen) -> Seen;
descendant_pids([Parent | Rest], Pairs, Seen) ->
    Children = [Child || {Child, ParentPid} <- Pairs, ParentPid =:= Parent, not lists:member(Child, Seen), Child =/= Parent],
    descendant_pids(Rest ++ Children, Pairs, Children ++ Seen).

process_table() ->
    Output = os:cmd("ps -e -o pid= -o ppid= 2>/dev/null"),
    parse_process_table(string:split(Output, "\n", all), []).

parse_process_table([], Acc) -> Acc;
parse_process_table([Line | Rest], Acc) ->
    case string:tokens(Line, " \t") of
        [PidText, PpidText | _] ->
            case {parse_int(PidText), parse_int(PpidText)} of
                {{ok, Pid}, {ok, Ppid}} -> parse_process_table(Rest, [{Pid, Ppid} | Acc]);
                _ -> parse_process_table(Rest, Acc)
            end;
        _ -> parse_process_table(Rest, Acc)
    end.

parse_int(Text) ->
    try {ok, list_to_integer(Text)}
    catch _:_ -> error
    end.

safe_pids(Pids) ->
    lists:usort([Pid || Pid <- Pids, is_integer(Pid), Pid > 1]).

signal_pids(_Signal, []) -> ok;
signal_pids(Signal, [Pid | Rest]) ->
    safe_kill_pid(Signal, Pid),
    signal_pids(Signal, Rest).

safe_kill_pid(Signal, Pid) when is_integer(Pid), Pid > 1 ->
    _ = os:cmd("/bin/kill -" ++ Signal ++ " " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 || true"),
    ok;
safe_kill_pid(_Signal, _Pid) -> ok.

safe_kill_group(Signal, Pid) when is_integer(Pid), Pid > 1 ->
    _ = os:cmd("/bin/kill -" ++ Signal ++ " -" ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 || true"),
    ok;
safe_kill_group(_Signal, _Pid) -> ok.

pid_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("/bin/kill -0 " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end;
pid_alive(_Pid) -> false.

process_group_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("/bin/kill -0 -" ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end;
process_group_alive(_Pid) -> false.

read_child_pid_with_wait(undefined, _Timeout) -> none;
read_child_pid_with_wait(Path, Timeout) ->
    Deadline = now_ms() + Timeout,
    read_child_pid_until(Path, Deadline).

read_child_pid_until(Path, Deadline) ->
    case read_child_pid(Path) of
        {ok, Pid} -> {ok, Pid};
        none ->
            case now_ms() >= Deadline of
                true -> none;
                false ->
                    sleep_until_next_poll(Deadline),
                    read_child_pid_until(Path, Deadline)
            end
    end.

read_child_pid(undefined) -> none;
read_child_pid(Path) ->
    case file:read_file(Path) of
        {ok, Bytes} ->
            Text = string:trim(binary_to_list(Bytes)),
            case parse_int(Text) of
                {ok, Pid} when Pid > 1 -> {ok, Pid};
                _ -> none
            end;
        _ -> none
    end.

sleep_until_next_poll(Deadline) ->
    Remaining = remaining_ms(Deadline),
    case Remaining > 0 of
        true -> timer:sleep(min_int(?POLL_MS, Remaining));
        false -> ok
    end.

remaining_ms(Deadline) ->
    Remaining = Deadline - now_ms(),
    case Remaining > 0 of
        true -> Remaining;
        false -> 0
    end.

now_ms() -> erlang:monotonic_time(millisecond).

min_int(A, B) when A =< B -> A;
min_int(_A, B) -> B.

process_port({scherzo_process, Port, _ErrPath}) -> Port;
process_port({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath}) -> Port.

process_err_path({scherzo_process, _Port, ErrPath}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath}) -> ErrPath.

process_os_pid({scherzo_process, Port, _ErrPath}) -> port_os_pid(Port);
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath}) -> OsPid.

process_child_pid_path({scherzo_process, _Port, _ErrPath}) -> undefined;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath}) -> ChildPidPath.

port_os_pid(Port) ->
    case catch erlang:port_info(Port, os_pid) of
        {os_pid, Pid} when is_integer(Pid), Pid > 1 -> Pid;
        _ -> undefined
    end.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

stderr_path() ->
    tmp_path("scherzo-port-", ".stderr").

child_pid_path() ->
    tmp_path("scherzo-port-", ".child.pid").

tmp_path(Prefix, Suffix) ->
    Base = case os:getenv("TMPDIR") of
        false -> "/tmp";
        Value -> Value
    end,
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    filename:join(Base, Prefix ++ Unique ++ Suffix).

normalize_env(Env) ->
    [{to_list(Key), to_list(Value)} || {Key, Value} <- Env].

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
