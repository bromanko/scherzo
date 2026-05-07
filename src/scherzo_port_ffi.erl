-module(scherzo_port_ffi).

-include_lib("kernel/include/file.hrl").

-export([
    start/2,
    start_with_env/3,
    start_argv/4,
    send_line/2,
    read_stdout_line/2,
    read_diagnostics/1,
    terminate/1,
    await_exit/2,
    temp_dir_for_test/1
]).

-define(MAX_LINE, 10000000).
-define(TERM_GRACE_MS, 300).
-define(KILL_GRACE_MS, 700).
-define(CHILD_PID_WAIT_MS, 200).
-define(POLL_MS, 25).

start(Command, Cwd) ->
    start_with_env(Command, Cwd, []).

start_with_env(Command, Cwd, Env) ->
    try
        case validate_command(Command) of
            {ok, Cmd} ->
                case validate_cwd(Cwd) of
                    {ok, Dir} ->
                        case normalize_env_checked(Env) of
                            {ok, NormalizedEnv} -> start_shell(Cmd, Dir, NormalizedEnv);
                            {error, Error} -> {error, Error}
                        end;
                    {error, Error} -> {error, Error}
                end;
            {error, Error} -> {error, Error}
        end
    catch
        Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
    end.

start_argv(Executable, Args, Cwd, Env) ->
    try
        case validate_executable(Executable) of
            {ok, Exe} ->
                case validate_args(Args) of
                    {ok, ArgList} ->
                        case validate_cwd(Cwd) of
                            {ok, Dir} ->
                                case normalize_env_checked(Env) of
                                    {ok, NormalizedEnv} -> start_argv_checked(Exe, ArgList, Dir, NormalizedEnv);
                                    {error, Error} -> {error, Error}
                                end;
                            {error, Error} -> {error, Error}
                        end;
                    {error, Error} -> {error, Error}
                end;
            {error, Error} -> {error, Error}
        end
    catch
        Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
    end.

start_shell(Cmd, Dir, Env) ->
    case new_temp_storage() of
        {ok, TmpDir, ErrPath, ChildPidPath} ->
            try
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", shell_launch_wrapper(), "scherzo-shell", ErrPath, ChildPidPath, Cmd]},
                    {cd, Dir},
                    {env, Env}
                ]),
                {ok, {scherzo_process, Port, ErrPath, port_os_pid(Port), ChildPidPath, TmpDir}}
            catch
                Class:CatchReason ->
                    _ = cleanup_private_temp_dir(TmpDir),
                    {error, tagged_error(spawn_failed, format_error(Class, CatchReason))}
            end;
        {error, Error} -> {error, Error}
    end.

start_argv_checked(Exe, ArgList, Dir, Env) ->
    case new_temp_storage() of
        {ok, TmpDir, ErrPath, ChildPidPath} ->
            try
                Port = open_port({spawn_executable, "/bin/bash"}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", argv_launch_wrapper(), "scherzo-argv", ErrPath, ChildPidPath, Exe | ArgList]},
                    {cd, Dir},
                    {env, Env}
                ]),
                {ok, {scherzo_process, Port, ErrPath, port_os_pid(Port), ChildPidPath, TmpDir}}
            catch
                Class:CatchReason ->
                    _ = cleanup_private_temp_dir(TmpDir),
                    {error, tagged_error(spawn_failed, format_error(Class, CatchReason))}
            end;
        {error, Error} -> {error, Error}
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
    case process_port_result(Process) of
        {ok, Port} ->
            try
                case erlang:port_info(Port) of
                    undefined -> {error, <<"closed">>};
                    _ ->
                        case erlang:port_command(Port, [Line, <<"\n">>]) of
                            true -> {ok, nil};
                            false -> {error, <<"closed">>}
                        end
                end
            catch
                error:badarg -> {error, <<"closed">>};
                Class:CatchReason -> {error, tagged_error(send_failed, format_error(Class, CatchReason))}
            end;
        error -> {error, <<"closed">>}
    end.

read_stdout_line(Process, TimeoutMs) ->
    case process_port_result(Process) of
        {ok, Port} ->
            try
                Timeout = normalize_timeout(TimeoutMs),
                Key = {scherzo_port_stdout_state, Port},
                State = get_stdout_state(Key),
                case pop_stdout_state(State) of
                    {line, Line, NextState} ->
                        put_stdout_state(Key, NextState),
                        {ok, Line};
                    line_too_long ->
                        erase_stdout_state(Key),
                        {error, line_too_long_error()};
                    {exit_status, Status} ->
                        erase_stdout_state(Key),
                        {error, exit_status_error(Status)};
                    closed ->
                        erase_stdout_state(Key),
                        {error, <<"closed">>};
                    wait ->
                        read_stdout_line_loop(Port, Key, State, Timeout)
                end
            catch
                Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
            end;
        error -> {error, <<"closed">>}
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
                    {error, line_too_long_error()};
                {exit_status, Status} ->
                    erase_stdout_state(Key),
                    {error, exit_status_error(Status)};
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
                    {error, line_too_long_error()};
                {exit_status, ExitStatus} ->
                    erase_stdout_state(Key),
                    {error, exit_status_error(ExitStatus)};
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
                    {error, line_too_long_error()};
                {exit_status, Status} ->
                    erase_stdout_state(Key),
                    {error, exit_status_error(Status)};
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
    try
        ErrPath = process_err_path(Process),
        case file:read_file(ErrPath) of
            {ok, Bytes} ->
                put_cached_diagnostics(Process, Bytes),
                {ok, Bytes};
            {error, enoent} -> {ok, cached_diagnostics(Process)};
            {error, Reason} -> {error, tagged_error(diagnostics_failed, reason_to_binary(Reason))}
        end
    catch
        Class:CatchReason -> {error, tagged_error(diagnostics_failed, format_error(Class, CatchReason))}
    end.

terminate(Process) ->
    try
        _ = cache_diagnostics(Process),
        OsPid = process_os_pid(Process),
        ChildPidPath = process_child_pid_path(Process),
        terminate_launched_process(OsPid, ChildPidPath),
        case process_port_result(Process) of
            {ok, Port} -> catch erlang:port_close(Port);
            error -> ok
        end,
        case cleanup_process_storage(Process) of
            ok -> {ok, nil};
            {error, Reason} -> {error, tagged_error(cleanup_failed, Reason)}
        end
    catch
        Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
    end.

await_exit(Process, TimeoutMs) ->
    case process_port_result(Process) of
        {ok, Port} ->
            try
                OsPid = process_os_pid(Process),
                ChildPidPath = process_child_pid_path(Process),
                Timeout = normalize_timeout(TimeoutMs),
                Deadline = now_ms() + Timeout,
                receive
                    {Port, {exit_status, Status}} ->
                        await_os_exit(Process, Status, OsPid, ChildPidPath, remaining_ms(Deadline));
                    {'EXIT', Port, _Reason} ->
                        await_os_exit(Process, 0, OsPid, ChildPidPath, remaining_ms(Deadline))
                after Timeout ->
                    case erlang:port_info(Port) of
                        undefined -> await_os_exit(Process, 0, OsPid, ChildPidPath, 0);
                        _ -> {error, <<"timeout">>}
                    end
                end
            catch
                Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
            end;
        error -> {error, <<"closed">>}
    end.

await_os_exit(Process, Status, OsPid, ChildPidPath, Timeout) ->
    case wait_for_launched_process_gone(OsPid, ChildPidPath, Timeout) of
        ok -> finish_process_exit(Process, Status);
        timeout -> {error, <<"timeout">>}
    end.

finish_process_exit(Process, Status) ->
    _ = cache_diagnostics(Process),
    case cleanup_process_storage(Process) of
        ok -> {ok, Status};
        {error, Reason} -> {error, tagged_error(cleanup_failed, Reason)}
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

process_port_result({scherzo_process, Port, _ErrPath}) -> {ok, Port};
process_port_result({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath}) -> {ok, Port};
process_port_result({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir}) -> {ok, Port};
process_port_result(_Other) -> error.

process_err_path({scherzo_process, _Port, ErrPath}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath, _TmpDir}) -> ErrPath.

process_os_pid({scherzo_process, Port, _ErrPath}) -> port_os_pid(Port);
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath}) -> OsPid;
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath, _TmpDir}) -> OsPid.

process_child_pid_path({scherzo_process, _Port, _ErrPath}) -> undefined;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath}) -> ChildPidPath;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath, _TmpDir}) -> ChildPidPath.

process_tmp_dir({scherzo_process, _Port, _ErrPath}) -> undefined;
process_tmp_dir({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath}) -> undefined;
process_tmp_dir({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, TmpDir}) -> TmpDir.

port_os_pid(Port) ->
    case catch erlang:port_info(Port, os_pid) of
        {os_pid, Pid} when is_integer(Pid), Pid > 1 -> Pid;
        _ -> undefined
    end.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

new_temp_storage() ->
    Base = tmp_base(),
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    TmpDir = filename:join(Base, "scherzo-port-" ++ Unique),
    case file:make_dir(TmpDir) of
        ok -> {ok, TmpDir, filename:join(TmpDir, "stderr.log"), filename:join(TmpDir, "child.pid")};
        {error, eexist} -> new_temp_storage();
        {error, Reason} -> {error, tagged_error(spawn_failed, <<"create_temp_dir:", (reason_to_binary(Reason))/binary>>)}
    end.

tmp_base() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        "" -> "/tmp";
        Value -> Value
    end.

cache_diagnostics(Process) ->
    try
        ErrPath = process_err_path(Process),
        case file:read_file(ErrPath) of
            {ok, Bytes} ->
                put_cached_diagnostics(Process, Bytes),
                ok;
            {error, enoent} -> ok;
            {error, Reason} -> {error, reason_to_binary(Reason)}
        end
    catch
        _Class:_Reason -> ok
    end.

put_cached_diagnostics(Process, Bytes) ->
    erlang:put(diagnostics_key(Process), Bytes),
    ok.

cached_diagnostics(Process) ->
    case erlang:get(diagnostics_key(Process)) of
        Bytes when is_binary(Bytes) -> Bytes;
        _ -> <<>>
    end.

diagnostics_key(Process) ->
    case process_port_result(Process) of
        {ok, Port} -> {scherzo_port_diagnostics, Port};
        error -> {scherzo_port_diagnostics, process_err_path(Process)}
    end.

cleanup_process_storage(Process) ->
    case process_tmp_dir(Process) of
        undefined -> cleanup_legacy_storage(Process);
        TmpDir -> cleanup_private_temp_dir(TmpDir)
    end.

cleanup_legacy_storage(Process) ->
    _ = delete_file_if_present(process_err_path(Process)),
    case process_child_pid_path(Process) of
        undefined -> ok;
        ChildPidPath -> delete_file_if_present(ChildPidPath)
    end.

cleanup_private_temp_dir(undefined) -> ok;
cleanup_private_temp_dir(TmpDir) ->
    case safe_private_temp_dir(TmpDir) of
        true -> remove_path(TmpDir);
        false -> {error, <<"refusing_to_remove_unexpected_temp_dir">>}
    end.

safe_private_temp_dir(TmpDir) when is_list(TmpDir) ->
    lists:prefix("scherzo-port-", filename:basename(TmpDir));
safe_private_temp_dir(_TmpDir) -> false.

remove_path(Path) ->
    case file:read_file_info(Path) of
        {ok, Info} ->
            case Info#file_info.type of
                directory -> remove_directory(Path);
                _ -> delete_file_if_present(Path)
            end;
        {error, enoent} -> ok;
        {error, Reason} -> {error, reason_to_binary(Reason)}
    end.

remove_directory(Path) ->
    case file:list_dir(Path) of
        {ok, Entries} ->
            case remove_entries(Path, Entries) of
                ok ->
                    case file:del_dir(Path) of
                        ok -> ok;
                        {error, enoent} -> ok;
                        {error, Reason} -> {error, reason_to_binary(Reason)}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, enoent} -> ok;
        {error, Reason} -> {error, reason_to_binary(Reason)}
    end.

remove_entries(_Dir, []) -> ok;
remove_entries(Dir, [Entry | Rest]) ->
    case remove_path(filename:join(Dir, Entry)) of
        ok -> remove_entries(Dir, Rest);
        {error, Reason} -> {error, Reason}
    end.

delete_file_if_present(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> {error, reason_to_binary(Reason)}
    end.

temp_dir_for_test(Process) ->
    case process_tmp_dir(Process) of
        undefined -> {error, <<"not_available">>};
        TmpDir -> {ok, unicode:characters_to_binary(TmpDir)}
    end.

validate_command(Value) ->
    case safe_to_list(Value) of
        {ok, Command} ->
            case string:trim(Command) of
                "" -> {error, tagged_error(invalid_command, <<"empty">>)};
                _ -> {ok, Command}
            end;
        error -> {error, tagged_error(invalid_command, <<"not_string">>)}
    end.

validate_executable(Value) ->
    case safe_to_list(Value) of
        {ok, Executable} ->
            case string:trim(Executable) of
                "" -> {error, tagged_error(invalid_executable, <<"empty">>)};
                _ -> {ok, Executable}
            end;
        error -> {error, tagged_error(invalid_executable, <<"not_string">>)}
    end.

validate_args(Args) when is_list(Args) ->
    validate_args(Args, []);
validate_args(_Args) ->
    {error, tagged_error(invalid_arg, <<"args_not_list">>)}.

validate_args([], Acc) -> {ok, lists:reverse(Acc)};
validate_args([Arg | Rest], Acc) ->
    case safe_to_list(Arg) of
        {ok, ArgList} -> validate_args(Rest, [ArgList | Acc]);
        error -> {error, tagged_error(invalid_arg, <<"arg_not_string">>)}
    end.

validate_cwd(Value) ->
    case safe_to_list(Value) of
        {ok, Dir} ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, <<"cwd_not_directory">>}
            end;
        error -> {error, <<"cwd_not_directory">>}
    end.

normalize_env_checked(Env) when is_list(Env) ->
    normalize_env_checked(Env, []);
normalize_env_checked(_Env) ->
    {error, tagged_error(invalid_env, <<"env_not_list">>)}.

normalize_env_checked([], Acc) -> {ok, lists:reverse(Acc)};
normalize_env_checked([{Key, Value} | Rest], Acc) ->
    case {safe_to_list(Key), safe_to_list(Value)} of
        {{ok, ""}, _} -> {error, tagged_error(invalid_env, <<"empty_key">>)};
        {{ok, KeyList}, {ok, ValueList}} -> normalize_env_checked(Rest, [{KeyList, ValueList} | Acc]);
        {error, _} -> {error, tagged_error(invalid_env, <<"key_not_string">>)};
        {_, error} -> {error, tagged_error(invalid_env, <<"value_not_string">>)}
    end;
normalize_env_checked([_Entry | _Rest], _Acc) ->
    {error, tagged_error(invalid_env, <<"entry_not_pair">>)}.

safe_to_list(Value) when is_binary(Value) -> {ok, binary_to_list(Value)};
safe_to_list(Value) when is_list(Value) -> {ok, Value};
safe_to_list(_Value) -> error.

line_too_long_error() ->
    <<"line_too_long:", (integer_to_binary(?MAX_LINE))/binary>>.

exit_status_error(Status) ->
    <<"exit_status:", (integer_to_binary(Status))/binary>>.

tagged_error(Tag, Reason) when is_atom(Tag) ->
    TagBin = atom_to_binary(Tag, utf8),
    ReasonBin = reason_to_binary(Reason),
    <<TagBin/binary, ":", ReasonBin/binary>>.

unexpected_error(Class, Reason) ->
    tagged_error(unexpected_ffi_failure, format_error(Class, Reason)).

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).
