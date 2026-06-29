-module(scherzo_port_ffi).

-include_lib("kernel/include/file.hrl").

-export([
    start/2,
    start_with_env/3,
    start_argv/4,
    start_argv_with_input/5,
    send_line/2,
    read_stdout_line/2,
    read_diagnostics/1,
    terminate/1,
    await_exit/2,
    await_exit_with_stdout/2,
    temp_dir_for_test/1
]).

-define(MAX_LINE, 10000000).
-define(RESIDUAL_DRAIN_GRACE_MS, 100).
-define(TERM_GRACE_MS, 300).
-define(KILL_GRACE_MS, 700).
-define(CHILD_PID_WAIT_MS, 200).
-define(LAUNCH_READY_WAIT_MS, 1000).
-define(START_POLL_MS, 5).
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
        case validate_argv_start(Executable, Args, Cwd, Env) of
            {ok, Exe, ArgList, Dir, NormalizedEnv} -> start_argv_checked(Exe, ArgList, Dir, NormalizedEnv);
            {error, Error} -> {error, Error}
        end
    catch
        Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
    end.

start_argv_with_input(Executable, Args, Cwd, Env, Stdin) ->
    try
        case validate_argv_start(Executable, Args, Cwd, Env) of
            {ok, Exe, ArgList, Dir, NormalizedEnv} ->
                case safe_to_binary(Stdin) of
                    {ok, StdinBytes} -> start_argv_checked_with_input(Exe, ArgList, Dir, NormalizedEnv, StdinBytes);
                    error -> {error, tagged_error(invalid_arg, <<"stdin_not_string">>)}
                end;
            {error, Error} -> {error, Error}
        end
    catch
        Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
    end.

validate_argv_start(Executable, Args, Cwd, Env) ->
    case validate_executable(Executable) of
        {ok, Exe} ->
            case validate_args(Args) of
                {ok, ArgList} ->
                    case validate_cwd(Cwd) of
                        {ok, Dir} ->
                            case normalize_env_checked(Env) of
                                {ok, NormalizedEnv} -> {ok, Exe, ArgList, Dir, NormalizedEnv};
                                {error, Error} -> {error, Error}
                            end;
                        {error, Error} -> {error, Error}
                    end;
                {error, Error} -> {error, Error}
            end;
        {error, Error} -> {error, Error}
    end.

resolve_bash(Env) ->
    PathBash = case lists:keyfind("PATH", 1, Env) of false -> false; {_, Path} -> os:find_executable("bash", Path) end,
    Find = case PathBash of false -> os:find_executable("bash"); FoundBash -> FoundBash end,
    case Find of false -> {error, tagged_error(spawn_failed, <<"bash executable not found on PATH">>)}; ResolvedBash -> {ok, ResolvedBash} end.

start_shell(Cmd, Dir, Env) ->
    case resolve_bash(Env) of
        {ok, BashPath} -> start_shell(Cmd, Dir, Env, BashPath);
        {error, Error} -> {error, Error}
    end.

start_shell(Cmd, Dir, Env, BashPath) ->
    case new_temp_storage() of
        {ok, TmpDir, ErrPath, ChildPidPath} ->
            try
                Port = open_port({spawn_executable, BashPath}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", shell_launch_wrapper(), "scherzo-shell", ErrPath, ChildPidPath, status_path_for_tmp_dir(TmpDir), BashPath, Cmd]},
                    {cd, Dir},
                    {env, Env}
                ]),
                Process = process_with_owner_cleanup(Port, ErrPath, ChildPidPath, TmpDir),
                wait_for_launch_ready(Port, ChildPidPath),
                {ok, Process}
            catch
                Class:CatchReason ->
                    _ = cleanup_private_temp_dir(TmpDir),
                    {error, tagged_error(spawn_failed, format_error(Class, CatchReason))}
            end;
        {error, Error} -> {error, Error}
    end.

start_argv_checked(Exe, ArgList, Dir, Env) ->
    case resolve_bash(Env) of
        {ok, BashPath} -> start_argv_checked(Exe, ArgList, Dir, Env, BashPath);
        {error, Error} -> {error, Error}
    end.

start_argv_checked(Exe, ArgList, Dir, Env, BashPath) ->
    case new_temp_storage() of
        {ok, TmpDir, ErrPath, ChildPidPath} ->
            try
                Port = open_port({spawn_executable, BashPath}, [
                    binary,
                    exit_status,
                    use_stdio,
                    {args, ["-c", argv_launch_wrapper(), "scherzo-argv", ErrPath, ChildPidPath, status_path_for_tmp_dir(TmpDir), Exe | ArgList]},
                    {cd, Dir},
                    {env, Env}
                ]),
                Process = process_with_owner_cleanup(Port, ErrPath, ChildPidPath, TmpDir),
                wait_for_launch_ready(Port, ChildPidPath),
                {ok, Process}
            catch
                Class:CatchReason ->
                    _ = cleanup_private_temp_dir(TmpDir),
                    {error, tagged_error(spawn_failed, format_error(Class, CatchReason))}
            end;
        {error, Error} -> {error, Error}
    end.

start_argv_checked_with_input(Exe, ArgList, Dir, Env, StdinBytes) ->
    case resolve_bash(Env) of
        {ok, BashPath} -> start_argv_checked_with_input(Exe, ArgList, Dir, Env, StdinBytes, BashPath);
        {error, Error} -> {error, Error}
    end.

start_argv_checked_with_input(Exe, ArgList, Dir, Env, StdinBytes, BashPath) ->
    case new_temp_storage() of
        {ok, TmpDir, ErrPath, ChildPidPath} ->
            InputPath = filename:join(TmpDir, "stdin.json"),
            case file:write_file(InputPath, StdinBytes) of
                ok ->
                    try
                        EnvAssignments = env_assignments(Env),
                        EnvCount = integer_to_list(length(EnvAssignments)),
                        Port = open_port({spawn_executable, BashPath}, [
                            binary,
                            exit_status,
                            use_stdio,
                            {args, ["-c", argv_launch_wrapper_with_input(), "scherzo-argv-input", ErrPath, ChildPidPath, status_path_for_tmp_dir(TmpDir), InputPath, EnvCount | EnvAssignments ++ [Exe | ArgList]]},
                            {cd, Dir},
                            {env, Env}
                        ]),
                        Process = process_with_owner_cleanup(Port, ErrPath, ChildPidPath, TmpDir),
                        wait_for_launch_ready(Port, ChildPidPath),
                        {ok, Process}
                    catch
                        Class:CatchReason ->
                            _ = cleanup_private_temp_dir(TmpDir),
                            {error, tagged_error(spawn_failed, format_error(Class, CatchReason))}
                    end;
                {error, Reason} ->
                    _ = cleanup_private_temp_dir(TmpDir),
                    {error, tagged_error(spawn_failed, <<"write_stdin:", (reason_to_binary(Reason))/binary>>)}
            end;
        {error, Error} -> {error, Error}
    end.

process_with_owner_cleanup(Port, ErrPath, ChildPidPath, TmpDir) ->
    OsPid = port_os_pid(Port),
    CleanupRef = make_ref(),
    CleanupHandle = start_owner_cleanup_watcher(self(), CleanupRef, OsPid, ChildPidPath, TmpDir),
    {scherzo_process, Port, ErrPath, OsPid, ChildPidPath, TmpDir, CleanupHandle}.

start_owner_cleanup_watcher(Owner, CleanupRef, OsPid, ChildPidPath, TmpDir) ->
    CleanupPid = spawn(fun() -> owner_cleanup_watcher(Owner, CleanupRef, OsPid, ChildPidPath, TmpDir) end),
    {CleanupPid, CleanupRef}.

owner_cleanup_watcher(Owner, CleanupRef, OsPid, ChildPidPath, TmpDir) ->
    Monitor = erlang:monitor(process, Owner),
    receive
        {scherzo_port_owner_done, CleanupRef} ->
            erlang:demonitor(Monitor, [flush]),
            ok;
        {'DOWN', Monitor, process, Owner, _Reason} ->
            terminate_launched_process(OsPid, ChildPidPath),
            _ = cleanup_private_temp_dir(TmpDir),
            ok
    end.

shell_launch_wrapper() ->
    "exec 2> \"$1\"\n"
    "child_pid_path=\"$2\"\n"
    "status_path=\"$3\"\n"
    "bash_path=\"$4\"\n"
    "shift 4\n"
    "if set -m 2>/dev/null; then :; fi\n"
    "\"$bash_path\" -lc \"$1\" <&0 &\n"
    "child_pid=$!\n"
    "set +m 2>/dev/null || true\n"
    "printf '%s\\n' \"$child_pid\" > \"$child_pid_path\"\n"
    "wait \"$child_pid\"\n"
    "status=$?\n"
    "printf '%s\\n' \"$status\" > \"$status_path\"\n" ++
    residual_group_cleanup_script() ++
    "exit \"$status\"\n".

argv_launch_wrapper() ->
    "exec 2> \"$1\"\n"
    "child_pid_path=\"$2\"\n"
    "status_path=\"$3\"\n"
    "shift 3\n"
    "if set -m 2>/dev/null; then :; fi\n"
    "\"$@\" <&0 &\n"
    "child_pid=$!\n"
    "set +m 2>/dev/null || true\n"
    "printf '%s\\n' \"$child_pid\" > \"$child_pid_path\"\n"
    "wait \"$child_pid\"\n"
    "status=$?\n"
    "printf '%s\\n' \"$status\" > \"$status_path\"\n" ++
    residual_group_cleanup_script() ++
    "exit \"$status\"\n".

argv_launch_wrapper_with_input() ->
    "exec 2> \"$1\"\n"
    "child_pid_path=\"$2\"\n"
    "status_path=\"$3\"\n"
    "stdin_path=\"$4\"\n"
    "env_count=\"$5\"\n"
    "shift 5\n"
    "env_args=()\n"
    "while [ \"$env_count\" -gt 0 ]; do\n"
    "  env_args+=(\"$1\")\n"
    "  shift\n"
    "  env_count=$((env_count - 1))\n"
    "done\n"
    "if set -m 2>/dev/null; then :; fi\n"
    "env -i \"${env_args[@]}\" \"$@\" < \"$stdin_path\" &\n"
    "child_pid=$!\n"
    "set +m 2>/dev/null || true\n"
    "printf '%s\\n' \"$child_pid\" > \"$child_pid_path\"\n"
    "wait \"$child_pid\"\n"
    "status=$?\n"
    "printf '%s\\n' \"$status\" > \"$status_path\"\n" ++
    residual_group_cleanup_script() ++
    "exit \"$status\"\n".

residual_group_cleanup_script() ->
    "if [ -n \"$child_pid\" ] && [ \"$child_pid\" -gt 1 ] 2>/dev/null; then\n"
    "  if kill -0 -- \"-$child_pid\" >/dev/null 2>&1; then\n"
    "    i=0\n"
    "    while kill -0 -- \"-$child_pid\" >/dev/null 2>&1 && [ \"$i\" -lt 5 ]; do\n"
    "      sleep 0.02 2>/dev/null || true\n"
    "      i=$((i + 1))\n"
    "    done\n"
    "    if kill -0 -- \"-$child_pid\" >/dev/null 2>&1; then\n"
    "      kill -TERM -- \"-$child_pid\" >/dev/null 2>&1 || true\n"
    "      i=0\n"
    "      while kill -0 -- \"-$child_pid\" >/dev/null 2>&1 && [ \"$i\" -lt 15 ]; do\n"
    "        sleep 0.02 2>/dev/null || true\n"
    "        i=$((i + 1))\n"
    "      done\n"
    "      if kill -0 -- \"-$child_pid\" >/dev/null 2>&1; then\n"
    "        kill -KILL -- \"-$child_pid\" >/dev/null 2>&1 || true\n"
    "        i=0\n"
    "        while kill -0 -- \"-$child_pid\" >/dev/null 2>&1 && [ \"$i\" -lt 35 ]; do\n"
    "          sleep 0.02 2>/dev/null || true\n"
    "          i=$((i + 1))\n"
    "        done\n"
    "      fi\n"
    "    fi\n"
    "  fi\n"
    "fi\n".

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
                        erlang:erase(Key),
                        {error, line_too_long_error()};
                    {exit_status, Status} ->
                        erlang:erase(Key),
                        {error, exit_status_error(Status)};
                    closed ->
                        erlang:erase(Key),
                        {error, <<"closed">>};
                    wait ->
                        read_stdout_line_loop(Port, Key, State, now_ms() + Timeout)
                end
            catch
                Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
            end;
        error -> {error, <<"closed">>}
    end.

read_stdout_line_loop(Port, Key, State, Deadline) ->
    Timeout = remaining_ms(Deadline),
    case Timeout =< 0 of
        true ->
            put_stdout_state(Key, State),
            {error, <<"timeout">>};
        false ->
            receive
        {Port, {data, Bytes}} ->
            Buffer = maps:get(buffer, State, <<>>),
            NextState = State#{buffer => <<Buffer/binary, Bytes/binary>>},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erlang:erase(Key),
                    {error, line_too_long_error()};
                {exit_status, Status} ->
                    erlang:erase(Key),
                    {error, exit_status_error(Status)};
                closed ->
                    erlang:erase(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Deadline)
            end;
        {Port, {exit_status, Status}} ->
            NextState = State#{status => {exit_status, Status}},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erlang:erase(Key),
                    {error, line_too_long_error()};
                {exit_status, ExitStatus} ->
                    erlang:erase(Key),
                    {error, exit_status_error(ExitStatus)};
                closed ->
                    erlang:erase(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Deadline)
            end;
        {'EXIT', Port, _Reason} ->
            NextState = State#{status => closed},
            case pop_stdout_state(NextState) of
                {line, Line, RemainingState} ->
                    put_stdout_state(Key, RemainingState),
                    {ok, Line};
                line_too_long ->
                    erlang:erase(Key),
                    {error, line_too_long_error()};
                {exit_status, Status} ->
                    erlang:erase(Key),
                    {error, exit_status_error(Status)};
                closed ->
                    erlang:erase(Key),
                    {error, <<"closed">>};
                wait ->
                    put_stdout_state(Key, NextState),
                    read_stdout_line_loop(Port, Key, NextState, Deadline)
            end
            after Timeout ->
                put_stdout_state(Key, State),
                {error, <<"timeout">>}
            end
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
        _ = stop_owner_cleanup_watcher(Process),
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
                StatusPath = process_status_path(Process),
                Timeout = normalize_timeout(TimeoutMs),
                Deadline = now_ms() + Timeout,
                _ = erlang:erase({scherzo_port_stdout_state, Port}),
                await_exit_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline)
            catch
                Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
            end;
        error -> {error, <<"closed">>}
    end.

await_exit_with_stdout(Process, TimeoutMs) ->
    case process_port_result(Process) of
        {ok, Port} ->
            try
                OsPid = process_os_pid(Process),
                ChildPidPath = process_child_pid_path(Process),
                StatusPath = process_status_path(Process),
                Timeout = normalize_timeout(TimeoutMs),
                Deadline = now_ms() + Timeout,
                Key = {scherzo_port_stdout_state, Port},
                State = get_stdout_state(Key),
                _ = erlang:erase(Key),
                await_exit_collect_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, State)
            catch
                Class:CatchReason -> {error, unexpected_error(Class, CatchReason)}
            end;
        error -> {error, <<"closed">>}
    end.

await_exit_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline) ->
    case read_exit_status(StatusPath) of
        {ok, Status} -> await_status_file_exit(Process, Port, Status, OsPid, ChildPidPath, Deadline);
        none ->
            Timeout = min_int(?POLL_MS, remaining_ms(Deadline)),
            case Timeout =< 0 of
                true -> await_exit_deadline(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline);
                false ->
                    receive
                        {Port, {data, _Bytes}} ->
                            await_exit_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline);
                        {Port, {exit_status, Status}} ->
                            await_os_exit(Process, Port, Status, OsPid, ChildPidPath, remaining_ms(Deadline));
                        {'EXIT', Port, _Reason} ->
                            await_os_exit(Process, Port, exit_status_or_default(StatusPath, 0), OsPid, ChildPidPath, remaining_ms(Deadline))
                    after Timeout ->
                        await_exit_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline)
                    end
            end
    end.

await_exit_deadline(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline) ->
    case read_exit_status(StatusPath) of
        {ok, Status} -> await_status_file_exit(Process, Port, Status, OsPid, ChildPidPath, Deadline);
        none ->
            case erlang:port_info(Port) of
                undefined -> await_os_exit(Process, Port, 0, OsPid, ChildPidPath, 0);
                _ -> {error, <<"timeout">>}
            end
    end.

await_status_file_exit(Process, Port, Status, _OsPid, ChildPidPath, Deadline) ->
    case child_target_alive(read_child_pid(ChildPidPath)) of
        false -> _ = drain_port_stdout_and_exit(Port), finish_process_exit(Process, Status);
        true ->
            case terminate_residual_launched_process_until(ChildPidPath, Deadline) of
                ok -> _ = drain_port_stdout_and_exit(Port), finish_process_exit(Process, Status);
                timeout -> {error, <<"timeout">>}
            end
    end.

await_os_exit(Process, Port, Status, OsPid, ChildPidPath, Timeout) ->
    case wait_for_launched_process_gone(OsPid, ChildPidPath, Timeout) of
        ok -> _ = drain_port_stdout_and_exit(Port), finish_process_exit(Process, Status);
        timeout -> {error, <<"timeout">>}
    end.

await_exit_collect_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, State) ->
    case read_exit_status(StatusPath) of
        {ok, Status} -> await_status_file_exit_collect(Process, Port, Status, OsPid, ChildPidPath, Deadline, State);
        none ->
            Timeout = min_int(?POLL_MS, remaining_ms(Deadline)),
            case Timeout =< 0 of
                true -> await_exit_collect_deadline(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, State);
                false ->
                    receive
                        {Port, {data, Bytes}} ->
                            await_exit_collect_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, append_stdout_buffer(State, Bytes));
                        {Port, {exit_status, Status}} ->
                            await_os_exit_collect(Process, Port, Status, OsPid, ChildPidPath, remaining_ms(Deadline), State);
                        {'EXIT', Port, _Reason} ->
                            await_os_exit_collect(Process, Port, exit_status_or_default(StatusPath, 0), OsPid, ChildPidPath, remaining_ms(Deadline), State)
                    after Timeout ->
                        await_exit_collect_loop(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, State)
                    end
            end
    end.

await_exit_collect_deadline(Process, Port, OsPid, ChildPidPath, StatusPath, Deadline, State) ->
    case read_exit_status(StatusPath) of
        {ok, Status} -> await_status_file_exit_collect(Process, Port, Status, OsPid, ChildPidPath, Deadline, State);
        none ->
            case erlang:port_info(Port) of
                undefined -> await_os_exit_collect(Process, Port, 0, OsPid, ChildPidPath, 0, State);
                _ -> {error, <<"timeout">>}
            end
    end.

await_status_file_exit_collect(Process, Port, Status, _OsPid, ChildPidPath, Deadline, State) ->
    case child_target_alive(read_child_pid(ChildPidPath)) of
        false -> finish_process_exit_with_stdout(Process, Port, Status, State);
        true ->
            case terminate_residual_launched_process_until(ChildPidPath, Deadline) of
                ok -> finish_process_exit_with_stdout(Process, Port, Status, State);
                timeout -> {error, <<"timeout">>}
            end
    end.

await_os_exit_collect(Process, Port, Status, OsPid, ChildPidPath, Timeout, State) ->
    case wait_for_launched_process_gone(OsPid, ChildPidPath, Timeout) of
        ok -> finish_process_exit_with_stdout(Process, Port, Status, State);
        timeout -> {error, <<"timeout">>}
    end.

finish_process_exit_with_stdout(Process, Port, Status, State) ->
    Drained = drain_port_stdout_state(Port, State),
    Stdout = maps:get(buffer, Drained, <<>>),
    _ = erlang:erase({scherzo_port_stdout_state, Port}),
    case finish_process_exit(Process, Status) of
        {ok, Status} -> {ok, {Status, Stdout}};
        Error -> Error
    end.

finish_process_exit(Process, Status) ->
    _ = cache_diagnostics(Process),
    _ = stop_owner_cleanup_watcher(Process),
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

terminate_residual_launched_process_until(ChildPidPath, Deadline) ->
    case read_child_pid(ChildPidPath) of
        {ok, ChildPid} ->
            Targets = process_tree_pids(ChildPid),
            terminate_child_targets_until({ok, ChildPid}, Targets, Deadline);
        none -> ok
    end.

terminate_child_targets_until(ChildPidResult, Targets, Deadline) ->
    SafeTargets = safe_pids(Targets),
    case targets_alive(ChildPidResult, SafeTargets) of
        false -> ok;
        true ->
            DrainDeadline = min_int(Deadline, now_ms() + ?RESIDUAL_DRAIN_GRACE_MS),
            case wait_for_targets_gone_until(ChildPidResult, SafeTargets, DrainDeadline) of
                ok -> ok;
                timeout -> terminate_live_child_targets_until(ChildPidResult, SafeTargets, Deadline)
            end
    end.

terminate_live_child_targets_until(ChildPidResult, Targets, Deadline) ->
    case ChildPidResult of
        {ok, TermChildPid} -> safe_kill_group("TERM", TermChildPid);
        none -> ok
    end,
    signal_pids("TERM", lists:reverse(Targets)),
    TermDeadline = min_int(Deadline, now_ms() + ?TERM_GRACE_MS),
    case wait_for_targets_gone_until(ChildPidResult, Targets, TermDeadline) of
        ok -> ok;
        timeout ->
            case now_ms() >= Deadline of
                true -> timeout;
                false ->
                    case ChildPidResult of
                        {ok, KillChildPid} -> safe_kill_group("KILL", KillChildPid);
                        none -> ok
                    end,
                    signal_pids("KILL", lists:reverse(Targets)),
                    wait_for_targets_gone_until(ChildPidResult, Targets, min_int(Deadline, now_ms() + ?KILL_GRACE_MS))
            end
    end.

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
    _ = os:cmd("kill -" ++ Signal ++ " " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 || true"),
    ok;
safe_kill_pid(_Signal, _Pid) -> ok.

safe_kill_group(Signal, Pid) when is_integer(Pid), Pid > 1 ->
    _ = os:cmd("kill -" ++ Signal ++ " -" ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 || true"),
    ok;
safe_kill_group(_Signal, _Pid) -> ok.

pid_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("kill -0 " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end;
pid_alive(_Pid) -> false.

process_group_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("kill -0 -" ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
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

%% `open_port/2` can return before the fork helper's child has consumed
%% the final launch ack and exec'd the Bash wrapper. Very short test timeouts
%% can then close the port quickly enough for OTP to print
%% `erl_child_setup: failed with error 32` to the VM stderr. The wrapper writes
%% its child pid immediately after exec, so this best-effort wait keeps early
%% terminate/timeout paths from racing that launch handshake.
wait_for_launch_ready(Port, ChildPidPath) ->
    Deadline = now_ms() + ?LAUNCH_READY_WAIT_MS,
    wait_for_launch_ready_until(Port, ChildPidPath, Deadline).

wait_for_launch_ready_until(Port, ChildPidPath, Deadline) ->
    case read_child_pid(ChildPidPath) of
        {ok, _Pid} -> ok;
        none ->
            case erlang:port_info(Port) of
                undefined -> ok;
                _ ->
                    case now_ms() >= Deadline of
                        true -> ok;
                        false ->
                            sleep_until_next_start_poll(Deadline),
                            wait_for_launch_ready_until(Port, ChildPidPath, Deadline)
                    end
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

read_exit_status(undefined) -> none;
read_exit_status(Path) ->
    case file:read_file(Path) of
        {ok, Bytes} ->
            Text = string:trim(binary_to_list(Bytes)),
            case parse_int(Text) of
                {ok, Status} -> {ok, Status};
                _ -> none
            end;
        _ -> none
    end.

exit_status_or_default(StatusPath, Default) ->
    case read_exit_status(StatusPath) of
        {ok, Status} -> Status;
        none -> Default
    end.

append_stdout_buffer(State, Bytes) ->
    Buffer = maps:get(buffer, State, <<>>),
    State#{buffer => <<Buffer/binary, Bytes/binary>>}.

drain_port_stdout_and_exit(Port) ->
    receive
        {Port, {data, _Bytes}} -> drain_port_stdout_and_exit(Port);
        {Port, {exit_status, _Status}} -> drain_port_stdout_and_exit(Port);
        {'EXIT', Port, _Reason} -> drain_port_stdout_and_exit(Port)
    after 0 -> ok
    end.

drain_port_stdout_state(Port, State) ->
    receive
        {Port, {data, Bytes}} ->
            drain_port_stdout_state(Port, append_stdout_buffer(State, Bytes));
        {Port, {exit_status, Status}} ->
            drain_port_stdout_state(Port, State#{status => {exit_status, Status}});
        {'EXIT', Port, _Reason} ->
            drain_port_stdout_state(Port, State#{status => closed})
    after 0 -> State
    end.

sleep_until_next_poll(Deadline) ->
    Remaining = remaining_ms(Deadline),
    case Remaining > 0 of
        true -> timer:sleep(min_int(?POLL_MS, Remaining));
        false -> ok
    end.

sleep_until_next_start_poll(Deadline) ->
    Remaining = remaining_ms(Deadline),
    case Remaining > 0 of
        true -> timer:sleep(min_int(?START_POLL_MS, Remaining));
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

stop_owner_cleanup_watcher(Process) ->
    case process_cleanup_handle(Process) of
        {Pid, CleanupRef} when is_pid(Pid), is_reference(CleanupRef) ->
            Pid ! {scherzo_port_owner_done, CleanupRef},
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {scherzo_port_owner_done, self()},
            ok;
        _ -> ok
    end.

process_port_result({scherzo_process, Port, _ErrPath}) -> {ok, Port};
process_port_result({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath}) -> {ok, Port};
process_port_result({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir}) -> {ok, Port};
process_port_result({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir, _CleanupPid}) -> {ok, Port};
process_port_result(_Other) -> error.

process_err_path({scherzo_process, _Port, ErrPath}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath, _TmpDir}) -> ErrPath;
process_err_path({scherzo_process, _Port, ErrPath, _OsPid, _ChildPidPath, _TmpDir, _CleanupPid}) -> ErrPath.

process_os_pid({scherzo_process, Port, _ErrPath}) -> port_os_pid(Port);
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath}) -> OsPid;
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath, _TmpDir}) -> OsPid;
process_os_pid({scherzo_process, _Port, _ErrPath, OsPid, _ChildPidPath, _TmpDir, _CleanupPid}) -> OsPid.

process_child_pid_path({scherzo_process, _Port, _ErrPath}) -> undefined;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath}) -> ChildPidPath;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath, _TmpDir}) -> ChildPidPath;
process_child_pid_path({scherzo_process, _Port, _ErrPath, _OsPid, ChildPidPath, _TmpDir, _CleanupPid}) -> ChildPidPath.

process_tmp_dir({scherzo_process, _Port, _ErrPath}) -> undefined;
process_tmp_dir({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath}) -> undefined;
process_tmp_dir({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, TmpDir}) -> TmpDir;
process_tmp_dir({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, TmpDir, _CleanupPid}) -> TmpDir.

process_cleanup_handle({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir, CleanupHandle}) -> CleanupHandle;
process_cleanup_handle(_Process) -> undefined.

process_status_path(Process) ->
    case process_tmp_dir(Process) of
        undefined -> undefined;
        TmpDir -> status_path_for_tmp_dir(TmpDir)
    end.

port_os_pid(Port) ->
    case catch erlang:port_info(Port, os_pid) of
        {os_pid, Pid} when is_integer(Pid), Pid > 1 -> Pid;
        _ -> undefined
    end.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_) -> 0.

status_path_for_tmp_dir(TmpDir) ->
    filename:join(TmpDir, "exit.status").

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

env_assignments(Env) ->
    [Key ++ "=" ++ Value || {Key, Value} <- Env].

safe_to_list(Value) when is_binary(Value) -> {ok, binary_to_list(Value)};
safe_to_list(Value) when is_list(Value) -> {ok, Value};
safe_to_list(_Value) -> error.

safe_to_binary(Value) when is_binary(Value) -> {ok, Value};
safe_to_binary(Value) when is_list(Value) -> {ok, unicode:characters_to_binary(Value)};
safe_to_binary(_Value) -> error.

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
