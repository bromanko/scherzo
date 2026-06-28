-module(scherzo_test_ffi).
-export([
    set_cwd/1,
    getenv/1,
    os_pid/0,
    pid_alive/1,
    process_cleanup_watcher_alive/1,
    wait_for_port_data_and_requeue/2,
    drain_port_data_messages/1,
    drain_any_port_data_messages/0
]).

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

os_pid() ->
    list_to_integer(os:getpid()).

pid_alive(Pid) when is_integer(Pid), Pid > 1 ->
    case os:cmd("kill -0 " ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end;
pid_alive(_Pid) -> false.

process_cleanup_watcher_alive({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir, {CleanupPid, _CleanupRef}}) when is_pid(CleanupPid) ->
    erlang:is_process_alive(CleanupPid);
process_cleanup_watcher_alive({scherzo_process, _Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir, CleanupPid}) when is_pid(CleanupPid) ->
    erlang:is_process_alive(CleanupPid);
process_cleanup_watcher_alive(_Process) -> false.

wait_for_port_data_and_requeue(Process, TimeoutMs) ->
    case process_port(Process) of
        {ok, Port} ->
            receive
                {Port, {data, _Bytes}} = Message ->
                    self() ! Message,
                    true
            after normalize_timeout(TimeoutMs) -> false
            end;
        error -> false
    end.

drain_port_data_messages(Process) ->
    case process_port(Process) of
        {ok, Port} -> drain_port_data_messages(Port, 0);
        error -> 0
    end.

drain_port_data_messages(Port, Count) ->
    receive
        {Port, {data, _Bytes}} -> drain_port_data_messages(Port, Count + 1)
    after 0 -> Count
    end.

drain_any_port_data_messages() ->
    drain_any_port_data_messages(0).

drain_any_port_data_messages(Count) ->
    receive
        {Port, {data, _Bytes}} when is_port(Port) -> drain_any_port_data_messages(Count + 1)
    after 0 -> Count
    end.

process_port({scherzo_process, Port, _ErrPath}) when is_port(Port) -> {ok, Port};
process_port({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath}) when is_port(Port) -> {ok, Port};
process_port({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir}) when is_port(Port) -> {ok, Port};
process_port({scherzo_process, Port, _ErrPath, _OsPid, _ChildPidPath, _TmpDir, _CleanupPid}) when is_port(Port) -> {ok, Port};
process_port(_Other) -> error.

normalize_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 -> TimeoutMs;
normalize_timeout(_TimeoutMs) -> 0.
