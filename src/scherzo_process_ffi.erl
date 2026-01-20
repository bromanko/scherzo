-module(scherzo_process_ffi).
-export([exit_kill/1]).

%% Send an untrappable 'kill' exit signal to a process.
%% This will terminate the process immediately and close any ports it owns,
%% which sends SIGHUP to any OS subprocesses spawned via those ports.
exit_kill(Pid) ->
    exit(Pid, kill).
