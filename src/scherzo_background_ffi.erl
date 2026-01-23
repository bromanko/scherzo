-module(scherzo_background_ffi).
-export([run_command/4, system_time_ms/0, safe_spawn/2]).

%% Run a command with timeout and capture output
%% Returns {ExitCode, Output} as a tuple
run_command(Executable, Args, WorkingDir, TimeoutMs) ->
    %% Build the command
    Cmd = binary_to_list(Executable),
    ArgsList = [binary_to_list(A) || A <- Args],
    WD = binary_to_list(WorkingDir),

    %% Debug: write command info to file
    DebugFile = "/home/sprite/scherzo/.scherzo/ffi_debug.log",
    DebugInfo = io_lib:format("Cmd: ~s~nArgs: ~p~nWD: ~s~n~n", [Cmd, ArgsList, WD]),
    file:write_file(DebugFile, DebugInfo, [append]),

    %% Use open_port for better control
    PortSettings = [
        {cd, WD},
        {args, ArgsList},
        binary,
        exit_status,
        stderr_to_stdout,
        use_stdio
    ],

    try
        Port = open_port({spawn_executable, Cmd}, PortSettings),
        Result = collect_output(Port, <<>>, TimeoutMs),
        %% Debug: log result
        {ExitCode, Output} = Result,
        file:write_file(DebugFile, io_lib:format("Exit: ~p~nOutput: ~s~n---~n", [ExitCode, Output]), [append]),
        Result
    catch
        error:enoent ->
            file:write_file(DebugFile, io_lib:format("ENOENT: ~s~n---~n", [Cmd]), [append]),
            {127, <<"Command not found: ", (list_to_binary(Cmd))/binary>>};
        _:Reason ->
            file:write_file(DebugFile, io_lib:format("Error: ~p~n---~n", [Reason]), [append]),
            {1, list_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

%% Collect output from port with timeout
collect_output(Port, Acc, TimeoutMs) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, <<Acc/binary, Data/binary>>, TimeoutMs);
        {Port, {exit_status, ExitCode}} ->
            {ExitCode, Acc}
    after TimeoutMs ->
        %% Timeout - kill the port
        catch port_close(Port),
        {124, <<Acc/binary, "\n[Timed out]">>}
    end.

%% Get system time in milliseconds
system_time_ms() ->
    erlang:system_time(millisecond).

%% Spawn a process that catches exceptions and calls OnError if it crashes
%% WorkFn: fn() -> Nil - the work to do
%% OnError: fn(String) -> Nil - called with error reason if WorkFn crashes
safe_spawn(WorkFn, OnError) ->
    erlang:spawn(fun() ->
        try
            WorkFn()
        catch
            Class:Reason:Stacktrace ->
                ErrorMsg = list_to_binary(io_lib:format(
                    "Process crashed: ~p:~p~n~p",
                    [Class, Reason, Stacktrace]
                )),
                OnError(ErrorMsg)
        end
    end).
