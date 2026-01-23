-module(scherzo_background_ffi).
-export([run_command/4, system_time_ms/0, safe_spawn/2]).

%% Run a command with timeout and capture output
%% Returns {ExitCode, Output} as a tuple
run_command(Executable, Args, WorkingDir, TimeoutMs) ->
    %% Build the command
    Cmd = binary_to_list(Executable),
    ArgsList = [binary_to_list(A) || A <- Args],
    WD = binary_to_list(WorkingDir),

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
        collect_output(Port, <<>>, TimeoutMs)
    catch
        error:enoent ->
            {127, <<"Command not found: ", (list_to_binary(Cmd))/binary>>};
        _:Reason ->
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
