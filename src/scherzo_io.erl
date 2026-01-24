%% Native I/O functions for scherzo agent list TUI
%% Provides raw terminal input handling
-module(scherzo_io).
-export([read_char/0, read_char_timeout/1, enable_raw_mode/0, disable_raw_mode/0]).

%% Read a single character from stdin
%% In raw mode, this returns immediately when a key is pressed
read_char() ->
    case io:get_chars("", 1) of
        eof -> <<>>;
        {error, _} -> <<>>;
        Char when is_list(Char) -> unicode:characters_to_binary(Char);
        Char when is_binary(Char) -> Char
    end.

%% Read a character with timeout (in milliseconds)
%% Returns the character or empty string if timeout
read_char_timeout(TimeoutMs) ->
    Self = self(),
    Pid = spawn(fun() ->
        Char = read_char(),
        Self ! {read_result, Char}
    end),
    receive
        {read_result, Char} -> Char
    after TimeoutMs ->
        exit(Pid, kill),
        <<>>
    end.

%% Enable raw mode (no line buffering, no echo)
enable_raw_mode() ->
    %% Set terminal options for raw input
    case os:type() of
        {unix, _} ->
            os:cmd("stty raw -echo");
        _ ->
            ok
    end,
    nil.

%% Disable raw mode (restore normal terminal)
disable_raw_mode() ->
    case os:type() of
        {unix, _} ->
            os:cmd("stty cooked echo");
        _ ->
            ok
    end,
    nil.
