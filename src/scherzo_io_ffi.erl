-module(scherzo_io_ffi).
-export([get_line/1]).

%% Read a line from stdin, returning {ok, Line} or {error, nil}
get_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line when is_list(Line) ->
            %% Convert charlist to binary (Gleam string)
            {ok, unicode:characters_to_binary(Line)};
        Line when is_binary(Line) ->
            {ok, Line}
    end.
