-module(scherzo_terminal_ffi).

-export([stdout_supports_color/0, terminal_columns/0]).

stdout_supports_color() ->
    try
        case color_override() of
            force -> true;
            disable -> false;
            auto -> stdout_is_terminal() andalso term_supports_color()
        end
    catch
        _Class:_Reason -> false
    end.

terminal_columns() ->
    try
        case parse_positive_env("SCHERZO_ATTACH_COLUMNS") of
            {ok, Columns} -> Columns;
            error ->
                case catch io:columns() of
                    {ok, Columns} when is_integer(Columns), Columns > 0 -> Columns;
                    _ -> 0
                end
        end
    catch
        _Class:_Reason -> 0
    end.

color_override() ->
    case env_nonempty("NO_COLOR") orelse env_is_false("CLICOLOR") of
        true -> disable;
        false ->
            case env_truthy("FORCE_COLOR") orelse env_truthy("CLICOLOR_FORCE") of
                true -> force;
                false -> auto
            end
    end.

stdout_is_terminal() ->
    case catch io:getopts(standard_io) of
        Options when is_list(Options) ->
            case lists:keyfind(terminal, 1, Options) of
                {terminal, true} -> true;
                _ -> false
            end;
        _ -> false
    end.

term_supports_color() ->
    case os:getenv("TERM") of
        false -> env_nonempty("COLORTERM");
        "" -> env_nonempty("COLORTERM");
        "dumb" -> false;
        _ -> true
    end.

env_nonempty(Name) ->
    case os:getenv(Name) of
        false -> false;
        "" -> false;
        _ -> true
    end.

parse_positive_env(Name) ->
    case os:getenv(Name) of
        false -> error;
        Value ->
            case string:to_integer(Value) of
                {Columns, ""} when Columns > 0 -> {ok, Columns};
                _ -> error
            end
    end.

env_truthy(Name) ->
    case os:getenv(Name) of
        false -> false;
        "" -> false;
        Value -> not lists:member(string:lowercase(Value), ["0", "false", "no"])
    end.

env_is_false(Name) ->
    case os:getenv(Name) of
        false -> false;
        Value -> lists:member(string:lowercase(Value), ["0", "false", "no"])
    end.
