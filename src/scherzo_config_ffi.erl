-module(scherzo_config_ffi).

-include_lib("kernel/include/file.hrl").

-export([getenv/1, putenv/2, unsetenv/1, home/0, tmpdir/0, dirname/1, absname/1, realpath/1, symlink/2, has_control_character/1, unicode_scalar_length/1]).

getenv(Name) ->
    case os:getenv(to_list(Name)) of
        false -> {error, nil};
        "" -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

putenv(Name, Value) ->
    case os:putenv(to_list(Name), to_list(Value)) of
        true -> {ok, nil};
        _ -> {error, nil}
    end.

unsetenv(Name) ->
    case os:unsetenv(to_list(Name)) of
        true -> {ok, nil};
        _ -> {error, nil}
    end.

home() ->
    case os:getenv("HOME") of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

tmpdir() ->
    Value = case os:getenv("TMPDIR") of
        false -> "/tmp";
        V -> V
    end,
    {ok, unicode:characters_to_binary(Value)}.

dirname(Path) ->
    {ok, unicode:characters_to_binary(filename:dirname(to_list(Path)))}.

absname(Path) ->
    {ok, unicode:characters_to_binary(filename:absname(to_list(Path)))}.

realpath(Path) ->
    try
        case resolve_path(filename:absname(to_list(Path)), 0) of
            {ok, Resolved} -> {ok, unicode:characters_to_binary(Resolved)};
            {error, _Reason} -> {error, nil}
        end
    catch
        _:_ -> {error, nil}
    end.

symlink(Target, LinkName) ->
    case file:make_symlink(to_list(Target), to_list(LinkName)) of
        ok -> {ok, nil};
        {error, _Reason} -> {error, nil}
    end.

has_control_character(Value) ->
    lists:any(
        fun(Codepoint) ->
            (Codepoint >= 0 andalso Codepoint =< 31)
                orelse (Codepoint >= 127 andalso Codepoint =< 159)
        end,
        unicode:characters_to_list(Value)
    ).

unicode_scalar_length(Value) ->
    length(unicode:characters_to_list(Value)).

resolve_path(_Path, Depth) when Depth > 40 ->
    {error, too_many_symlinks};
resolve_path(Path, Depth) ->
    case filename:split(filename:absname(Path)) of
        [Root | Rest] -> resolve_segments(root_path(Root), Rest, Depth);
        [] -> {error, empty_path}
    end.

resolve_segments(Current, [], _Depth) ->
    case file:read_link_info(Current) of
        {ok, #file_info{type = symlink}} ->
            resolve_symlink(Current, [], _Depth);
        {ok, _Info} -> {ok, Current};
        {error, Reason} -> {error, Reason}
    end;
resolve_segments(Current, ["." | Rest], Depth) ->
    resolve_segments(Current, Rest, Depth);
resolve_segments(Current, [".." | Rest], Depth) ->
    resolve_segments(filename:dirname(Current), Rest, Depth);
resolve_segments(Current, [Segment | Rest], Depth) ->
    Candidate = join_segment(Current, Segment),
    case file:read_link_info(Candidate) of
        {ok, #file_info{type = symlink}} ->
            resolve_symlink(Candidate, Rest, Depth);
        {ok, _Info} -> resolve_segments(Candidate, Rest, Depth);
        {error, Reason} -> {error, Reason}
    end.

resolve_symlink(Candidate, Rest, Depth) ->
    case file:read_link(Candidate) of
        {ok, Target} ->
            Base = filename:dirname(Candidate),
            TargetPath = case filename:pathtype(Target) of
                absolute -> Target;
                _ -> filename:join(Base, Target)
            end,
            resolve_path(join_remaining(TargetPath, Rest), Depth + 1);
        {error, Reason} -> {error, Reason}
    end.

root_path("/") -> "/";
root_path(Root) -> Root.

join_segment("/", Segment) -> "/" ++ Segment;
join_segment(Current, Segment) -> filename:join(Current, Segment).

join_remaining(Path, []) -> Path;
join_remaining(Path, Rest) -> filename:join([Path | Rest]).

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
