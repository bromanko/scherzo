-module(scherzo_expected_crash_ffi).

-export([with_suppressed_crash_reports/2, filter/2]).

-define(TABLE, scherzo_expected_crash_suppression).
-define(FILTER_ID, scherzo_expected_crash_suppression_filter).

with_suppressed_crash_reports(Markers, Fun) ->
    Ref = begin_suppression(Markers),
    try
        Fun()
    after
        end_suppression(Ref)
    end.

filter(LogEvent, _Config) ->
    case active_marker_sets() of
        [] -> ignore;
        MarkerSets ->
            EventBinary = term_to_binary(LogEvent),
            case any_marker_set_matches(EventBinary, MarkerSets) of
                true -> stop;
                false -> ignore
            end
    end.

begin_suppression(Markers) ->
    ensure_table(),
    ensure_filter(),
    Ref = make_ref(),
    ets:insert(?TABLE, {Ref, normalize_markers(Markers)}),
    Ref.

end_suppression(Ref) ->
    case ets:info(?TABLE) of
        undefined -> nil;
        _ ->
            ets:delete(?TABLE, Ref),
            nil
    end.

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            try
                _ = ets:new(?TABLE, [
                    named_table,
                    public,
                    set,
                    {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end.

ensure_filter() ->
    case logger:add_primary_filter(?FILTER_ID, {fun ?MODULE:filter/2, []}) of
        ok -> ok;
        {error, _Reason} -> ok
    end.

active_marker_sets() ->
    try
        case ets:info(?TABLE) of
            undefined -> [];
            _ -> [Markers || {_Ref, Markers} <- ets:tab2list(?TABLE)]
        end
    catch
        error:badarg -> []
    end.

normalize_markers(Markers) ->
    [normalize_marker(Marker) || Marker <- Markers].

normalize_marker(Marker) when is_binary(Marker) ->
    Marker;
normalize_marker(Marker) when is_list(Marker) ->
    unicode:characters_to_binary(Marker);
normalize_marker(Marker) ->
    unicode:characters_to_binary(io_lib:format("~p", [Marker])).

any_marker_set_matches(_EventBinary, []) ->
    false;
any_marker_set_matches(EventBinary, [Markers | Rest]) ->
    case all_markers_match(EventBinary, Markers) of
        true -> true;
        false -> any_marker_set_matches(EventBinary, Rest)
    end.

all_markers_match(_EventBinary, []) ->
    false;
all_markers_match(EventBinary, Markers) ->
    lists:all(
        fun(Marker) -> binary:match(EventBinary, Marker) =/= nomatch end,
        Markers
    ).
