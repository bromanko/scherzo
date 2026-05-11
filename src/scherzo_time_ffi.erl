-module(scherzo_time_ffi).

-export([monotonic_ms/0, wall_clock_ms/0]).

monotonic_ms() ->
    erlang:monotonic_time(millisecond).

wall_clock_ms() ->
    erlang:system_time(millisecond).
