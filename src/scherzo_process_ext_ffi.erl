-module(scherzo_process_ext_ffi).
-export([trap_exits/1]).

trap_exits(Enabled) ->
    erlang:process_flag(trap_exit, Enabled).
