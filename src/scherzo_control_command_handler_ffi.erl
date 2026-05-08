-module(scherzo_control_command_handler_ffi).

-export([
    log_result/4,
    supports_run_schedule_now/1
]).

-define(RUN_SCHEDULE_NOW_INDEX, 9).
-define(LEGACY_LOG_RESULT_INDEX, 11).
-define(LOG_RESULT_INDEX, 12).

log_result(Context, State, Result, Fields) ->
    case log_result_fun(Context) of
        {ok, Fun} -> Fun(State, Result, Fields);
        error -> nil
    end.

supports_run_schedule_now(Context) when is_tuple(Context), tuple_size(Context) >= ?LOG_RESULT_INDEX ->
    is_function(element(?RUN_SCHEDULE_NOW_INDEX, Context), 3);
supports_run_schedule_now(_Context) ->
    false.

log_result_fun(Context) when is_tuple(Context), tuple_size(Context) >= ?LOG_RESULT_INDEX ->
    Candidate = element(?LOG_RESULT_INDEX, Context),
    case is_function(Candidate, 3) of
        true -> {ok, Candidate};
        false -> legacy_log_result_fun(Context)
    end;
log_result_fun(Context) ->
    legacy_log_result_fun(Context).

legacy_log_result_fun(Context) when is_tuple(Context), tuple_size(Context) >= ?LEGACY_LOG_RESULT_INDEX ->
    Candidate = element(?LEGACY_LOG_RESULT_INDEX, Context),
    case is_function(Candidate, 3) of
        true -> {ok, Candidate};
        false -> error
    end;
legacy_log_result_fun(_Context) ->
    error.
