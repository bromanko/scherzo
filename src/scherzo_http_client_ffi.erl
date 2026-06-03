-module(scherzo_http_client_ffi).

-export([ensure_started/0]).

ensure_started() ->
    _ = application:ensure_all_started(inets),
    _ = application:ensure_all_started(ssl),
    nil.
