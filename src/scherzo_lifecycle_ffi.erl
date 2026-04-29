-module(scherzo_lifecycle_ffi).

-export([safe_shutdown/2]).

safe_shutdown(Shutdown, Reason) ->
    try Shutdown(Reason) of
        {ok, nil} -> {ok, nil};
        {error, nil} -> {error, nil};
        _Unexpected -> {error, nil}
    catch
        _Class:_Reason:_Stack -> {error, nil}
    end.
