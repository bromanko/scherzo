-module(scherzo_test_ffi).
-export([set_cwd/1]).

set_cwd(Path) ->
    case file:set_cwd(Path) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
