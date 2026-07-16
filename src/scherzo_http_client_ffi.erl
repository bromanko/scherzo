-module(scherzo_http_client_ffi).

-export([ensure_started/0, set_proxy/4]).

ensure_started() ->
    _ = application:ensure_all_started(inets),
    _ = application:ensure_all_started(ssl),
    nil.

set_proxy(Kind, Host, Port, NoProxy) ->
    ensure_started(),
    try
        Option = proxy_option(Kind),
        Proxy = {{to_list(Host), Port}, lists:map(fun to_list/1, NoProxy)},
        case httpc:set_options([{Option, Proxy}]) of
            ok -> {ok, nil};
            {error, _Reason} -> {error, nil}
        end
    catch
        _:_ -> {error, nil}
    end.

proxy_option(<<"http">>) -> proxy;
proxy_option(<<"https">>) -> https_proxy.

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
