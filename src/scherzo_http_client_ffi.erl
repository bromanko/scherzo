-module(scherzo_http_client_ffi).

-export([ensure_started/0, configure/3]).

ensure_started() ->
    _ = application:ensure_all_started(inets),
    _ = application:ensure_all_started(ssl),
    nil.

configure(HttpProxyValue, HttpsProxyValue, NoProxyValue) ->
    Result = try
        case ensure_applications_started() of
            ok -> configure_started(HttpProxyValue, HttpsProxyValue, NoProxyValue);
            {error, Error} -> {error, Error}
        end
    catch
        _Class:_Reason ->
            {error, <<"HTTP client proxy configuration failed">>}
    end,
    to_gleam_result(Result).

to_gleam_result({ok, nil}) -> {ok, nil};
to_gleam_result({error, Message}) -> {error, {configure_error, Message}}.

ensure_applications_started() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ensure_ssl_started();
        {error, _} -> {error, <<"HTTP client startup failed">>}
    end.

ensure_ssl_started() ->
    case application:ensure_all_started(ssl) of
        {ok, _} -> ok;
        {error, _} -> {error, <<"HTTP client TLS startup failed">>}
    end.

configure_started(HttpProxyValue, HttpsProxyValue, NoProxyValue) ->
    case parse_proxy(HttpProxyValue, <<"HTTP">>) of
        {error, Error} -> {error, Error};
        {ok, HttpProxy} ->
            case parse_proxy(HttpsProxyValue, <<"HTTPS">>) of
                {error, Error} -> {error, Error};
                {ok, HttpsProxy} ->
                    {NoProxy, BypassAll} = parse_no_proxy(NoProxyValue),
                    apply_proxy_options(HttpProxy, HttpsProxy, NoProxy, BypassAll)
            end
    end.

parse_proxy(Value, Label) ->
    Proxy = string:trim(to_list(Value)),
    case Proxy of
        [] -> {ok, undefined};
        _ -> parse_proxy_uri(with_default_scheme(Proxy), Label)
    end.

with_default_scheme(Value) ->
    case string:find(Value, "://") of
        nomatch -> "http://" ++ Value;
        _ -> Value
    end.

parse_proxy_uri(Value, Label) ->
    case uri_string:parse(Value) of
        Map when is_map(Map) -> validate_proxy_uri(Map, Label);
        _ -> {error, invalid_proxy_message(Label)}
    end.

validate_proxy_uri(Map, Label) ->
    Scheme = string:lowercase(maps:get(scheme, Map, "")),
    Host = maps:get(host, Map, ""),
    Port = maps:get(port, Map, 80),
    Path = maps:get(path, Map, ""),
    HasCredentials = maps:is_key(userinfo, Map),
    HasUnsupportedParts =
        maps:is_key(query, Map) orelse maps:is_key(fragment, Map),
    case HasCredentials of
        true -> {error, proxy_auth_message(Label)};
        false when Scheme =/= "http" -> {error, invalid_proxy_message(Label)};
        false when Host =:= "" -> {error, invalid_proxy_message(Label)};
        false when not is_integer(Port); Port < 1; Port > 65535 ->
            {error, invalid_proxy_message(Label)};
        false when Path =/= "" andalso Path =/= "/" ->
            {error, invalid_proxy_message(Label)};
        false when HasUnsupportedParts -> {error, invalid_proxy_message(Label)};
        false -> {ok, {Host, Port}}
    end.

invalid_proxy_message(Label) ->
    <<
        "invalid ",
        Label/binary,
        " proxy configuration; expected an http:// URL with a host and optional valid port"
    >>.

proxy_auth_message(Label) ->
    <<
        Label/binary,
        " proxy configuration contains credentials; proxy authentication is not supported"
    >>.

parse_no_proxy(Value) ->
    Entries = string:split(to_list(Value), ",", all),
    parse_no_proxy_entries(Entries, [], false).

parse_no_proxy_entries([], Acc, BypassAll) ->
    {lists:reverse(Acc), BypassAll};
parse_no_proxy_entries([Entry0 | Rest], Acc, BypassAll) ->
    Entry = string:trim(Entry0),
    case Entry of
        [] -> parse_no_proxy_entries(Rest, Acc, BypassAll);
        "*" -> parse_no_proxy_entries(Rest, Acc, true);
        _ ->
            Normalized = normalize_no_proxy_entry(Entry),
            parse_no_proxy_entries(Rest, lists:reverse(Normalized, Acc), BypassAll)
    end.

normalize_no_proxy_entry(Entry0) ->
    Entry = string:lowercase(strip_optional_port(Entry0)),
    case Entry of
        "." ++ Domain when Domain =/= [] -> [domain_suffix(Domain), Domain];
        "*." ++ Domain when Domain =/= [] -> [domain_suffix(Domain), Domain];
        _ -> add_domain_suffix_match(Entry)
    end.

add_domain_suffix_match(Entry) ->
    case is_domain_name(Entry) of
        true -> [domain_suffix(Entry), Entry];
        false -> [Entry]
    end.

%% httpc removes its leading "*." before suffix comparison. Retaining the
%% separator dot prevents "notexample.test" from matching "example.test".
domain_suffix(Domain) -> "*.." ++ Domain.

is_domain_name(Entry) ->
    not lists:member($/, Entry)
        andalso lists:member($., Entry)
        andalso inet:parse_address(Entry) =:= {error, einval}.

strip_optional_port([$[ | _] = Entry) ->
    strip_bracketed_port(Entry);
strip_optional_port(Entry) ->
    case string:split(Entry, ":", all) of
        [Host, PortText] when Host =/= [], PortText =/= [] ->
            case parse_port(PortText) of
                {ok, _} -> Host;
                error -> Entry
            end;
        _ -> Entry
    end.

strip_bracketed_port(Entry) ->
    Capture = [{capture, [1, 2], list}],
    case re:run(Entry, "^\\[([^]]+)\\](?::([0-9]+))?$", Capture) of
        {match, [Host, []]} -> Host;
        {match, [Host, PortText]} ->
            case parse_port(PortText) of
                {ok, _} -> Host;
                error -> Entry
            end;
        nomatch -> Entry
    end.

parse_port(Value) ->
    case string:to_integer(Value) of
        {Port, []} when Port >= 1, Port =< 65535 -> {ok, Port};
        _ -> error
    end.

apply_proxy_options(HttpProxy, HttpsProxy, _NoProxy, true) ->
    set_proxy_options(HttpProxy, HttpsProxy, ["*."]);
apply_proxy_options(HttpProxy, HttpsProxy, NoProxy, false) ->
    set_proxy_options(HttpProxy, HttpsProxy, NoProxy).

set_proxy_options(HttpProxy, HttpsProxy, NoProxy) ->
    Options =
        add_proxy_option(https_proxy, HttpsProxy, NoProxy,
            add_proxy_option(proxy, HttpProxy, NoProxy, [])),
    case Options of
        [] -> {ok, nil};
        _ ->
            case httpc:set_options(Options) of
                ok -> {ok, nil};
                {error, _} ->
                    {error, <<"OTP HTTP client rejected proxy configuration">>}
            end
    end.

add_proxy_option(_Name, undefined, _NoProxy, Options) -> Options;
add_proxy_option(Name, Proxy, NoProxy, Options) ->
    [{Name, {Proxy, NoProxy}} | Options].

to_list(Value) when is_binary(Value) -> unicode:characters_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
