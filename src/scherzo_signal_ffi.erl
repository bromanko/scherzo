-module(scherzo_signal_ffi).

-behaviour(gen_event).

-export([install_sigterm/1, cleanup_sigterm/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

install_sigterm(Subject) ->
    try
        case os:set_signal(sigterm, handle) of
            ok -> install_sigterm_handler(Subject);
            Other -> {error, format_error("os:set_signal(sigterm, handle) returned ~p", [Other])}
        end
    catch
        Class:Reason:Stack ->
            {error, format_error("install_sigterm failed: ~p:~p ~p", [Class, Reason, Stack])}
    end.

cleanup_sigterm({scherzo_signal_handle, HandlerId, RestoreDefault}) ->
    _ = delete_handler(HandlerId),
    case RestoreDefault of
        true -> restore_default_handler();
        false -> ok
    end,
    nil;
cleanup_sigterm(_Other) ->
    nil.

install_sigterm_handler(Subject) ->
    HandlerId = {?MODULE, make_ref()},
    case which_handlers() of
        {error, Message} -> {error, Message};
        {ok, Handlers} ->
            RestoreDefault = lists:member(erl_signal_handler, Handlers),
            case install_custom_handler(HandlerId, Subject, RestoreDefault) of
                ok ->
                    case remove_remaining_default_handlers() of
                        ok ->
                            case ensure_default_removed() of
                                ok ->
                                    {ok, {{scherzo_signal_handle, HandlerId, RestoreDefault}, os_pid_binary()}};
                                {error, Message} ->
                                    cleanup_after_failed_install(HandlerId, RestoreDefault),
                                    {error, Message}
                            end;
                        {error, Message} ->
                            cleanup_after_failed_install(HandlerId, RestoreDefault),
                            {error, Message}
                    end;
                {error, Message} -> {error, Message}
            end
    end.

install_custom_handler(HandlerId, Subject, true) ->
    case catch gen_event:swap_handler(
        erl_signal_server,
        {erl_signal_handler, scherzo_takeover},
        {HandlerId, Subject}
    ) of
        ok -> ok;
        {'EXIT', Reason} -> {error, format_error("swap SIGTERM handler failed: ~p", [Reason])};
        Other -> {error, format_error("swap SIGTERM handler returned ~p", [Other])}
    end;
install_custom_handler(HandlerId, Subject, false) ->
    case catch gen_event:add_handler(erl_signal_server, HandlerId, Subject) of
        ok -> ok;
        {'EXIT', Reason} -> {error, format_error("add SIGTERM handler failed: ~p", [Reason])};
        Other -> {error, format_error("add SIGTERM handler returned ~p", [Other])}
    end.

cleanup_after_failed_install(HandlerId, RestoreDefault) ->
    _ = delete_handler(HandlerId),
    case RestoreDefault of
        true -> restore_default_handler();
        false -> ok
    end.

remove_remaining_default_handlers() ->
    case which_handlers() of
        {error, Message} -> {error, Message};
        {ok, Handlers} ->
            case lists:member(erl_signal_handler, Handlers) of
                false -> ok;
                true ->
                    case delete_default_handler() of
                        ok -> remove_remaining_default_handlers();
                        {error, Message} -> {error, Message}
                    end
            end
    end.

ensure_default_removed() ->
    case which_handlers() of
        {error, Message} -> {error, Message};
        {ok, Handlers} ->
            case lists:member(erl_signal_handler, Handlers) of
                false -> ok;
                true -> {error, <<"erl_signal_handler remained installed after SIGTERM handler replacement">>}
            end
    end.

delete_default_handler() ->
    case catch gen_event:delete_handler(erl_signal_server, erl_signal_handler, remove) of
        ok -> ok;
        {'EXIT', Reason} -> {error, format_error("delete default SIGTERM handler failed: ~p", [Reason])};
        Other -> {error, format_error("delete default SIGTERM handler returned ~p", [Other])}
    end.

delete_handler(HandlerId) ->
    case catch gen_event:delete_handler(erl_signal_server, HandlerId, cleanup) of
        ok -> ok;
        {'EXIT', _Reason} -> ok;
        _Other -> ok
    end.

restore_default_handler() ->
    case which_handlers() of
        {error, _Message} -> ok;
        {ok, Handlers} ->
            case lists:member(erl_signal_handler, Handlers) of
                true -> ok;
                false ->
                    case catch gen_event:add_handler(erl_signal_server, erl_signal_handler, []) of
                        ok -> ok;
                        {'EXIT', _Reason} -> ok;
                        _Other -> ok
                    end
            end
    end.

which_handlers() ->
    case catch gen_event:which_handlers(erl_signal_server) of
        {'EXIT', Reason} -> {error, format_error("erl_signal_server unavailable: ~p", [Reason])};
        Handlers when is_list(Handlers) -> {ok, Handlers};
        Other -> {error, format_error("erl_signal_server returned unexpected handlers: ~p", [Other])}
    end.

os_pid_binary() ->
    unicode:characters_to_binary(os:getpid()).

format_error(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).

init({Subject, _OldTerminateResult}) ->
    init(Subject);
init(Subject) ->
    {ok, #{subject => Subject, delivered => false}}.

handle_event(sigterm, #{subject := Subject, delivered := false} = State) ->
    gleam@erlang@process:send(Subject, sigterm),
    {ok, State#{delivered => true}};
handle_event(sigterm, State) ->
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
