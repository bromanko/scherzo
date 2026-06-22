-module(scherzo_code_snapshot_ffi).
-export([ensure_scherzo_modules_loaded/0]).

ensure_scherzo_modules_loaded() ->
    try
        case ensure_application_loaded() of
            ok -> ensure_application_modules();
            {error, _Reason} -> ensure_modules_from_current_code_path()
        end
    catch
        CatchClass:CatchReason -> {error, format_error(CatchClass, CatchReason)}
    end.

ensure_application_loaded() ->
    case application:load(scherzo) of
        ok -> ok;
        {error, {already_loaded, scherzo}} -> ok;
        {error, Reason} -> {error, tagged_error(application_load, reason_to_binary(Reason))}
    end.

ensure_application_modules() ->
    case application:get_key(scherzo, modules) of
        {ok, []} -> ensure_modules_from_ebin();
        {ok, Modules} -> ensure_modules(Modules, 0);
        undefined -> {error, tagged_error(application_modules, <<"undefined">>)};
        {error, Reason} -> {error, tagged_error(application_modules, reason_to_binary(Reason))}
    end.

ensure_modules_from_ebin() ->
    case code:lib_dir(scherzo) of
        {error, bad_name} -> ensure_modules_from_current_code_path();
        LibDir ->
            Pattern = filename:join([LibDir, "ebin", "*.beam"]),
            Modules = lists:filtermap(fun beam_path_to_module/1, filelib:wildcard(Pattern)),
            case Modules of
                [] -> ensure_modules_from_current_code_path();
                _ -> ensure_modules(Modules, 0)
            end
    end.

ensure_modules_from_current_code_path() ->
    case code:which(?MODULE) of
        non_existing -> {error, tagged_error(application_modules, <<"module_path_missing">>)};
        ModulePath ->
            Pattern = filename:join([filename:dirname(ModulePath), "*.beam"]),
            Modules = lists:filtermap(fun beam_path_to_module/1, filelib:wildcard(Pattern)),
            ensure_modules(Modules, 0)
    end.

beam_path_to_module(Path) ->
    BaseName = filename:basename(Path, ".beam"),
    case should_load_module(BaseName) of
        true -> {true, binary_to_atom(unicode:characters_to_binary(BaseName), utf8)};
        false -> false
    end.

should_load_module("scherzo_code_snapshot_ffi") ->
    false;
should_load_module("scherzo") ->
    true;
should_load_module(Name) ->
    lists:prefix("scherzo@", Name) orelse lists:prefix("scherzo_", Name).

ensure_modules([], Count) ->
    {ok, Count};
ensure_modules([Module | Rest], Count) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> ensure_modules(Rest, Count + 1);
        {error, Reason} ->
            {error,
             <<"ensure_loaded:",
               (atom_to_binary(Module, utf8))/binary,
               ":",
               (reason_to_binary(Reason))/binary>>}
    end.

tagged_error(Tag, Reason) when is_atom(Tag) ->
    TagBin = atom_to_binary(Tag, utf8),
    <<TagBin/binary, ":", Reason/binary>>.

reason_to_binary(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
reason_to_binary(Reason) when is_binary(Reason) -> Reason;
reason_to_binary(Reason) -> unicode:characters_to_binary(io_lib:format("~p", [Reason])).

format_error(Class, Reason) ->
    unicode:characters_to_binary(io_lib:format("~p:~p", [Class, Reason])).
