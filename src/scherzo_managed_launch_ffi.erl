-module(scherzo_managed_launch_ffi).

-include_lib("kernel/include/file.hrl").

-export([validate_private_regular_file/2]).

validate_private_regular_file(Path, ParentDir) ->
    try
        PathList = to_list(Path),
        ParentList = to_list(ParentDir),
        case validate_not_symlink(PathList, grant_file_symlink) of
            ok -> validate_file_and_parent(PathList, ParentList);
            {error, Code} -> {error, atom_to_binary(Code, utf8)}
        end
    catch
        _:_ -> {error, <<"grant_file_unsafe">>}
    end.

validate_file_and_parent(Path, Parent) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, uid = Uid, mode = Mode}} ->
            case validate_owner(Uid, grant_file_wrong_owner) of
                ok ->
                    case validate_private_mode(Mode, grant_file_permissions_loose) of
                        ok -> validate_parent(Parent);
                        {error, Code} -> {error, atom_to_binary(Code, utf8)}
                    end;
                {error, Code} -> {error, atom_to_binary(Code, utf8)}
            end;
        {ok, #file_info{type = _}} -> {error, <<"grant_file_non_regular">>};
        {error, _} -> {error, <<"grant_file_non_regular">>}
    end.

validate_parent(Parent) ->
    case validate_not_symlink(Parent, grant_file_parent_symlink) of
        ok ->
            case file:read_file_info(Parent) of
                {ok, #file_info{type = directory, uid = Uid, mode = Mode}} ->
                    case validate_owner(Uid, grant_file_parent_wrong_owner) of
                        ok ->
                            case validate_private_mode(Mode, grant_file_parent_permissions_loose) of
                                ok -> {ok, nil};
                                {error, Code} -> {error, atom_to_binary(Code, utf8)}
                            end;
                        {error, Code} -> {error, atom_to_binary(Code, utf8)}
                    end;
                {ok, #file_info{type = _}} -> {error, <<"grant_file_parent_non_directory">>};
                {error, _} -> {error, <<"grant_file_parent_non_directory">>}
            end;
        {error, Code} -> {error, atom_to_binary(Code, utf8)}
    end.

validate_not_symlink(Path, Code) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = symlink}} -> {error, Code};
        {ok, _} -> ok;
        {error, _} -> ok
    end.

validate_owner(Uid, Code) ->
    case current_uid() of
        {ok, Uid} -> ok;
        {ok, _} -> {error, Code};
        {error, current_uid_unavailable} -> {error, grant_file_owner_unknown}
    end.

validate_private_mode(Mode, Code) ->
    case (Mode band 8#077) =:= 0 of
        true -> ok;
        false -> {error, Code}
    end.

current_uid() ->
    case os:getenv("UID") of
        false -> current_uid_from_os();
        Value ->
            case catch list_to_integer(Value) of
                Int when is_integer(Int) -> {ok, Int};
                _ -> current_uid_from_os()
            end
    end.

current_uid_from_os() ->
    Value = string:trim(os:cmd("id -u")),
    case catch list_to_integer(Value) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, current_uid_unavailable}
    end.

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.
