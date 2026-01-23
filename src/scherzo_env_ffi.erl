-module(scherzo_env_ffi).
-export([getenv/1]).

%% Get environment variable, returning empty binary if not set
getenv(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> <<>>;
        Value -> list_to_binary(Value)
    end.
