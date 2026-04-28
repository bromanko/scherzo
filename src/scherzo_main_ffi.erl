-module(scherzo_main_ffi).
-export([args/0, halt/1]).

args() ->
    [unicode:characters_to_binary(A) || A <- init:get_plain_arguments()].

halt(Code) ->
    erlang:halt(Code).
