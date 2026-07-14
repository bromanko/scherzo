-module(scherzo_ledger_cache_ffi).
-export([get/1, put/2, delete/1]).

-define(TABLE, scherzo_ledger_cache).
-define(KEEPER, scherzo_ledger_cache_keeper).

get(Key) ->
    ensure_table(),
    case ets:lookup(?TABLE, to_binary(Key)) of
        [{_, Value}] -> {some, Value};
        [] -> none
    end.

put(Key, Value) ->
    ensure_table(),
    true = ets:insert(?TABLE, {to_binary(Key), Value}),
    nil.

delete(Key) ->
    ensure_table(),
    true = ets:delete(?TABLE, to_binary(Key)),
    nil.

ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined -> start_keeper();
        _Table -> ok
    end.

start_keeper() ->
    Parent = self(),
    Pid = spawn(fun() -> keeper(Parent) end),
    receive
        {Pid, ready} -> ok;
        {Pid, retry} -> ensure_table()
    after 1000 -> ensure_table()
    end.

keeper(Parent) ->
    try register(?KEEPER, self()) of
        true ->
            _ = ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            Parent ! {self(), ready},
            keeper_loop()
    catch
        error:badarg -> wait_for_table(Parent, 1000)
    end.

wait_for_table(Parent, 0) ->
    Parent ! {self(), retry},
    ok;
wait_for_table(Parent, Remaining) ->
    case ets:whereis(?TABLE) of
        undefined ->
            receive
            after 1 -> wait_for_table(Parent, Remaining - 1)
            end;
        _Table ->
            Parent ! {self(), ready},
            ok
    end.

keeper_loop() ->
    receive
        stop -> ok;
        _ -> keeper_loop()
    end.

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value);
to_binary(Value) -> unicode:characters_to_binary(io_lib:format("~p", [Value])).
