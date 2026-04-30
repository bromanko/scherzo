import gleam/erlang/process

@external(erlang, "erlang", "spawn_monitor")
pub fn spawn_monitor(
  running: fn() -> anything,
) -> #(process.Pid, process.Monitor)
