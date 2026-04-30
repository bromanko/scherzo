import gleam/erlang/process

@external(erlang, "erlang", "spawn_monitor")
pub fn spawn_monitor(
  running: fn() -> anything,
) -> #(process.Pid, process.Monitor)

@external(erlang, "scherzo_process_ext_ffi", "trap_exits")
pub fn trap_exits(enabled: Bool) -> Bool
