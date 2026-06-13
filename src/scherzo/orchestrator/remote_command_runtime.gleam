import gleam/erlang/process
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/log
import scherzo/orchestrator/daemon_remote_client
import scherzo/session/hub

pub type Handle =
  daemon_remote_client.Handle

pub type StartError =
  daemon_remote_client.StartError

pub fn start_error_fields(error: StartError) -> #(String, String) {
  let daemon_remote_client.StartError(code: code, message: message) = error
  #(code, message)
}

pub opaque type ControlDependencies(message) {
  ControlDependencies(
    apply_operator_command: fn(
      process.Subject(message),
      command.OperatorCommand,
      Int,
    ) -> Result(command.CommandResult, Nil),
    get_remote_dispatch_paused: fn(process.Subject(message), Int) ->
      Result(Bool, Nil),
  )
}

pub fn control_dependencies(
  apply_operator_command apply_operator_command: fn(
    process.Subject(message),
    command.OperatorCommand,
    Int,
  ) -> Result(command.CommandResult, Nil),
  get_remote_dispatch_paused get_remote_dispatch_paused: fn(
    process.Subject(message),
    Int,
  ) -> Result(Bool, Nil),
) -> ControlDependencies(message) {
  ControlDependencies(
    apply_operator_command: apply_operator_command,
    get_remote_dispatch_paused: get_remote_dispatch_paused,
  )
}

pub fn apply_remote_command(
  daemon_subject: process.Subject(message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  dependencies: ControlDependencies(message),
) -> Result(command.CommandResult, Nil) {
  dependencies.apply_operator_command(
    daemon_subject,
    operator_command,
    timeout_ms,
  )
}

pub fn read_remote_dispatch_paused(
  daemon_subject: process.Subject(message),
  timeout_ms: Int,
  dependencies: ControlDependencies(message),
) -> Result(Bool, Nil) {
  dependencies.get_remote_dispatch_paused(daemon_subject, timeout_ms)
}

pub fn start_remote_client(
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(message),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
  dependencies: ControlDependencies(message),
) -> Result(Handle, StartError) {
  daemon_remote_client.start_with_control(
    effective,
    event_hub,
    fn(timeout_ms) {
      read_remote_dispatch_paused(daemon_subject, timeout_ms, dependencies)
    },
    secrets,
    logger,
  )
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  daemon_remote_client.stop(handle, timeout_ms)
}

pub fn monitor(handle: Handle) -> process.Monitor {
  daemon_remote_client.monitor(handle)
}

pub fn kill(handle: Handle) -> Nil {
  daemon_remote_client.kill(handle)
}
