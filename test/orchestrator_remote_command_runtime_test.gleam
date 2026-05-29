import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/orchestrator/remote_command_runtime

pub fn apply_remote_command_delegates_to_daemon_callback_test() {
  let daemon_subject = process.new_subject()
  let calls = process.new_subject()
  let dependencies =
    remote_command_runtime.control_dependencies(
      apply_operator_command: fn(subject, operator_command, timeout_ms) {
        process.send(calls, #(subject, operator_command, timeout_ms))
        Ok(command.applied(operator_command, Some("applied remotely")))
      },
      get_remote_dispatch_paused: fn(_, _) { Ok(False) },
    )

  let assert Ok(result) =
    remote_command_runtime.apply_remote_command(
      daemon_subject,
      command.PauseDispatch,
      250,
      dependencies,
    )

  let assert Ok(#(called_subject, called_command, called_timeout)) =
    process.receive(calls, within: 1000)
  assert called_subject == daemon_subject
  assert called_command == command.PauseDispatch
  assert called_timeout == 250
  assert result.status == command.Applied
}

pub fn read_remote_dispatch_paused_delegates_to_daemon_callback_test() {
  let daemon_subject = process.new_subject()
  let calls = process.new_subject()
  let dependencies =
    remote_command_runtime.control_dependencies(
      apply_operator_command: fn(_, operator_command, _) {
        Ok(command.applied(operator_command, None))
      },
      get_remote_dispatch_paused: fn(subject, timeout_ms) {
        process.send(calls, #(subject, timeout_ms))
        Ok(True)
      },
    )

  let assert Ok(paused) =
    remote_command_runtime.read_remote_dispatch_paused(
      daemon_subject,
      500,
      dependencies,
    )

  let assert Ok(#(called_subject, called_timeout)) =
    process.receive(calls, within: 1000)
  assert called_subject == daemon_subject
  assert called_timeout == 500
  assert paused
}
