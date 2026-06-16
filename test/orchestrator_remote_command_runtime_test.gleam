import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/control/query/types as query_types
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
      execute_query: fn(_, _, _) {
        Ok(
          query_types.StatusResponse(
            query_types.StatusDto(
              daemon_id: "daemon_abc",
              boot_id: "boot_abc",
              dispatch_paused: False,
              ui_server_enabled: True,
              supported_queries: ["status"],
            ),
          ),
        )
      },
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
      execute_query: fn(_, _, _) {
        Ok(
          query_types.StatusResponse(
            query_types.StatusDto(
              daemon_id: "daemon_abc",
              boot_id: "boot_abc",
              dispatch_paused: False,
              ui_server_enabled: True,
              supported_queries: ["status"],
            ),
          ),
        )
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

pub fn execute_remote_query_delegates_to_daemon_callback_test() {
  let daemon_subject = process.new_subject()
  let calls = process.new_subject()
  let dependencies =
    remote_command_runtime.control_dependencies(
      apply_operator_command: fn(_, operator_command, _) {
        Ok(command.applied(operator_command, None))
      },
      get_remote_dispatch_paused: fn(_, _) { Ok(False) },
      execute_query: fn(subject, query, timeout_ms) {
        process.send(calls, #(subject, query, timeout_ms))
        Ok(
          query_types.StatusResponse(
            query_types.StatusDto(
              daemon_id: "daemon_abc",
              boot_id: "boot_abc",
              dispatch_paused: False,
              ui_server_enabled: True,
              supported_queries: ["status"],
            ),
          ),
        )
      },
    )

  let assert Ok(result) =
    remote_command_runtime.execute_remote_query(
      daemon_subject,
      query_types.Status,
      250,
      dependencies,
    )

  let assert Ok(#(called_subject, called_query, called_timeout)) =
    process.receive(calls, within: 1000)
  assert called_subject == daemon_subject
  assert called_query == query_types.Status
  assert called_timeout == 250
  assert result
    == query_types.StatusResponse(
      query_types.StatusDto(
        daemon_id: "daemon_abc",
        boot_id: "boot_abc",
        dispatch_paused: False,
        ui_server_enabled: True,
        supported_queries: ["status"],
      ),
    )
}
