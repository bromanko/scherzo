import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/control/query/service as query_service
import scherzo/control/query/types as query_types
import scherzo/orchestrator/control_plane_runtime
import scherzo/orchestrator/remote_command_runtime

pub fn remote_and_control_owner_states_ignore_stale_monitors_test() {
  let query_backend =
    query_service.Backend(run: fn(_) {
      Error(query_types.QueryError(query_types.QueryBackendFailed, "unused"))
    })
  let assert Ok(query_handle) =
    query_service.start(
      query_service.Settings(max_concurrent: 1, max_queued: 1, timeout_ms: 50),
      query_backend,
    )
  let waiting = process.new_subject()
  let first_pid =
    process.spawn_unlinked(fn() {
      let _ = process.receive(waiting, within: 10_000)
      Nil
    })
  let second_pid =
    process.spawn_unlinked(fn() {
      let _ = process.receive(waiting, within: 10_000)
      Nil
    })
  let first_monitor = process.monitor(first_pid)
  let second_monitor = process.monitor(second_pid)
  let control =
    control_plane_runtime.new(
      control_plane_runtime.NoControlServer,
      Some(first_monitor),
      Some("control.json"),
      query_handle,
    )
  assert control_plane_runtime.monitor_matches(control, first_monitor)
  assert !control_plane_runtime.monitor_matches(control, second_monitor)
  let control = control_plane_runtime.cleared(control)
  assert control_plane_runtime.monitor(control) == None
  assert control_plane_runtime.control_file_path(control) == None

  let remote = remote_command_runtime.new(None)
  assert remote_command_runtime.handle(remote) == None
  assert !remote_command_runtime.monitor_matches(remote, first_monitor)
  assert remote_command_runtime.managed_launch(remote) == None

  process.demonitor_process(first_monitor)
  process.demonitor_process(second_monitor)
  process.kill(first_pid)
  process.kill(second_pid)
  assert query_service.stop(query_handle, 1000) == Ok(Nil)
}

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
