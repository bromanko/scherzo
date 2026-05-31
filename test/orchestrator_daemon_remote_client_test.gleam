import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/client as control_client
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/types as query_types
import scherzo/control/remote/client as remote_client
import scherzo/control/remote_envelope
import scherzo/daemon_identity
import scherzo/log
import scherzo/orchestrator/daemon
import scherzo/orchestrator/daemon_remote_client
import scherzo/path
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/tracker
import scherzo/tracker/adapter_legacy
import simplifile
import support/test_helpers
import test_async

type Connection {
  Connection(outbound: process.Subject(String), inbound_path: String)
}

type RemoteMode {
  RemoteConnectOk(Connection)
  RemoteConnectError(String)
}

type ControlMode {
  UseRealControlServer
  UseNoControlServer
}

pub fn daemon_disabled_ui_server_does_not_start_remote_client_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-disabled", False)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectError("unused"),
      [],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  test_async.assert_no_extra_message_within(starts, 100)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  test_async.assert_no_extra_message_within(stops, 100)
}

pub fn daemon_remote_client_start_reports_missing_endpoint_test() {
  let effective =
    direct_effective_config(
      "test/tmp/daemon-remote-client-direct-missing-endpoint",
      None,
      Some("test-token"),
    )

  let assert Error(daemon_remote_client.StartError(code: code, message: _)) =
    daemon_remote_client.start(
      effective,
      process.new_subject(),
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  assert code == "remote_client_config_missing"
}

pub fn daemon_remote_client_start_reports_missing_token_test() {
  let effective =
    direct_effective_config(
      "test/tmp/daemon-remote-client-direct-missing-token",
      Some("https://127.0.0.1:4443"),
      None,
    )

  let assert Error(daemon_remote_client.StartError(code: code, message: _)) =
    daemon_remote_client.start(
      effective,
      process.new_subject(),
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  assert code == "remote_client_config_missing"
}

pub fn daemon_remote_client_start_reports_invalid_endpoint_test() {
  let effective =
    direct_effective_config(
      "test/tmp/daemon-remote-client-direct-invalid-endpoint",
      Some("not-a-url"),
      Some("test-token"),
    )

  let assert Error(daemon_remote_client.StartError(code: code, message: _)) =
    daemon_remote_client.start(
      effective,
      process.new_subject(),
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  assert code == "remote_client_endpoint_missing_host"
}

pub fn daemon_remote_client_start_accepts_valid_config_test() {
  let effective =
    direct_effective_config(
      "test/tmp/daemon-remote-client-direct-valid",
      Some("https://127.0.0.1:1"),
      Some("test-token"),
    )
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })

  let assert Ok(handle) =
    daemon_remote_client.start(
      effective,
      event_hub,
      ["test-token"],
      fn(_, _, _, _) { Ok(Nil) },
    )
  assert daemon_remote_client.stop(handle, 2000) == Ok(Nil)
  hub.stop(event_hub)
}

pub fn daemon_enabled_ui_server_reuses_daemon_id_and_stops_remote_client_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-enabled", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let wire = new_wire()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(wire),
      [],
      UseNoControlServer,
    )

  let assert Ok(first) = daemon.start(Some(workflow_path), deps)
  let first_settings = test_async.expect_message(starts)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)
  assert daemon.shutdown(first.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"

  let second_wire = new_wire()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(second_wire),
      [],
      UseNoControlServer,
    )
  let assert Ok(second) = daemon.start(Some(workflow_path), deps)
  let second_settings = test_async.expect_message(starts)
  assert first_settings.daemon_id == second_settings.daemon_id
  assert first_settings.boot_id != second_settings.boot_id
  assert daemon.shutdown(second.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_shutdown_logs_remote_client_stop_timeout_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-stop-timeout", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let wire = new_wire()
  let logs = process.new_subject()
  let base_deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(wire),
      [],
      UseNoControlServer,
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      logger: fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
      stop_remote_client: fn(handle, timeout_ms) {
        process.send(stops, "stop")
        let _ = daemon_remote_client.stop(handle, timeout_ms)
        Error(Nil)
      },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
  assert list.any(test_async.drain_subject(logs), fn(entry) {
    string.contains(entry, "remote_client_shutdown_timeout")
  })
}

pub fn daemon_enabled_ui_server_unreachable_remote_does_not_block_snapshot_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-unreachable", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectError("unreachable_remote_transport"),
      [],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_enabled_ui_server_unreachable_remote_keeps_local_control_ping_test() {
  let dir = "test/tmp/daemon-remote-client-local-control"
  let workflow_path = write_workflow(dir, True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectError("unreachable_remote_transport"),
      [],
      UseRealControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let control_path = control_file.path_for_workspace(dir <> "/workspaces")
  let assert Ok(control) = control_file.read(control_path)
  assert control_client.ping(control) == Ok(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_restarts_remote_client_after_monitored_down_test() {
  let workflow_path = write_workflow("test/tmp/daemon-remote-client-down", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let handles = process.new_subject()
  let first_wire = new_wire()
  let deps =
    remote_dependencies(
      starts,
      stops,
      Some(handles),
      RemoteConnectOk(first_wire),
      [],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let first_handle = test_async.expect_message(handles)
  let _ = test_async.expect_message(first_wire.outbound)
  let _ = test_async.expect_message(first_wire.outbound)
  let _ = test_async.expect_message(first_wire.outbound)

  assert daemon_remote_client.stop(first_handle, 1000) == Ok(Nil)

  let _ = test_async.expect_message(starts)
  let _ = test_async.expect_message(handles)

  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_remote_client_state_snapshot_uses_event_hub_sessions_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-state", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let wire = new_wire()
  let summary = session_summary()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(wire),
      [summary],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)

  let assert Ok(remote_envelope.RemoteStateSnapshot(
    _,
    dispatch_paused,
    sessions,
  )) = remote_envelope.decode(test_async.expect_message(wire.outbound))
  assert dispatch_paused == False
  assert sessions == [expected_remote_session(summary)]

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_remote_query_returns_status_response_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-query", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let wire = new_wire()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(wire),
      [session_summary()],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let settings = test_async.expect_message(starts)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)

  append_inbound_line(
    wire.inbound_path,
    remote_envelope.RemoteQueryRequest("query-1", query_types.Status)
      |> remote_envelope.to_string,
  )
  let assert Ok(remote_envelope.RemoteQueryResponse("query-1", Ok(response))) =
    receive_envelope_of_kind(wire.outbound, "query_response")
  let query_types.StatusResponse(query_types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
  )) = response
  assert daemon_id == settings.daemon_id
  assert boot_id != ""
  assert dispatch_paused == False
  assert ui_server_enabled == True
  assert supported_queries == ["status"]

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

pub fn daemon_remote_pause_resume_uses_apply_operator_command_and_updates_state_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-remote-client-command", True)
  let starts = process.new_subject()
  let stops = process.new_subject()
  let wire = new_wire()
  let deps =
    remote_dependencies(
      starts,
      stops,
      None,
      RemoteConnectOk(wire),
      [session_summary()],
      UseNoControlServer,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let _ = test_async.expect_message(starts)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)
  let _ = test_async.expect_message(wire.outbound)

  append_inbound_line(
    wire.inbound_path,
    remote_envelope.RemoteServerCommand("pause-1", command.PauseDispatch)
      |> remote_envelope.to_string,
  )
  let assert Ok(remote_envelope.RemoteCommandReceipt("pause-1", True, _)) =
    receive_envelope_of_kind(wire.outbound, "command_receipt")
  let assert Ok(remote_envelope.RemoteCommandResult("pause-1", pause_result)) =
    receive_envelope_of_kind(wire.outbound, "command_result")
  assert pause_result.status == command.Applied
  let assert Ok(remote_envelope.RemoteStateSnapshot(_, True, _)) =
    receive_envelope_of_kind(wire.outbound, "state_snapshot")
  let assert Ok(True) = daemon.get_remote_dispatch_paused(started.data, 1000)

  append_inbound_line(
    wire.inbound_path,
    remote_envelope.RemoteServerCommand("resume-1", command.ResumeDispatch)
      |> remote_envelope.to_string,
  )
  let assert Ok(remote_envelope.RemoteCommandReceipt("resume-1", True, _)) =
    receive_envelope_of_kind(wire.outbound, "command_receipt")
  let assert Ok(remote_envelope.RemoteCommandResult("resume-1", resume_result)) =
    receive_envelope_of_kind(wire.outbound, "command_result")
  assert resume_result.status == command.Applied
  let assert Ok(remote_envelope.RemoteStateSnapshot(_, False, _)) =
    receive_envelope_of_kind(wire.outbound, "state_snapshot")
  let assert Ok(False) = daemon.get_remote_dispatch_paused(started.data, 1000)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert test_async.expect_message(stops) == "stop"
}

fn write_workflow(dir: String, ui_server_enabled: Bool) -> String {
  test_helpers.reset_dir(dir)
  let workflow_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, ui_server_enabled))
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
",
    )
  workflow_path
}

fn direct_effective_config(
  dir: String,
  endpoint: Option(String),
  enrollment_token: Option(String),
) -> config_types.EffectiveConfig {
  test_helpers.reset_dir(dir)
  let assert Ok(root) = path.absolute(dir)
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: root),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config_types.UiServerConfig(
      enabled: True,
      endpoint: endpoint,
      enrollment_token_env: None,
      enrollment_token: enrollment_token,
    ),
  )
}

fn workflow_text(root: String, ui_server_enabled: Bool) -> String {
  let ui_server = case ui_server_enabled {
    True ->
      "ui_server:\n  enabled: true\n  endpoint: https://127.0.0.1:4443\n  enrollment_token_env: HOME\n"
    False -> "ui_server:\n  enabled: false\n"
  }
  "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 1
  retries:
    attempts: 1
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
" <> ui_server
}

fn remote_dependencies(
  starts: process.Subject(remote_client.Settings),
  stops: process.Subject(String),
  handles: Option(process.Subject(daemon_remote_client.Handle)),
  mode: RemoteMode,
  session_summaries: List(event.SessionSummary),
  control_mode: ControlMode,
) -> daemon.RuntimeDependencies {
  let defaults = daemon.default_dependencies()
  daemon.RuntimeDependencies(
    ..defaults,
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(empty_tracker(), "linear")
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 42 },
    send_after: fn(subject, delay, message) {
      let _ = process.send_after(subject, delay, message)
      daemon.TestTimer(delay)
    },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { start_event_hub_with_sessions(session_summaries) },
    make_control_token: fn() {
      case control_mode {
        UseRealControlServer -> defaults.make_control_token()
        UseNoControlServer -> Ok("test-token")
      }
    },
    start_control_server: fn(settings, backend) {
      case control_mode {
        UseRealControlServer -> defaults.start_control_server(settings, backend)
        UseNoControlServer -> Ok(daemon.NoControlServer)
      }
    },
    stop_control_server: fn(handle) {
      case control_mode {
        UseRealControlServer -> defaults.stop_control_server(handle)
        UseNoControlServer -> Nil
      }
    },
    start_remote_client: fn(
      effective: config_types.EffectiveConfig,
      event_hub,
      daemon_subject,
      secrets,
      logger,
    ) {
      let assert Ok(identity) =
        daemon_identity.load_or_create(effective.workspace.root)
      let assert Some(endpoint) = effective.ui_server.endpoint
      let assert Some(enrollment_token) = effective.ui_server.enrollment_token
      let settings =
        remote_client.Settings(
          endpoint: endpoint,
          daemon_id: identity.daemon_id,
          boot_id: identity.boot_id,
          enrollment_token: enrollment_token,
          capabilities: [
            "control_commands",
            "session_snapshots",
            "read_queries",
          ],
          heartbeat_interval_ms: 1000,
          state_interval_ms: 1000,
          retry_initial_ms: 50,
          retry_max_ms: 100,
          connect_timeout_ms: 50,
          command_timeout_ms: 100,
          redaction_secrets: secrets,
        )
      let deps = client_dependencies(mode, logger, event_hub, daemon_subject)
      case remote_client.start(settings, deps) {
        Ok(handle) -> {
          let wrapped = daemon_remote_client.wrap(handle)
          process.send(starts, settings)
          case handles {
            Some(handle_subject) -> process.send(handle_subject, wrapped)
            None -> Nil
          }
          Ok(wrapped)
        }
        Error(remote_client.ClientError(code: code, message: message)) ->
          Error(daemon.StartupError(code, message))
      }
    },
    stop_remote_client: fn(handle, timeout_ms) {
      process.send(stops, "stop")
      daemon_remote_client.stop(handle, timeout_ms)
    },
    monitor_remote_client: daemon_remote_client.monitor,
  )
}

fn client_dependencies(
  mode: RemoteMode,
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(daemon.Message),
) -> remote_client.Dependencies(Connection, process.Timer) {
  remote_client.Dependencies(
    now_ms: fn() { 42 },
    connect: fn(_, _) {
      case mode {
        RemoteConnectOk(connection) -> Ok(connection)
        RemoteConnectError(message) -> Error(message)
      }
    },
    send_line: fn(connection, line, _) {
      process.send(connection.outbound, line)
      Ok(Nil)
    },
    recv_line: fn(connection, _) { read_inbound_line(connection.inbound_path) },
    close: fn(_) { Nil },
    send_after: process.send_after,
    cancel_timer: fn(timer) {
      let _ = process.cancel_timer(timer)
      Nil
    },
    list_sessions: fn() {
      daemon_remote_client.list_sessions_for_remote_snapshot(event_hub, 1000)
    },
    apply_command: fn(operator_command, timeout_ms) {
      daemon.apply_operator_command(
        daemon_subject,
        operator_command,
        timeout_ms,
      )
    },
    execute_query: fn(query) {
      daemon.execute_query(daemon_subject, query, 1000)
    },
    dispatch_paused: fn(timeout_ms) {
      case daemon.get_remote_dispatch_paused(daemon_subject, timeout_ms) {
        Ok(value) -> Ok(value)
        Error(Nil) -> Error("dispatch_paused_timeout")
      }
    },
    logger: logger,
  )
}

fn start_event_hub_with_sessions(
  session_summaries: List(event.SessionSummary),
) -> Result(process.Subject(hub.Message), hub.HubError) {
  let assert Ok(subject) = hub.start(20, fn() { 42 })
  session_summaries
  |> list.each(fn(summary) { hub.register_session(subject, summary) })
  Ok(subject)
}

fn session_summary() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    display_name: "Demo session",
    issue_id: "issue-1",
    issue_identifier: "LIV-686",
    issue_title: "Remote state",
    workspace_path: "test/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 3,
    current_turn_status: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    current_turn_started_at_ms: None,
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 123,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn expected_remote_session(
  summary: event.SessionSummary,
) -> remote_envelope.RemoteSession {
  remote_envelope.RemoteSession(
    session_id: summary.session_id,
    display_name: summary.display_name,
    issue_identifier: summary.issue_identifier,
    status: event.status_to_string(summary.status),
    current_turn: summary.current_turn,
    last_event_at_ms: summary.last_event_at_ms,
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn new_wire() -> Connection {
  let root =
    "test/tmp/daemon-remote-client-wire/" <> int.to_string(unique_integer())
  test_helpers.reset_dir(root)
  Connection(process.new_subject(), root <> "/inbound.txt")
}

fn receive_envelope_of_kind(
  outbound: process.Subject(String),
  expected: String,
) -> Result(remote_envelope.Envelope, remote_envelope.DecodeError) {
  let decoded = remote_envelope.decode(test_async.expect_message(outbound))
  case decoded {
    Ok(envelope) ->
      case envelope_kind(envelope) == expected {
        True -> Ok(envelope)
        False -> receive_envelope_of_kind(outbound, expected)
      }
    Error(_) -> receive_envelope_of_kind(outbound, expected)
  }
}

fn envelope_kind(envelope: remote_envelope.Envelope) -> String {
  case envelope {
    remote_envelope.RemoteHello(_) -> "hello"
    remote_envelope.RemoteHeartbeat(_) -> "heartbeat"
    remote_envelope.RemoteServerCommand(_, _) -> "server_command"
    remote_envelope.RemoteQueryRequest(_, _) -> "query_request"
    remote_envelope.RemoteCommandReceipt(_, _, _) -> "command_receipt"
    remote_envelope.RemoteCommandResult(_, _) -> "command_result"
    remote_envelope.RemoteQueryResponse(_, _) -> "query_response"
    remote_envelope.RemoteStateSnapshot(_, _, _) -> "state_snapshot"
  }
}

fn append_inbound_line(path: String, line: String) -> Nil {
  let existing = case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
  let assert Ok(Nil) = simplifile.write(path, existing <> line <> "\n")
  Nil
}

fn read_inbound_line(path: String) -> Result(String, String) {
  case simplifile.read(path) {
    Error(_) -> Error("timeout")
    Ok(contents) ->
      case string.split(contents, "\n") {
        [first, ..rest] if first != "" -> {
          let remaining = rest |> string.join(with: "\n")
          let assert Ok(Nil) = simplifile.write(path, remaining)
          Ok(first)
        }
        _ -> Error("timeout")
      }
  }
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int
