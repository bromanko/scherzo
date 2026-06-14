import gleam/dict
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/control/client
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/dto
import scherzo/control/query/types as query_types
import scherzo/control/remote/ui_websocket_client
import scherzo/control/server as control_server
import scherzo/error
import scherzo/handoff
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/daemon_remote_client
import scherzo/orchestrator/read_model
import scherzo/path
import scherzo/result_artifact
import scherzo/runtime/state as orchestrator_state
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/turn_telemetry
import scherzo/workflow_attempt
import scherzo/workflow_run
import simplifile
import support/test_helpers
import test_async

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.StepRecoveryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn workflow_text(root: String) -> String {
  workflow_text_with_extra_config(root, 0, 1, "")
}

fn workflow_text_with_extra_config(
  root: String,
  max_concurrent_agents: Int,
  max_sessions_per_issue: Int,
  extra_config: String,
) -> String {
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
  concurrency: " <> int_to_string(max_concurrent_agents) <> "
  sessions_per_task: " <> int_to_string(max_sessions_per_issue) <> "
  runtime:
    type: pi
    pi:
      executable: fake
" <> extra_config <> "task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
"
}

fn write_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  #(write_workflow_files(dir, workflow_text(root)), root)
}

fn write_workflow_with_limits(
  dir: String,
  max_concurrent_agents: Int,
  max_sessions_per_issue: Int,
) -> #(String, String) {
  write_workflow_with_extra_config(
    dir,
    max_concurrent_agents,
    max_sessions_per_issue,
    "",
  )
}

fn write_workflow_with_extra_config(
  dir: String,
  max_concurrent_agents: Int,
  max_sessions_per_issue: Int,
  extra_config: String,
) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  #(
    write_workflow_files(
      dir,
      workflow_text_with_extra_config(
        root,
        max_concurrent_agents,
        max_sessions_per_issue,
        extra_config,
      ),
    ),
    root,
  )
}

fn write_workflow_files(dir: String, config_text: String) -> String {
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(config_path, config_text)
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
  config_path
}

fn reload_snapshot_workflow_text(
  root: String,
  default_workflow: Option(String),
) -> String {
  let default_line = case default_workflow {
    Some(workflow_id) -> "    default_workflow: " <> workflow_id <> "\n"
    None -> ""
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
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
" <> default_line <> "workflows:
  implementation: workflows/implementation.yaml
  review: workflows/review.yaml
"
}

fn write_reload_snapshot_workflow(
  dir: String,
  default_workflow: Option(String),
) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      prompt_dir <> "/implementation.md",
      "Implementation Prompt",
    )
  let assert Ok(Nil) =
    simplifile.write(prompt_dir <> "/review.md", "Review Prompt")
  write_reload_snapshot_workflow_file(
    workflow_dir <> "/implementation.yaml",
    "implementation",
    "prompts/implementation.md",
  )
  write_reload_snapshot_workflow_file(
    workflow_dir <> "/review.yaml",
    "review",
    "prompts/review.md",
  )
  overwrite_reload_snapshot_config(config_path, root, default_workflow)
  #(config_path, root)
}

fn write_reload_snapshot_workflow_file(
  path: String,
  workflow_id: String,
  prompt_path: String,
) -> Nil {
  let assert Ok(Nil) = simplifile.write(path, "version: 1
id: " <> workflow_id <> "
steps:
  - id: run
    kind: agent
    prompt: " <> prompt_path <> "
    run_in: main
")
  Nil
}

fn overwrite_reload_snapshot_config(
  config_path: String,
  root: String,
  default_workflow: Option(String),
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      reload_snapshot_workflow_text(root, default_workflow),
    )
  Nil
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn dependencies(
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(empty_tracker(), "linear")
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, fields, _) {
      process.send(log_subject, control_log_value(event, fields))
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
  )
}

fn dependencies_with_tracker(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..dependencies(log_subject),
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
    },
  )
}

fn in_process_dependencies(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
  handoff_client: handoff.Client,
  hub_subject: process.Subject(hub.Message),
  agent_runner: fn(
    tracker_issue.Issue,
    Option(Int),
    String,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(String, agent_types.RunnerUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..dependencies_with_tracker(log_subject, tracker_client),
    make_tracker_adapter: fn(_) {
      adapter.TrackerAdapter(
        ..adapter_legacy.adapter_from_legacy_client(tracker_client, "linear"),
        handoff: Some(test_handoff_capability(handoff_client)),
      )
    },
    workflow_run_dependencies: workflow_deps_from_agent(agent_runner),
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

type FakeRemoteClientConnection {
  FakeRemoteClientConnection
}

fn ui_server_config_text(enabled: Bool) -> String {
  case enabled {
    True ->
      "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  credential_ref: work-laptop\n"
    False -> "ui_server:\n  enabled: false\n"
  }
}

fn overwrite_workflow_config(
  workflow_path: String,
  root: String,
  ui_server_enabled: Bool,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      workflow_text_with_extra_config(
        root,
        0,
        1,
        ui_server_config_text(ui_server_enabled),
      ),
    )
  Nil
}

fn expected_resolved_workspace_root(
  workflow_path: String,
  root: String,
) -> String {
  case path.is_absolute(root) {
    True -> root
    False -> {
      let assert Ok(config_dir) = path.dirname(workflow_path)
      path.absolute_or_original(path.join(config_dir, root))
    }
  }
}

fn fake_remote_client_handle(key: String) -> daemon_remote_client.Handle {
  let settings =
    ui_websocket_client.Settings(
      server_url: "https://ui.example.test",
      websocket_url: "wss://ui.example.test/api/daemons/ws",
      daemon_id: "daemon_abc",
      boot_id: "boot_abc",
      daemon_label: None,
      credential: "dcred_secret_1",
      heartbeat_interval_ms: 1000,
      state_interval_ms: 1000,
      retry_initial_ms: 50,
      retry_max_ms: 100,
      connect_timeout_ms: 50,
      command_bridge_enabled: False,
      redaction_secrets: ["dcred_secret_1"],
    )
  let deps =
    ui_websocket_client.Dependencies(
      now_ms: fn() { 42 },
      connect: fn(_, _, _) { Ok(FakeRemoteClientConnection) },
      send_text: fn(_, _, _) { Ok(Nil) },
      recv_text: fn(_, _) { Error("timeout") },
      close: fn(_) { Nil },
      send_after: process.send_after,
      cancel_timer: fn(timer) {
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() { Ok([]) },
      dispatch_paused: fn(_) { Ok(False) },
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let _ = key
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)
  daemon_remote_client.wrap(handle)
}

fn remote_client_dependencies(
  log_subject: process.Subject(String),
  starts: process.Subject(String),
  stops: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..dependencies(log_subject),
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
    start_remote_client: fn(effective: config_types.EffectiveConfig, _, _, _, _) {
      process.send(starts, effective.workspace.root)
      Ok(fake_remote_client_handle(effective.workspace.root))
    },
    stop_remote_client: fn(handle, _) {
      process.send(stops, "stop")
      daemon_remote_client.stop(handle, 1000)
    },
    monitor_remote_client: daemon_remote_client.monitor,
  )
}

fn test_handoff_capability(
  client: handoff.Client,
) -> adapter.HandoffCapability {
  adapter.HandoffCapability(report: fn(event) {
    case event {
      adapter.HandoffClaim(task_context, _, run_id) ->
        map_tracker_nil(client.claim_issue(
          task.to_runtime_issue(task_context),
          run_id,
        ))
      adapter.HandoffSuccess(task_context, success, run_id, workflow_id) ->
        map_tracker_nil(client.report_success_for_workflow(
          task.to_runtime_issue(task_context),
          success,
          run_id,
          workflow_id,
        ))
      adapter.HandoffFailure(task_context, failure, run_id, workflow_id) ->
        map_tracker_nil(client.report_failure_for_workflow(
          task.to_runtime_issue(task_context),
          failure,
          run_id,
          workflow_id,
        ))
      adapter.HandoffPark(report) ->
        map_tracker_nil(
          client.report_park(handoff.ParkReport(
            issue_id: report.task.remote_id,
            issue_identifier: report.issue_identifier,
            reason: report.reason,
            release_policy: report.release_policy,
            run_id: report.run_id,
          )),
        )
    }
  })
}

fn map_tracker_nil(
  result: Result(Nil, error.TrackerError),
) -> Result(Nil, adapter.TrackerError) {
  case result {
    Ok(Nil) -> Ok(Nil)
    Error(error.LinearApiRequest(message)) -> Error(adapter.Permanent(message))
    Error(_) -> Error(adapter.Permanent("tracker error"))
  }
}

fn workflow_deps_from_agent(
  agent_runner: fn(
    tracker_issue.Issue,
    Option(Int),
    String,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(String, agent_types.RunnerUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    ..workflow_run.default_dependencies(),
    agent_step: fn(
      issue,
      _context,
      prompt_mode,
      _attempt_context,
      effective,
      tracker_client,
      emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      agent_runner(
        issue,
        None,
        prompt_text(prompt_mode),
        effective,
        tracker_client,
        fn(_, update) { emit_update(update) },
        command_subject,
        fn() { command_ready(command_subject) },
      )
    },
  )
}

fn disabled_handoff() -> handoff.Client {
  handoff.disabled_client()
}

fn park_reporting_handoff(subject: process.Subject(String)) -> handoff.Client {
  handoff.Client(
    claim_issue: fn(_, _) { Ok(Nil) },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
    report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_park: fn(report) {
      process.send(subject, park_report_message(report))
      Ok(Nil)
    },
  )
}

fn park_report_message(report: handoff.ParkReport) -> String {
  "park:"
  <> report.issue_id
  <> ":"
  <> report.issue_identifier
  <> ":"
  <> report.reason
  <> ":"
  <> option_string(report.release_policy)
  <> ":"
  <> option_string(report.run_id)
}

fn option_string(value: Option(String)) -> String {
  case value {
    None -> ""
    Some(value) -> value
  }
}

fn blocking_handoff(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> handoff.Client {
  handoff.Client(
    claim_issue: fn(_, _) {
      process.send(log_subject, "claim_started")
      test_async.block_until_released(barrier)
      Ok(Nil)
    },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
    report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_park: fn(_) { Ok(Nil) },
  )
}

fn long_running_agent(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.id)
    test_async.block_until_released(barrier)
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("stopped")),
      workspace_path: None,
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn token_reporting_blocking_agent(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, emit_update, _, ready) {
    ready()
    let tokens =
      session_tokens.TokenTotals(
        input: 2,
        output: 3,
        cache_read: 1,
        cache_write: 3,
        total: 9,
      )
    emit_update(
      issue.id,
      agent_types.RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate(
        name: turn_telemetry.EventStarted,
        turn: 1,
        tokens: session_tokens.zero_token_totals(),
        reason: None,
      )),
    )
    emit_update(
      issue.id,
      agent_types.RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate(
        name: turn_telemetry.EventFinished,
        turn: 1,
        tokens: tokens,
        reason: None,
      )),
    )
    process.send(log_subject, "agent_tokens_emitted")
    test_async.block_until_released(barrier)
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("stopped")),
      workspace_path: None,
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn failing_agent(
  log_subject: process.Subject(String),
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.id)
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("boom")),
      workspace_path: Some("test/tmp/failed-workspace"),
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn active_success_agent(
  log_subject: process.Subject(String),
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.id)
    Ok(agent_types.WorkerSuccess(
      final_issue: Some(issue),
      final_classification: agent_types.FinalActive,
      workspace_path: "test/tmp/active-workspace",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.from_final_response(Some("active"), False, "test"),
    ))
  }
}

fn prompt_logging_agent(
  log_subject: process.Subject(String),
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, prompt, _, _, _, _, _) {
    process.send(log_subject, "agent_prompt:" <> prompt)
    Ok(agent_types.WorkerSuccess(
      final_issue: Some(issue),
      final_classification: agent_types.FinalActive,
      workspace_path: "test/tmp/reload-snapshot-workspace",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.from_final_response(
        Some("reload snapshot"),
        False,
        "test",
      ),
    ))
  }
}

fn fail_original_then_block_agent(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.title)
    case issue.title == "Changed title" {
      True -> test_async.block_until_released(barrier)
      False -> Nil
    }
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("boom")),
      workspace_path: Some("test/tmp/failed-workspace"),
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn control_log_value(event: String, fields: List(#(String, String))) -> String {
  case event == "control_server_started" {
    True -> find_field(fields, "control_file")
    False -> event
  }
}

fn find_field(fields: List(#(String, String)), key: String) -> String {
  case fields {
    [] -> ""
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> value
        False -> find_field(rest, key)
      }
  }
}

pub fn daemon_writes_control_file_and_serves_session_list_test() {
  let #(workflow_path, _root) = write_workflow("test/tmp/daemon-control-basic")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)
  assert control.host == "127.0.0.1"
  assert control.port > 0
  assert control.token != ""
  let assert Ok([]) = client.list_sessions(control)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_control_server_uses_extended_command_timeout_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-timeout")
  let log_subject = process.new_subject()
  let settings_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_control_server: fn(
        settings: control_server.Settings,
        _backend: control_server.Backend,
      ) {
        process.send(settings_subject, settings.command_timeout_ms)
        Ok(daemon.NoControlServer)
      },
      stop_control_server: fn(_) { Nil },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(command_timeout_ms) =
    process.receive(settings_subject, within: 1000)
  assert command_timeout_ms == control_server.default_command_timeout_ms
  assert command_timeout_ms == 60_000

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_control_server_uses_configured_command_timeout_test() {
  let #(workflow_path, _root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-timeout-override",
      0,
      1,
      "control:\n  command_timeout: 2s\n",
    )
  let log_subject = process.new_subject()
  let settings_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_control_server: fn(
        settings: control_server.Settings,
        _backend: control_server.Backend,
      ) {
        process.send(settings_subject, settings.command_timeout_ms)
        Ok(daemon.NoControlServer)
      },
      stop_control_server: fn(_) { Nil },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(command_timeout_ms) =
    process.receive(settings_subject, within: 1000)
  assert command_timeout_ms == 2000

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_stops_when_control_server_accept_loop_dies_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-accept-loop-down")
  let log_subject = process.new_subject()
  let server_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_control_server: fn(settings, backend) {
        case control_server.start(settings, backend) {
          Ok(server_handle) -> {
            process.send(server_subject, server_handle)
            Ok(daemon.RealControlServer(server_handle))
          }
          Error(control_server.ServerStartFailed(message)) ->
            Error(daemon.StartupError("control_server_start_failed", message))
        }
      },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.unlink(started.pid)
  let daemon_monitor = process.monitor(started.pid)
  let assert Ok(server_handle) = process.receive(server_subject, within: 1000)
  let assert Ok(control_file_path) = process.receive(log_subject, within: 1000)
  let assert Ok(control_file_present) = simplifile.is_file(control_file_path)
  assert control_file_present

  control_server.stop(server_handle)

  assert wait_for_log(log_subject, "control_server_down", 20)
  let daemon_stopped = wait_for_monitor_down(daemon_monitor, 1000)
  let control_file_removed = simplifile.is_file(control_file_path) != Ok(True)
  process.demonitor_process(daemon_monitor)
  case daemon_stopped {
    True -> Nil
    False -> {
      let _ = daemon.shutdown(started.data, 1000)
      Nil
    }
  }
  case control_file_removed {
    True -> Nil
    False -> control_file.remove(control_file_path)
  }
  assert daemon_stopped
  assert control_file_removed
}

pub fn daemon_metrics_query_reports_runtime_counts_test() {
  let candidate = issue("metrics-issue", "ABC-METRICS", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-metrics", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  let assert Ok(paused) =
    daemon.apply_operator_command(started.data, command.PauseDispatch, 1000)
  assert command.status_to_string(paused.status) == "applied"
  let assert Ok(metrics) =
    wait_for_metrics(started.data, 20, fn(metrics) {
      metrics.dispatch_paused
      && metrics.running_workers == 1
      && metrics.active_sessions == 2
    })
  let assert Ok(read_snapshot) =
    daemon.get_read_model_snapshot(started.data, 1000)
  let read_model.Snapshot(
    dispatch_paused: read_dispatch_paused,
    remote_client_status: remote_client_status,
    counts: read_model.RuntimeCounts(
      workflow_count: workflow_count,
      active_sessions: active_sessions,
      running_workers: running_workers,
      ..,
    ),
    ..,
  ) = read_snapshot
  assert read_dispatch_paused
  assert remote_client_status == read_model.Disabled
  assert workflow_count == 1
  assert active_sessions == 2
  assert running_workers == 1
  assert metrics.schema_version
    == query_types.operational_metrics_schema_version
  assert metrics.daemon_id != ""
  assert metrics.boot_id != ""
  assert metrics.sampled_at_ms == 42
  assert metrics.dispatch_paused
  assert metrics.workflow_count == 1
  assert metrics.scheduled_job_count == 0
  assert metrics.active_sessions == 2
  assert metrics.running_workers == 1
  assert metrics.running_scheduled_workers == 0
  assert metrics.queued_claims == 0
  assert !metrics.lifecycle_projection_failed
  assert metrics.token_totals.total == 0

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_metrics_count_active_yaml_child_steps_and_child_tokens_test() {
  let candidate = issue("yaml-metrics-issue", "ABC-YAML", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-yaml-metrics", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      token_reporting_blocking_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_tokens_emitted", 20)

  let assert Ok(query_types.MetricsResponse(active_metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)
  assert active_metrics.running_workers == 1
  assert active_metrics.running_scheduled_workers == 0
  assert active_metrics.active_sessions == 2
  assert active_metrics.token_totals.total == 9

  let assert Ok(parent_summary) =
    wait_for_session(hub_subject, "ABC-YAML-42-1", 20)
  assert parent_summary.current_turn == 1
  assert parent_summary.current_turn_status
    == Some(turn_telemetry.StatusFinished)
  assert parent_summary.last_turn_token_delta.total == 9
  assert parent_summary.token_totals.total == 9

  test_async.release_barrier(worker_barrier)
  assert wait_for_log(log_subject, "worker_exited", 20)
  let assert Ok(final_metrics) =
    wait_for_metrics(started.data, 20, fn(metrics) {
      metrics.active_sessions == 0 && metrics.token_totals.total == 9
    })
  assert final_metrics.active_sessions == 0
  assert final_metrics.token_totals.total == 9

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_status_and_metrics_queries_do_not_call_tracker_adapter_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-query-probe")
  let log_subject = process.new_subject()
  let tracker_probe = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      make_tracker_adapter: fn(_) { probed_tracker_adapter(tracker_probe) },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(query_types.StatusResponse(status)) =
    daemon.execute_query(started.data, query_types.Status, 1000)
  let assert Ok(query_types.MetricsResponse(metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)

  assert status.supported_queries == query_types.supported_queries()
  assert metrics.remote_client_status == "disabled"
  test_async.assert_no_extra_message_within(tracker_probe, 50)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_outbox_queries_use_recovered_outbox_snapshot_test() {
  let dir = "test/tmp/daemon-control-outbox-query"
  let #(workflow_path, root) = write_workflow(dir)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          1,
          1000,
          record.OutboxPermanentlyFailedWithTask(
            "outbox-daemon",
            record.linear_task_ref_fields(
              "issue-daemon",
              Some("LIV-1087"),
              Some("https://linear.example/LIV-1087"),
            ),
            "linear_comment",
            "invalid_payload",
            3,
          ),
        ),
      ],
      True,
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
      stop_control_server: fn(_) { Nil },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(query_types.OutboxListResponse(page)) =
    daemon.execute_query(
      started.data,
      query_types.OutboxList(query_types.OutboxListQuery(
        statuses: [query_types.OutboxPermanentStatus],
        kinds: ["linear_comment"],
        limit: 10,
        cursor: None,
      )),
      1000,
    )
  let assert [item] = page.items
  assert item.outbox_id == "outbox-daemon"
  assert item.task_ref.display_id == Some("LIV-1087")
  assert item.has_payload == False
  assert item.last_error_code == Some("invalid_payload")

  let assert Ok(query_types.OutboxShowResponse(shown)) =
    daemon.execute_query(
      started.data,
      query_types.OutboxShow(query_types.OutboxShowQuery(
        outbox_id: "outbox-daemon",
      )),
      1000,
    )
  assert shown.outbox_id == "outbox-daemon"
  assert shown.dedupe_key == None
  assert shown.attempt_count == Some(3)
  let encoded = shown |> dto.outbox_record_to_json |> json.to_string
  assert !string.contains(encoded, "raw-secret")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_status_and_metrics_queries_stay_bounded_with_large_retained_history_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-large-history")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  populate_retained_session_history(hub_subject, 80, 50)
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_event_hub: fn() { Ok(hub_subject) },
      start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
      stop_control_server: fn(_) { Nil },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(query_types.StatusResponse(status)) =
    daemon.execute_query(started.data, query_types.Status, 1000)
  let assert Ok(query_types.MetricsResponse(metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)
  let encoded_status = status |> dto.status_to_json |> json.to_string
  let encoded_metrics =
    metrics |> dto.operational_metrics_to_json |> json.to_string

  assert metrics.active_sessions == 0
  assert metrics.running_workers == 0
  assert string.length(encoded_status) < 300
  assert string.length(encoded_metrics) < 1200
  assert !string.contains(encoded_status, large_history_marker())
  assert !string.contains(encoded_metrics, large_history_marker())

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_read_model_reports_remote_client_retrying_when_start_fails_test() {
  let #(workflow_path, _root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-remote-client-retrying",
      0,
      1,
      "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  credential_ref: work-laptop\n",
    )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(log_subject),
      start_event_hub: fn() { Ok(hub_subject) },
      start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
      stop_control_server: fn(_) { Nil },
      start_remote_client: fn(_, _, _, _, _) {
        Error(daemon.StartupError("dial_failed", "boom"))
      },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  assert wait_for_log(log_subject, "remote_client_restart_failed", 20)
  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.remote_client_status == read_model.Retrying("dial_failed")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_control_server_routes_authenticated_command_to_actor_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-command")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  let assert Ok(result) = client.apply_command(control, command.PauseDispatch)
  assert result.command == "pause"
  assert command.status_to_string(result.status) == "applied"

  let assert Ok(cleanup_result) =
    client.apply_command(
      control,
      command.CleanupOrphanSteps("run-missing", True),
    )
  assert cleanup_result.command == "cleanup_orphan_steps"
  assert command.status_to_string(cleanup_result.status) == "not_found"
  assert cleanup_result.message == Some("workflow run not found: run-missing")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_park_and_unpark_commands_mutate_runtime_state_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let tracker_client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let #(workflow_path, _root) = write_workflow("test/tmp/daemon-control-park")
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies_with_tracker(log_subject, tracker_client),
      make_tracker_adapter: fn(_) {
        adapter.TrackerAdapter(
          ..adapter_legacy.adapter_from_legacy_client(tracker_client, "linear"),
          handoff: Some(
            test_handoff_capability(park_reporting_handoff(park_subject)),
          ),
        )
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  let assert Ok(parked) =
    client.apply_command(
      control,
      command.ParkIssue(command.IssueIdentifier("ABC-1"), "manual"),
    )
  assert command.status_to_string(parked.status) == "applied"
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot_after_park) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(read_snapshot_after_park) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert dict.has_key(snapshot_after_park.parked, identity)
  assert read_snapshot_after_park.counts.parked_tasks == 1
  let assert Ok(parked_entry) = dict.get(snapshot_after_park.parked, identity)
  assert parked_entry.release_policy == orchestrator_state.ExplicitUnparkOnly
  assert process.receive(park_subject, within: 1000)
    == Ok("park:issue-1:ABC-1:manual:explicit_unpark_only:")

  let assert Ok(unparked) =
    client.apply_command(
      control,
      command.UnparkIssue(command.IssueIdentifier("ABC-1")),
    )
  assert command.status_to_string(unparked.status) == "applied"
  let assert Ok(snapshot_after_unpark) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(read_snapshot_after_unpark) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot_after_unpark.parked, identity)
  assert read_snapshot_after_unpark.counts.parked_tasks == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn pause_command_suppresses_dispatch_and_resume_allows_it_test() {
  let candidate = issue("pause-issue", "ABC-PAUSE", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-pause", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(paused) =
    daemon.apply_operator_command(started.data, command.PauseDispatch, 1000)
  assert command.status_to_string(paused.status) == "applied"
  let _ = test_async.drain_subject(log_subject)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "tick_started", 10)
  let assert Ok(paused_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(paused_snapshot.running) == 0
  let paused_logs = test_async.drain_subject(log_subject)
  assert !list.contains(paused_logs, "dispatch_started")

  let assert Ok(resumed) =
    daemon.apply_operator_command(started.data, command.ResumeDispatch, 1000)
  assert command.status_to_string(resumed.status) == "applied"
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn startup_recovery_of_dispatch_pause_suppresses_dispatch_until_resume_test() {
  let candidate = issue("recovered-pause", "ABC-RECOVER-PAUSE", "Todo")
  let tracker_client = tracker_with(candidate)
  let dir = "test/tmp/daemon-control-pause-recovery"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 3)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [record.new(1, 1, record.DispatchPauseChanged(True))],
      True,
    )
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(recovered_read_snapshot) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert recovered_read_snapshot.dispatch_paused

  let _ = test_async.drain_subject(log_subject)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "tick_started", 10)
  let assert Ok(paused_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(paused_snapshot.running) == 0
  let paused_logs = test_async.drain_subject(log_subject)
  assert !list.contains(paused_logs, "dispatch_started")

  let assert Ok(resumed) =
    daemon.apply_operator_command(started.data, command.ResumeDispatch, 1000)
  assert command.status_to_string(resumed.status) == "applied"
  let assert Ok(resumed_read_snapshot) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert !resumed_read_snapshot.dispatch_paused
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_command_rejects_paused_and_dispatches_eligible_issue_test() {
  let candidate = issue("retry-issue", "ABC-RETRY", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(_) =
    daemon.apply_operator_command(started.data, command.PauseDispatch, 1000)
  let assert Ok(paused_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("retry-issue")),
      1000,
    )
  assert command.status_to_string(paused_retry.status) == "rejected"

  let assert Ok(_) =
    daemon.apply_operator_command(started.data, command.ResumeDispatch, 1000)
  let assert Ok(dispatched_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("retry-issue")),
      1000,
    )
  assert command.status_to_string(dispatched_retry.status) == "applied"
  assert wait_for_log(log_subject, "dispatch_started", 20)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_command_acknowledges_before_accepted_side_effects_finish_test() {
  let candidate = issue("retry-async-issue", "ABC-ASYNC-RETRY", "Todo")
  let tracker_client = tracker_with(candidate)
  let dir = "test/tmp/daemon-control-retry-async-ack"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 3)
  let ledger_root = dir <> "/" <> root
  let log_subject = process.new_subject()
  let operator_log_barrier = test_async.new_barrier()
  let claim_barrier = test_async.new_barrier()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let base_deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      blocking_handoff(log_subject, claim_barrier),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let deps =
    daemon.RuntimeDependencies(..base_deps, logger: fn(_, event, fields, _) {
      process.send(log_subject, control_log_value(event, fields))
      case event == "operator_command" {
        True -> {
          process.send(log_subject, "operator_log_started")
          test_async.block_until_released(operator_log_barrier)
        }
        False -> Nil
      }
      Ok(Nil)
    })
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(accepted_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("retry-async-issue")),
      50,
    )
  assert command.status_to_string(accepted_retry.status) == "applied"
  assert accepted_retry.message == Some("retry accepted")

  assert wait_for_log(log_subject, "operator_log_started", 20)
  test_async.release_barrier(operator_log_barrier)
  assert wait_for_log(log_subject, "claim_started", 20)
  test_async.release_barrier(claim_barrier)
  assert wait_for_log(log_subject, "dispatch_started", 20)
  assert has_workflow_started(
    ledger_bodies(ledger_root),
    "implementation",
    "implementation/ABC-ASYNC-RETRY",
  )

  test_async.release_barrier(worker_barrier)
  assert wait_for_log(log_subject, "worker_exited", 20)
  let assert Ok(failed_session) =
    wait_for_session_exit(hub_subject, "ABC-ASYNC-RETRY-42-1", 20)
  assert failed_session.status == event.Exited(session_reason.Failed)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn workflow_reload_changed_route_preserves_pending_claim_snapshot_test() {
  let candidate = issue("reload-changed", "ABC-RELOAD", "Todo")
  let tracker_client = tracker_with(candidate)
  let dir = "test/tmp/daemon-control-reload-changed-route"
  let #(workflow_path, root) =
    write_reload_snapshot_workflow(dir, Some("implementation"))
  let ledger_root = dir <> "/" <> root
  let log_subject = process.new_subject()
  let claim_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      blocking_handoff(log_subject, claim_barrier),
      hub_subject,
      prompt_logging_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "claim_started", 20)

  overwrite_reload_snapshot_config(workflow_path, root, Some("review"))
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "workflow_reloaded", 20)

  test_async.release_barrier(claim_barrier)
  assert wait_for_log(log_subject, "agent_prompt:Implementation Prompt", 20)
  assert has_workflow_started(
    ledger_bodies(ledger_root),
    "implementation",
    "implementation/ABC-RELOAD",
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn workflow_reload_removed_route_preserves_pending_claim_snapshot_test() {
  let candidate = issue("reload-removed", "ABC-REMOVED", "Todo")
  let tracker_client = tracker_with(candidate)
  let dir = "test/tmp/daemon-control-reload-removed-route"
  let #(workflow_path, root) =
    write_reload_snapshot_workflow(dir, Some("implementation"))
  let ledger_root = dir <> "/" <> root
  let log_subject = process.new_subject()
  let claim_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      blocking_handoff(log_subject, claim_barrier),
      hub_subject,
      prompt_logging_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "claim_started", 20)

  overwrite_reload_snapshot_config(workflow_path, root, None)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "workflow_reloaded", 20)

  test_async.release_barrier(claim_barrier)
  assert wait_for_log(log_subject, "agent_prompt:Implementation Prompt", 20)
  assert has_workflow_started(
    ledger_bodies(ledger_root),
    "implementation",
    "implementation/ABC-REMOVED",
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_rejects_active_pending_and_accepts_inactive_issues_test() {
  let active = issue("active-issue", "ABC-ACTIVE", "Todo")
  let tracker_client = tracker_with(active)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-active", 1, 3)
  let log_subject = process.new_subject()
  let active_worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, active_worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  let assert Ok(active_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("active-issue")),
      1000,
    )
  assert command.status_to_string(active_retry.status) == "rejected"
  test_async.release_barrier(active_worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)

  let pending = issue("pending-issue", "ABC-PENDING", "Todo")
  let tracker_client = tracker_with(pending)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-pending", 1, 3)
  let log_subject = process.new_subject()
  let claim_barrier = test_async.new_barrier()
  let pending_worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      blocking_handoff(log_subject, claim_barrier),
      hub_subject,
      long_running_agent(log_subject, pending_worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "claim_started", 20)

  let assert Ok(pending_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("pending-issue")),
      1000,
    )
  assert command.status_to_string(pending_retry.status) == "rejected"
  test_async.release_barrier(claim_barrier)
  test_async.release_barrier(pending_worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)

  let claimed = issue("claimed-issue", "ABC-CLAIMED", "Todo")
  let tracker_client = tracker_with(claimed)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-claimed", 1, 3)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      active_success_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_exited", 20)
  let claimed_identity = orchestrator_state.issue_identity(claimed)
  let assert Ok(claimed_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(claimed_snapshot.claimed, claimed_identity)
  assert !dict.has_key(claimed_snapshot.running, claimed_identity)
  assert !dict.has_key(claimed_snapshot.retry_attempts, claimed_identity)

  let assert Ok(claimed_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("claimed-issue")),
      1000,
    )
  assert command.status_to_string(claimed_retry.status) == "applied"
  let assert Ok(retried_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(retried_snapshot.completed, claimed_identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn park_inactive_issue_without_retry_queue_test() {
  let candidate = issue("park-claimed", "ABC-PARK-CLAIMED", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-park-claimed", 1, 3)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      active_success_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_exited", 20)

  let assert Ok(parked) =
    daemon.apply_operator_command(
      started.data,
      command.ParkIssue(command.IssueId("park-claimed"), "manual"),
      1000,
    )
  assert command.status_to_string(parked.status) == "applied"
  let claimed_identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(read_snapshot) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.claimed, claimed_identity)
  assert dict.has_key(snapshot.parked, claimed_identity)
  assert read_snapshot.counts.retry_tasks == 0
  assert read_snapshot.counts.claimed_tasks == 0
  assert read_snapshot.counts.parked_tasks == 1

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_candidate_dispatch_clears_stale_auto_park_test() {
  let candidate = issue("auto-park", "ABC-AUTO", "Todo")
  let changed = tracker_issue.Issue(..candidate, title: "Changed title")
  let tracker_server = start_control_tracker_server(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-auto-park", 1, 3)
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      dynamic_control_tracker(tracker_server),
      park_reporting_handoff(park_subject),
      hub_subject,
      fail_original_then_block_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:Title ABC-AUTO", 20)
  assert wait_for_log(log_subject, "issue_parked", 20)
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(parked_entry) = dict.get(parked_snapshot.parked, identity)
  assert parked_entry.release_policy
    == orchestrator_state.AutoUnparkOnIssueChange(core.issue_fingerprint(
      candidate,
    ))
  assert process.receive(park_subject, within: 1000)
    == Ok(
      "park:auto-park:ABC-AUTO:worker_failure:auto_unpark_on_issue_change:ABC-AUTO-42-1",
    )
  let _ = test_async.drain_subject(log_subject)

  process.send(tracker_server, SetControlTrackerCandidate(changed))
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "agent_run:Changed title", 20)
  let assert Ok(running_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(running_snapshot.running, identity)
  assert !dict.has_key(running_snapshot.parked, identity)
  assert !dict.has_key(running_snapshot.retry_attempts, identity)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn startup_recovery_of_parked_issue_does_not_repost_park_comment_test() {
  let candidate = issue("recovered-park", "ABC-RECOVER", "Todo")
  let dir = "test/tmp/daemon-control-park-recovery"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 3)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          1,
          1,
          record.IssueParkedV2(
            "recovered-park",
            "ABC-RECOVER",
            "operator_hold",
            "explicit_unpark_only",
            "",
            1,
          ),
        ),
      ],
      True,
    )
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with(candidate),
      park_reporting_handoff(park_subject),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(read_snapshot) =
    daemon.get_read_model_snapshot(started.data, 1000)
  assert dict.has_key(
    snapshot.parked,
    orchestrator_state.linear_issue_id_identity("recovered-park"),
  )
  assert read_snapshot.counts.parked_tasks == 1
  assert process.receive(park_subject, within: 100) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn startup_recovery_new_park_posts_park_comment_test() {
  let candidate = issue("recovery-new-park", "ABC-NEWREC", "Todo")
  let dir = "test/tmp/daemon-control-park-recovery-new"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 3)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          1,
          1,
          record.RunStarted(
            "ABC-NEWREC-42-1",
            "recovery-new-park",
            "ABC-NEWREC",
            "test/tmp/recovery-new-park-workspace",
          ),
        ),
      ],
      True,
    )
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with(candidate),
      park_reporting_handoff(park_subject),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(
    snapshot.parked,
    orchestrator_state.linear_issue_id_identity("recovery-new-park"),
  )
  assert process.receive(park_subject, within: 1000)
    == Ok(
      "park:recovery-new-park:ABC-NEWREC:worker_failure:auto_unpark_on_issue_change:ABC-NEWREC-42-1",
    )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn reload_workflow_updates_runtime_limits_before_reply_test() {
  let #(workflow_path, root) =
    write_workflow_with_limits("test/tmp/daemon-control-reload-runtime", 0, 1)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(10, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      empty_tracker(),
      disabled_handoff(),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(initial_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert initial_snapshot.max_concurrent_agents == 0

  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      workflow_text_with_extra_config(root, 2, 1, ""),
    )
  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"

  let assert Ok(reloaded_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert reloaded_snapshot.max_concurrent_agents == 2

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn reload_workflow_starts_remote_client_after_enabling_ui_server_test() {
  let #(workflow_path, root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-reload-ui-enable",
      0,
      1,
      ui_server_config_text(False),
    )
  let log_subject = process.new_subject()
  let starts = process.new_subject()
  let stops = process.new_subject()
  let assert Ok(started) =
    daemon.start(
      Some(workflow_path),
      remote_client_dependencies(log_subject, starts, stops),
    )

  overwrite_workflow_config(workflow_path, root, True)
  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"
  assert test_async.expect_message(starts) != ""
  test_async.assert_no_extra_message(stops)

  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.ui_server_enabled
  assert snapshot.remote_client_status == read_model.Connected
  let assert Ok(query_types.MetricsResponse(metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)
  assert metrics.ui_server_enabled
  assert metrics.remote_client_status == "connected"

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn reload_workflow_stops_remote_client_after_disabling_ui_server_test() {
  let #(workflow_path, root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-reload-ui-disable",
      0,
      1,
      ui_server_config_text(True),
    )
  let log_subject = process.new_subject()
  let starts = process.new_subject()
  let stops = process.new_subject()
  let assert Ok(started) =
    daemon.start(
      Some(workflow_path),
      remote_client_dependencies(log_subject, starts, stops),
    )
  let _ = test_async.expect_message(starts)

  overwrite_workflow_config(workflow_path, root, False)
  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"
  assert test_async.expect_message(stops) == "stop"

  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert !snapshot.ui_server_enabled
  assert snapshot.remote_client_status == read_model.Disabled
  let assert Ok(query_types.MetricsResponse(metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)
  assert !metrics.ui_server_enabled
  assert metrics.remote_client_status == "disabled"

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn reload_workflow_restarts_remote_client_when_ui_server_is_still_enabled_test() {
  let #(workflow_path, root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-reload-ui-restart",
      0,
      1,
      ui_server_config_text(True),
    )
  let log_subject = process.new_subject()
  let starts = process.new_subject()
  let stops = process.new_subject()
  let assert Ok(started) =
    daemon.start(
      Some(workflow_path),
      remote_client_dependencies(log_subject, starts, stops),
    )
  let _ = test_async.expect_message(starts)

  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"
  assert test_async.expect_message(stops) == "stop"
  assert test_async.expect_message(starts)
    == expected_resolved_workspace_root(workflow_path, root)

  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.ui_server_enabled
  assert snapshot.remote_client_status == read_model.Connected

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn poll_tick_restarts_remote_client_after_changed_config_reload_test() {
  let #(workflow_path, root) =
    write_workflow_with_extra_config(
      "test/tmp/daemon-control-reload-ui-poll",
      0,
      1,
      ui_server_config_text(True),
    )
  let log_subject = process.new_subject()
  let starts = process.new_subject()
  let stops = process.new_subject()
  let assert Ok(started) =
    daemon.start(
      Some(workflow_path),
      remote_client_dependencies(log_subject, starts, stops),
    )
  assert test_async.expect_message(starts)
    == expected_resolved_workspace_root(workflow_path, root)

  let reloaded_root = root <> "-after-reload"
  overwrite_workflow_config(workflow_path, reloaded_root, True)
  process.send(started.data, daemon.PollTick(1))

  assert test_async.expect_message(stops) == "stop"
  assert test_async.expect_message(starts)
    == expected_resolved_workspace_root(workflow_path, reloaded_root)
  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.ui_server_enabled
  assert snapshot.remote_client_status == read_model.Connected

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn reload_workflow_command_reports_success_and_missing_file_failure_test() {
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-reload", 0, 1)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(10, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      empty_tracker(),
      disabled_handoff(),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"

  let assert Ok(Nil) = simplifile.delete(workflow_path)
  let assert Ok(missing) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(missing.status) == "rejected"

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn abort_command_falls_back_to_kill_and_park_when_worker_does_not_reply_test() {
  assert_session_stop_command(
    "test/tmp/daemon-control-abort",
    issue("abort-issue", "ABC-ABORT", "Todo"),
    command.AbortSession("ABC-ABORT-42-1"),
    session_reason.OperatorAbort,
  )
}

pub fn abort_command_timeout_fallback_does_not_block_daemon_test() {
  let candidate = issue("abort-timeout-issue", "ABC-ABORT-TIMEOUT", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-abort-timeout", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      fn(issue: tracker_issue.Issue, _, _, _, _, _, command_subject, ready) {
        ready()
        process.send(log_subject, "agent_run:" <> issue.id)
        let assert Ok(worker_command.Abort(_)) =
          process.receive(command_subject, within: 1000)
        process.send(log_subject, "abort_received:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:abort-timeout-issue", 20)

  let operator_reply =
    daemon.apply_operator_command_async(
      started.data,
      command.AbortSession("ABC-ABORT-TIMEOUT-42-1"),
      250,
    )
  assert wait_for_log(log_subject, "abort_received:abort-timeout-issue", 20)
  let assert Ok(snapshot_while_pending) = daemon.get_snapshot(started.data, 100)
  assert dict.has_key(
    snapshot_while_pending.running,
    orchestrator_state.issue_identity(candidate),
  )

  let assert Ok(result) = process.receive(operator_reply, within: 1000)
  assert command.status_to_string(result.status) == "applied"
  assert result.message == Some("operator_abort")

  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let identity = orchestrator_state.issue_identity(candidate)
  assert !dict.has_key(snapshot.running, identity)
  assert dict.has_key(snapshot.parked, identity)

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn prompt_command_reaches_live_worker_command_subject_test() {
  let candidate = issue("prompt-live-issue", "ABC-LIVE", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-live-prompt", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      fn(issue: tracker_issue.Issue, _, _, _, _, _, command_subject, ready) {
        ready()
        process.send(log_subject, "agent_run:" <> issue.id)
        let assert Ok(worker_command.QueuePrompt(message, reply)) =
          process.receive(command_subject, within: 1000)
        assert message == "status?"
        process.send(reply, worker_command.Applied(Some("prompt accepted")))
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:prompt-live-issue", 20)

  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-LIVE-42-1", "status?"),
      1000,
    )
  assert command.status_to_string(prompt_result.status) == "applied"
  assert prompt_result.target == Some("ABC-LIVE-42-1")
  assert prompt_result.message == Some("prompt accepted")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn prompt_command_reports_worker_timeout_without_blocking_daemon_test() {
  let candidate = issue("prompt-timeout-issue", "ABC-PROMPT-TIMEOUT", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-prompt-timeout", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      fn(issue: tracker_issue.Issue, _, _, _, _, _, command_subject, ready) {
        ready()
        process.send(log_subject, "agent_run:" <> issue.id)
        let assert Ok(worker_command.QueuePrompt(message, _)) =
          process.receive(command_subject, within: 1000)
        assert message == "status?"
        process.send(log_subject, "prompt_received:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:prompt-timeout-issue", 20)

  let operator_reply =
    daemon.apply_operator_command_async(
      started.data,
      command.PromptSession("ABC-PROMPT-TIMEOUT-42-1", "status?"),
      250,
    )
  assert wait_for_log(log_subject, "prompt_received:prompt-timeout-issue", 20)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 100)
  assert dict.has_key(
    snapshot.running,
    orchestrator_state.issue_identity(candidate),
  )

  let assert Ok(prompt_result) = process.receive(operator_reply, within: 1000)
  assert prompt_result.command == "prompt"
  assert prompt_result.target == Some("ABC-PROMPT-TIMEOUT-42-1")
  assert prompt_result.status == command.Rejected("worker_command_timeout")
  assert prompt_result.message == Some("worker command timed out")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn prompt_and_respond_ui_commands_reject_workers_without_command_subject_test() {
  let candidate = issue("prompt-issue", "ABC-PROMPT", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-prompt", 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)
  assert wait_for_log(log_subject, "agent_run:prompt-issue", 20)

  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-PROMPT-42-1", "status?"),
      1000,
    )
  assert command.status_to_string(prompt_result.status) == "not_allowed"
  assert command.status_reason(prompt_result.status)
    == Some("worker_command_subject_unavailable")

  let assert Ok(ui_result) =
    daemon.apply_operator_command(
      started.data,
      command.RespondUi("ABC-PROMPT-42-1", "ui-1", command.UiCancel),
      1000,
    )
  assert command.status_to_string(ui_result.status) == "not_allowed"
  assert command.status_reason(ui_result.status)
    == Some("worker_command_subject_unavailable")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_shutdown_closes_control_server_and_removes_control_file_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-shutdown")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  let assert Error(client.ConnectionFailed(_)) = client.ping(control)
  assert simplifile.is_file(path) != Ok(True)
}

fn assert_session_stop_command(
  dir: String,
  candidate: tracker_issue.Issue,
  operator_command: command.OperatorCommand,
  reason: session_reason.WorkerExitReason,
) -> Nil {
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) = write_workflow_with_limits(dir, 1, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(started.data, operator_command, 1000)
  assert command.status_to_string(result.status) == "applied"
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.running, identity)
  assert dict.has_key(snapshot.parked, identity)

  let assert Ok(summary) =
    wait_for_session_exit(hub_subject, candidate.identifier <> "-42-1", 20)
  assert summary.status == event.Exited(reason)

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

fn populate_retained_session_history(
  hub_subject: process.Subject(hub.Message),
  session_count: Int,
  events_per_session: Int,
) -> Nil {
  populate_retained_session_history_loop(
    hub_subject,
    session_count,
    events_per_session,
    1,
  )
}

fn populate_retained_session_history_loop(
  hub_subject: process.Subject(hub.Message),
  session_count: Int,
  events_per_session: Int,
  index: Int,
) -> Nil {
  case index > session_count {
    True -> Nil
    False -> {
      let session_id = "history-" <> int_to_string(index)
      hub.register_session(
        hub_subject,
        retained_session_summary(session_id, index),
      )
      publish_retained_events(hub_subject, session_id, events_per_session, 1)
      hub.finish_session(hub_subject, session_id, session_reason.Normal)
      populate_retained_session_history_loop(
        hub_subject,
        session_count,
        events_per_session,
        index + 1,
      )
    }
  }
}

fn publish_retained_events(
  hub_subject: process.Subject(hub.Message),
  session_id: String,
  event_count: Int,
  index: Int,
) -> Nil {
  case index > event_count {
    True -> Nil
    False -> {
      hub.publish(
        hub_subject,
        session_id,
        event.EventPayload(
          ..event.empty_payload(
            event.Lifecycle,
            event.LifecycleName(event.DispatchStarted),
          ),
          message: Some(large_history_marker() <> "-" <> int_to_string(index)),
        ),
      )
      publish_retained_events(hub_subject, session_id, event_count, index + 1)
    }
  }
}

fn retained_session_summary(
  session_id: String,
  index: Int,
) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: "History " <> int_to_string(index),
    issue_id: "issue-" <> int_to_string(index),
    issue_identifier: "ABC-HISTORY-" <> int_to_string(index),
    issue_title: large_history_marker() <> " title " <> int_to_string(index),
    workspace_path: "test/tmp/history-" <> int_to_string(index),
    pi_session_id: None,
    status: event.Exited(session_reason.Normal),
    recovery: None,
    current_turn: 0,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: index,
    last_event_at_ms: index,
    token_totals: session_tokens.TokenTotals(
      input: index,
      output: index,
      cache_read: index,
      cache_write: index,
      total: index * 4,
    ),
  )
}

fn large_history_marker() -> String {
  "raw-history-marker-should-not-leak"
}

fn tracker_with(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

type ControlTrackerMessage {
  SetControlTrackerCandidate(tracker_issue.Issue)
  FetchControlTrackerCandidates(
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
  FetchControlTrackerByIds(
    List(String),
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
}

fn start_control_tracker_server(
  candidate: tracker_issue.Issue,
) -> process.Subject(ControlTrackerMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      control_tracker_loop(subject, candidate)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn control_tracker_loop(
  subject: process.Subject(ControlTrackerMessage),
  candidate: tracker_issue.Issue,
) -> Nil {
  case process.receive(subject, within: 10_000) {
    Ok(SetControlTrackerCandidate(candidate)) ->
      control_tracker_loop(subject, candidate)
    Ok(FetchControlTrackerCandidates(reply)) -> {
      process.send(reply, Ok([candidate]))
      control_tracker_loop(subject, candidate)
    }
    Ok(FetchControlTrackerByIds(ids, reply)) -> {
      case list.contains(ids, candidate.id) {
        True -> process.send(reply, Ok([candidate]))
        False -> process.send(reply, Ok([]))
      }
      control_tracker_loop(subject, candidate)
    }
    Error(_) -> Nil
  }
}

fn dynamic_control_tracker(
  server: process.Subject(ControlTrackerMessage),
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() {
      let reply = process.new_subject()
      process.send(server, FetchControlTrackerCandidates(reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(ids) {
      let reply = process.new_subject()
      process.send(server, FetchControlTrackerByIds(ids, reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn probed_tracker_adapter(
  probe: process.Subject(String),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: "probe",
    display_name: "Probe tracker",
    task_source: adapter.TaskSourceCapability(
      fetch_candidates: fn(_) {
        process.send(probe, "fetch_candidates")
        Ok([])
      },
      refresh_by_refs: fn(_) {
        process.send(probe, "refresh_by_refs")
        Ok([])
      },
      lookup_by_operator_ref: fn(_) {
        process.send(probe, "lookup_by_operator_ref")
        Ok(None)
      },
      list_tasks: fn(_) {
        process.send(probe, "list_tasks")
        Ok(adapter.TaskPage(items: [], has_more: False))
      },
      lookup_task_detail: fn(_) {
        process.send(probe, "lookup_task_detail")
        Ok(None)
      },
    ),
    comments: None,
    remote_commands: None,
    state_transitions: None,
    routing_metadata: Some(
      adapter.RoutingMetadataCapability(
        workflow_labels: fn(value) { task.label_names(value) },
        blocker_refs: fn(value) { value.blockers },
      ),
    ),
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}

fn ledger_bodies(root: String) -> List(record.RecordBody) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  list.map(read.records, fn(ledger_record) { ledger_record.body })
}

fn has_workflow_started(
  bodies: List(record.RecordBody),
  workflow_id: String,
  run_root_fragment: String,
) -> Bool {
  list.any(bodies, fn(body) {
    case body {
      record.WorkflowRunStartedWithTask(
        _,
        started_workflow_id,
        _,
        _,
        _,
        _,
        _,
        _,
        run_root,
      ) ->
        started_workflow_id == workflow_id
        && string.contains(run_root, run_root_fragment)
      _ -> False
    }
  })
}

fn wait_for_monitor_down(monitor: process.Monitor, timeout_ms: Int) -> Bool {
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { True })

  case process.selector_receive(selector, within: timeout_ms) {
    Ok(True) -> True
    Ok(False) -> False
    Error(_) -> False
  }
}

fn wait_for_log(
  subject: process.Subject(String),
  expected: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 100) {
        Ok(actual) ->
          case actual == expected {
            True -> True
            False -> wait_for_log(subject, expected, attempts - 1)
          }
        Error(_) -> wait_for_log(subject, expected, attempts - 1)
      }
  }
}

fn wait_for_metrics(
  daemon_subject: process.Subject(daemon.Message),
  attempts: Int,
  predicate: fn(query_types.OperationalMetricsDto) -> Bool,
) -> Result(query_types.OperationalMetricsDto, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case daemon.execute_query(daemon_subject, query_types.Metrics, 1000) {
        Ok(query_types.MetricsResponse(metrics)) ->
          case predicate(metrics) {
            True -> Ok(metrics)
            False -> {
              process.sleep(50)
              wait_for_metrics(daemon_subject, attempts - 1, predicate)
            }
          }
        _ -> {
          process.sleep(50)
          wait_for_metrics(daemon_subject, attempts - 1, predicate)
        }
      }
  }
}

fn wait_for_session(
  subject: process.Subject(hub.Message),
  session_id: String,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.get_session(subject, session_id, 100) {
        Ok(Some(summary)) -> Ok(summary)
        _ -> wait_for_session(subject, session_id, attempts - 1)
      }
  }
}

fn wait_for_session_exit(
  subject: process.Subject(hub.Message),
  session_id: String,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.get_session(subject, session_id, 100) {
        Ok(Some(summary)) ->
          case summary.status {
            event.Exited(_) -> Ok(summary)
            _ -> wait_for_session_exit(subject, session_id, attempts - 1)
          }
        _ -> wait_for_session_exit(subject, session_id, attempts - 1)
      }
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
