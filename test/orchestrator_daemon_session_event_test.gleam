import birl
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/daemon
import scherzo/path
import scherzo/result_artifact
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_run
import scherzo/workspace
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

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String) -> String {
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
  sessions_per_task: 2
  retries:
    attempts: 3
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
"
}

fn write_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let root = dir <> "/workspaces"
  let assert Ok(root) = path.absolute(root)
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(config_path, workflow_text(root))
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
  #(config_path, root)
}

fn write_scheduled_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let root = dir <> "/workspaces"
  let assert Ok(root) = path.absolute(root)
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root)
        <> "schedules:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n",
    )
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
  #(config_path, root)
}

pub type TestClockMessage {
  GetNow(process.Subject(Int))
  SetNow(Int)
  StopClock
}

fn start_test_clock(initial_ms: Int) -> process.Subject(TestClockMessage) {
  let ready = process.new_subject()
  let _ =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      test_clock_loop(subject, initial_ms)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn test_clock_loop(
  subject: process.Subject(TestClockMessage),
  now_ms: Int,
) -> Nil {
  case process.receive(subject, within: 5000) {
    Ok(GetNow(reply)) -> {
      process.send(reply, now_ms)
      test_clock_loop(subject, now_ms)
    }
    Ok(SetNow(next_ms)) -> test_clock_loop(subject, next_ms)
    Ok(StopClock) -> Nil
    Error(_) -> Nil
  }
}

fn clock_now(clock: process.Subject(TestClockMessage)) -> Int {
  let reply = process.new_subject()
  process.send(clock, GetNow(reply))
  let assert Ok(now_ms) = process.receive(reply, within: 1000)
  now_ms
}

fn set_clock(clock: process.Subject(TestClockMessage), now_ms: Int) -> Nil {
  process.send(clock, SetNow(now_ms))
}

fn success(
  final: tracker_issue.Issue,
  workspace_path: String,
) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(final),
    final_classification: agent_types.FinalTerminal,
    workspace_path: workspace_path,
    tokens: session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 0,
      cache_write: 0,
      total: 3,
    ),
    turns: 1,
    result: result_artifact.from_final_response(None, False, "none"),
  )
}

fn update(name: String, message: Option(String)) -> agent_types.RunnerUpdate {
  agent_types.RunnerPiUpdate(agent_types.PiUpdate(
    event: pi_event.from_string(name),
    message: message,
    raw_json: None,
    turn: Some(1),
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}

fn client_with(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
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

fn dependencies(
  client: tracker.Client,
  log_subject: process.Subject(String),
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
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(client, "linear")
    },
    workflow_run_dependencies: workflow_deps_from_agent(agent_runner),
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, logged_event, _, _) {
      process.send(log_subject, logged_event)
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

pub fn daemon_records_session_summary_and_replay_events_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/daemon-session-events")
  let candidate = issue("issue-id", "ABC-123", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(20, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(issue.id, update("message_update", Some("hello")))
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "worker_exited", 20)
  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-123-42-1", 20)
  let assert Ok(#(_, expected_workspace)) =
    workspace.workspace_path(root, "ABC-123")
  assert summary.issue_identifier == "ABC-123"
  assert summary.display_name
    == session_name.generate("ABC-123", "ABC-123-42-1")
  assert summary.workspace_path == expected_workspace
  assert summary.status == event.Exited(reason.Normal)
  assert summary.token_totals.total == 3

  let assert Ok(step_summary) =
    wait_for_session(
      hub_subject,
      "workflow-step-ABC-123-42-1-implement-a1-f9bb818d8483",
      20,
    )
  assert step_summary.display_name
    == session_name.generate(
      "ABC-123",
      "workflow-step-ABC-123-42-1-implement-a1-f9bb818d8483",
    )
  assert step_summary.display_name != summary.display_name

  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-123-42-1", 0, 20, 1000)
  assert event_names(page.events)
    == ["dispatch_started", "worker_started", "worker_exited"]
  assert event_cursors(page.events) == [1, 2, 5]

  let assert Ok(step_page) =
    hub.events_after(
      hub_subject,
      "workflow-step-ABC-123-42-1-implement-a1-f9bb818d8483",
      0,
      20,
      1000,
    )
  assert event_names(step_page.events) == ["step_started", "message_update"]
  let assert Some(message_event) =
    find_event(step_page.events, "message_update")
  assert message_event.payload.kind == event.AssistantMessage

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_keeps_successful_pi_auto_retry_events_in_one_yaml_step_session_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-auto-retry-success")
  let candidate = issue("auto-retry-ok", "ABC-RETRYOK", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(issue.id, update("auto_retry_start", None))
        emit_update(issue.id, update("auto_retry_end", None))
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "worker_exited", 20)
  let step_session_id =
    "workflow-step-ABC-RETRYOK-42-1-implement-a1-f9bb818d8483"
  let assert Ok(step_summary) =
    wait_for_session(hub_subject, step_session_id, 20)
  assert step_summary.status == event.Exited(reason.Normal)
  let assert Ok(step_page) =
    hub.events_after(hub_subject, step_session_id, 0, 20, 1000)
  assert event_names(step_page.events)
    == ["step_started", "auto_retry_start", "auto_retry_end"]

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_records_exhausted_pi_auto_retry_events_in_failed_yaml_step_session_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/daemon-auto-retry-fail")
  let candidate = issue("auto-retry-fail", "ABC-RETRYFAIL", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(issue.id, update("auto_retry_start", None))
        emit_update(issue.id, update("auto_retry_end", None))
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError(
            "provider_transport_failure: WebSocket error",
          )),
          workspace_path: Some(expected_workspace),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "retry_scheduled", 20)
  let step_session_id =
    "workflow-step-ABC-RETRYFAIL-42-1-implement-a1-f9bb818d8483"
  let assert Ok(step_summary) =
    wait_for_session(hub_subject, step_session_id, 20)
  assert step_summary.status == event.Exited(reason.Failed)
  let assert Ok(step_page) =
    hub.events_after(hub_subject, step_session_id, 0, 20, 1000)
  assert event_names(step_page.events)
    == ["step_started", "auto_retry_start", "auto_retry_end"]

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_classifies_tool_fields_as_tool_events_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/daemon-tool-events")
  let candidate = issue("tool-id", "ABC-TOOL", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(20, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(
          issue.id,
          agent_types.RunnerPiUpdate(agent_types.PiUpdate(
            event: pi_event.Message,
            message: None,
            raw_json: None,
            turn: Some(1),
            request_id: None,
            method: None,
            pi_session_id: None,
            tokens: session_tokens.zero_token_totals(),
            tool_name: Some("bash"),
            tool_input: Some("gleam test"),
            tool_output: None,
            tool_status: None,
          )),
        )
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "worker_exited", 20)
  let assert Ok(page) =
    hub.events_after(
      hub_subject,
      "workflow-step-ABC-TOOL-42-1-implement-a1-f9bb818d8483",
      0,
      20,
      1000,
    )
  let assert Some(tool_event) = find_event(page.events, "message")
  assert tool_event.payload.kind == event.Tool
  assert tool_event.payload.tool_name == Some("bash")
  assert tool_event.payload.tool_input == Some("gleam test")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_publishes_pi_update_before_worker_exit_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-live-session-event")
  let candidate = issue("issue-id", "ABC-123", "Todo")
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let worker_update_sent = process.new_subject()
  let assert Ok(hub_subject) = hub.start(20, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(issue.id, update("message_update", Some("hello")))
        process.send(worker_update_sent, Nil)
        test_async.block_until_released(worker_barrier)
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  test_async.expect_message_within(worker_update_sent, 5000)

  let assert Ok(page_before_exit) =
    wait_for_event_name(
      hub_subject,
      "workflow-step-ABC-123-42-1-implement-a1-f9bb818d8483",
      "message_update",
      20,
    )
  assert list.contains(event_names(page_before_exit.events), "message_update")

  let assert Ok(parent_page_before_exit) =
    hub.events_after(hub_subject, "ABC-123-42-1", 0, 20, 1000)
  assert !list.contains(
    event_names(parent_page_before_exit.events),
    "message_update",
  )
  assert !list.contains(
    event_names(parent_page_before_exit.events),
    "worker_exited",
  )

  test_async.release_barrier(worker_barrier)
  assert wait_for_log(log_subject, "worker_exited", 30)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_retry_uses_unique_session_ids_with_same_clock_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/daemon-retry-sessions")
  let first = issue("retry-id", "ABC-RETRY", "Todo")
  let second = tracker_issue.Issue(..first, title: "retry succeeds")
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    dependencies(
      client,
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        case issue.title == "retry succeeds" {
          False ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("boom")),
              workspace_path: Some(expected_workspace),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          True ->
            Ok(success(
              tracker_issue.Issue(
                ..issue,
                state: issue_state.from_string_unchecked("Done"),
              ),
              expected_workspace,
            ))
        }
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, first)
  assert wait_for_log(log_subject, "retry_scheduled", 20)

  let assert Ok(failed_summary) =
    wait_for_session(hub_subject, "ABC-RETRY-42-1", 20)
  assert failed_summary.status == event.Exited(reason.Failed)
  let assert Ok(failed_page) =
    hub.events_after(hub_subject, "ABC-RETRY-42-1", 0, 20, 1000)
  assert !list.contains(event_names(failed_page.events), "retry_scheduled")

  process.send(started.data, daemon.RetryTick("retry-id", 1))
  let assert Ok(retry_refresh) = process.receive(refresh_subject, within: 1000)
  process.send(retry_refresh, second)
  assert wait_for_log(log_subject, "worker_exited", 20)

  let assert Ok(succeeded_summary) =
    wait_for_session(hub_subject, "ABC-RETRY-42-2", 20)
  assert succeeded_summary.status == event.Exited(reason.Normal)
  let assert Ok(_) =
    hub.events_after(hub_subject, "ABC-RETRY-42-1", 0, 20, 1000)
  let assert Ok(_) =
    hub.events_after(hub_subject, "ABC-RETRY-42-2", 0, 20, 1000)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_startup_recovery_attaches_interrupted_metadata_to_retry_session_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-startup-recovery-session")
  let recovered = issue("recovered-id", "ABC-REC", "Todo")
  let assert Ok(#(_, known_workspace)) =
    workspace.workspace_path(root, recovered.identifier)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "old-run-started",
          1000,
          record.RunStarted(
            run_id: "old-run",
            issue_id: recovered.id,
            issue_identifier: recovered.identifier,
            workspace_path: known_workspace,
          ),
        ),
      ],
      False,
    )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([recovered]) },
    )
  let deps =
    dependencies(
      client,
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          known_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.RetryTick("recovered-id", 1))
  assert wait_for_log(log_subject, "worker_exited", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-REC-42-1", 20)
  let assert Some(recovery) = summary.recovery
  assert recovery.status == event.Interrupted
  assert recovery.source == "projection.run_running"
  assert recovery.workflow_run_id == Some("old-run")
  assert summary.status == event.Exited(reason.Normal)

  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-REC-42-1", 0, 20, 1000)
  let assert Some(recovery_event) =
    find_event(page.events, "recovery_interrupted")
  let assert Some(event_recovery) = recovery_event.payload.recovery
  assert event_recovery.status == event.Interrupted
  assert event_recovery.workflow_run_id == Some("old-run")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_post_success_cleanup_warning_publishes_recovery_cleanup_event_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-post-success-cleanup-warning")
  let candidate = issue("cleanup-id", "ABC-CLEANUP", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let base =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base,
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base.workflow_run_dependencies,
        cleanup_run: fn(_run_root, _orchestrator, _profile) {
          Error(error.WorkspaceIo("delete failed"))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_exited", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-CLEANUP-42-1", 20)
  assert summary.status == event.Exited(reason.Normal)
  let assert Some(recovery) = summary.recovery
  assert recovery.status == event.Cleanup
  assert recovery.source == "workflow.post_success_cleanup"

  let assert Ok(page) =
    wait_for_event_name(hub_subject, "ABC-CLEANUP-42-1", "recovery_cleanup", 20)
  let assert Some(cleanup_event) = find_event(page.events, "recovery_cleanup")
  let assert Some(event_recovery) = cleanup_event.payload.recovery
  assert event_recovery.status == event.Cleanup
  assert event_recovery.source == "workflow.post_success_cleanup"
  assert list.contains(event_names(page.events), "worker_exited")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_scheduled_post_success_cleanup_warning_publishes_recovery_cleanup_event_test() {
  let #(workflow_path, _root) =
    write_scheduled_workflow(
      "test/tmp/daemon-scheduled-post-success-cleanup-warning",
    )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let clock = start_test_clock(100)
  let base =
    dependencies(
      tracker.Client(
        fetch_candidate_issues: fn() { Ok([]) },
        fetch_issues_by_states: fn(_) { Ok([]) },
        fetch_issue_states_by_ids: fn(_) { Ok([]) },
      ),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          "",
        ))
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base,
      now_ms: fn() { clock_now(clock) },
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base.workflow_run_dependencies,
        cleanup_run: fn(_run_root, _orchestrator, _profile) {
          Error(error.WorkspaceIo("delete failed"))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "scheduled_worker_exited", 20)

  let session_id = "schedule-scheduled-job-19700101T000001Z-a1"
  let assert Ok(summary) = wait_for_session(hub_subject, session_id, 20)
  assert summary.status == event.Exited(reason.Normal)
  let assert Some(recovery) = summary.recovery
  assert recovery.status == event.Cleanup
  assert recovery.source == "workflow.post_success_cleanup"

  let assert Ok(page) =
    wait_for_event_name(hub_subject, session_id, "recovery_cleanup", 20)
  let assert Some(cleanup_event) = find_event(page.events, "recovery_cleanup")
  let assert Some(event_recovery) = cleanup_event.payload.recovery
  assert event_recovery.status == event.Cleanup
  assert event_recovery.source == "workflow.post_success_cleanup"
  assert list.contains(event_names(page.events), "worker_exited")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
  process.send(clock, StopClock)
}

pub fn daemon_success_continuation_does_not_publish_retry_to_exited_session_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-success-session-cleanup")
  let candidate = issue("active-id", "ABC-ACTIVE", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Todo"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_exited", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-ACTIVE-42-1", 20)
  assert summary.status == event.Exited(reason.Normal)
  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-ACTIVE-42-1", 0, 20, 1000)
  assert !list.contains(event_names(page.events), "retry_scheduled")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_worker_down_does_not_publish_retry_to_exited_session_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-down-session-cleanup")
  let candidate = issue("down-id", "ABC-DOWN", "Todo")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        process.kill(process.self())
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("worker_down")),
          workspace_path: Some(expected_workspace),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "retry_scheduled", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-DOWN-42-1", 20)
  assert summary.status == event.Exited(reason.Failed)
  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-DOWN-42-1", 0, 20, 1000)
  assert list.contains(event_names(page.events), "worker_exited")
  assert !list.contains(event_names(page.events), "retry_scheduled")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_stop_finishes_session_without_stale_lifecycle_events_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/daemon-stop-session-cleanup")
  let candidate = issue("stop-id", "ABC-STOP", "Todo")
  let terminal =
    tracker_issue.Issue(
      ..candidate,
      state: issue_state.from_string_unchecked("Done"),
    )
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    dependencies(
      client,
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, _expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        test_async.block_until_released(worker_barrier)
        Ok(success(
          tracker_issue.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          "unreachable",
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, candidate)
  assert wait_for_log(log_subject, "dispatch_started", 20)
  process.send(started.data, daemon.PollTick(2))
  let assert Ok(running_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(running_refresh, terminal)
  assert wait_for_log(log_subject, "worker_stop_requested", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-STOP-42-1", 20)
  assert summary.status == event.Exited(reason.Stopped)
  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-STOP-42-1", 0, 20, 1000)
  assert list.contains(event_names(page.events), "stop_requested")
  assert !list.contains(event_names(page.events), "retry_scheduled")

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_start_fails_when_event_hub_start_fails_test() {
  let #(workflow_path, _) = write_workflow("test/tmp/daemon-hub-start-failure")
  let candidate = issue("issue-id", "ABC-123", "Todo")
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(
        client_with(candidate),
        log_subject,
        process.new_subject(),
        fn(_, _, _, _, _, _, _, _) {
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("not used")),
            workspace_path: None,
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
      start_event_hub: fn() { Error(hub.HubUnavailable) },
    )

  let assert Error(daemon.StartupError(code: code, message: _)) =
    daemon.start(Some(workflow_path), deps)
  assert code == "event_hub_start_failed"
}

fn wait_for_log(
  subject: process.Subject(String),
  expected: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 250) {
        Ok(actual) ->
          case actual == expected {
            True -> True
            False -> wait_for_log(subject, expected, attempts - 1)
          }
        Error(_) -> {
          process.sleep(50)
          wait_for_log(subject, expected, attempts - 1)
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
      case hub.get_session(subject, session_id, 250) {
        Ok(Some(summary)) ->
          case summary.status {
            event.Exited(_) -> Ok(summary)
            _ -> {
              process.sleep(50)
              wait_for_session(subject, session_id, attempts - 1)
            }
          }
        _ -> {
          process.sleep(50)
          wait_for_session(subject, session_id, attempts - 1)
        }
      }
  }
}

fn wait_for_event_name(
  subject: process.Subject(hub.Message),
  session_id: String,
  name: String,
  attempts: Int,
) -> Result(event.EventPage, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.events_after(subject, session_id, 0, 20, 250) {
        Ok(page) ->
          case list.contains(event_names(page.events), name) {
            True -> Ok(page)
            False -> {
              process.sleep(50)
              wait_for_event_name(subject, session_id, name, attempts - 1)
            }
          }
        Error(_) -> {
          process.sleep(50)
          wait_for_event_name(subject, session_id, name, attempts - 1)
        }
      }
  }
}

fn find_event(
  events: List(event.SessionEvent),
  name: String,
) -> Option(event.SessionEvent) {
  case events {
    [] -> None
    [stored_event, ..rest] ->
      case event.name_to_string(stored_event.payload.name) == name {
        True -> Some(stored_event)
        False -> find_event(rest, name)
      }
  }
}

fn event_names(events: List(event.SessionEvent)) -> List(String) {
  list.map(events, fn(stored_event) {
    event.name_to_string(stored_event.payload.name)
  })
}

fn event_cursors(events: List(event.SessionEvent)) -> List(Int) {
  list.map(events, fn(stored_event) { stored_event.cursor })
}
