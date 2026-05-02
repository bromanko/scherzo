import birl
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/agent/runner
import scherzo/agent/worker_command
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_triage
import scherzo/orchestrator/daemon
import scherzo/path
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason
import scherzo/tracker
import scherzo/tracker/state as issue_state
import scherzo/workflow_run
import scherzo/workspace
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn issue(id: String, identifier: String, state: String) -> domain.Issue {
  domain.Issue(
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
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  active_states: [Todo]
  terminal_states: [Done]
workspace:
  root: " <> root <> "
  hooks:
    create: |
      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"
    before_step: |
      test -d \"$SCHERZO_WORKSPACE_PATH\"
    after_step: |
      true
    remove: |
      rm -rf \"$SCHERZO_WORKSPACE_PATH\"
    timeout_ms: 60000
polling:
  interval_ms: 1000
agent:
  max_concurrent_agents: 1
  max_retry_attempts: 3
  max_sessions_per_issue: 2
pi:
  command: fake
routing:
  workflow_label_prefix: \"workflow:\"
  require_exactly_one_workflow_label: false
  default_workflow: implementation
  workflows:
    implementation: workflows/implementation.yaml
"
}

fn write_workflow(dir: String) -> #(String, String) {
  reset_dir(dir)
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
    workspace: main
",
    )
  #(config_path, root)
}

fn success(
  final: domain.Issue,
  workspace_path: String,
) -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(final),
    final_classification: runner.FinalTerminal,
    workspace_path: workspace_path,
    tokens: domain.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 0,
      cache_write: 0,
      total: 3,
    ),
    turns: 1,
    result: domain.ResultArtifact(
      final_response: None,
      truncated: False,
      source: "none",
    ),
  )
}

fn update(name: String, message: Option(String)) -> runner.PiUpdate {
  runner.PiUpdate(
    event: pi_event.from_string(name),
    message: message,
    raw_json: None,
    turn: Some(1),
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: domain.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

fn client_with(candidate: domain.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) {
      Ok([
        domain.Issue(
          ..candidate,
          state: issue_state.from_string_unchecked("Done"),
        ),
      ])
    },
  )
}

fn workflow_deps_from_agent(
  agent_runner: fn(
    domain.Issue,
    Option(Int),
    String,
    domain.EffectiveConfig,
    tracker.Client,
    fn(String, runner.PiUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(runner.WorkerSuccess, runner.WorkerFailure),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    ..workflow_run.default_dependencies(),
    agent_step: fn(
      issue,
      _step_id,
      prompt,
      effective,
      tracker_client,
      _workspace_path,
      emit_update,
      command_ready,
    ) {
      let command_subject = process.new_subject()
      agent_runner(
        issue,
        None,
        prompt,
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
    domain.Issue,
    Option(Int),
    String,
    domain.EffectiveConfig,
    tracker.Client,
    fn(String, runner.PiUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(runner.WorkerSuccess, runner.WorkerFailure),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    make_linear_commands: fn(_) { disabled_linear_commands() },
    make_triage: fn(_, _) { linear_triage.disabled_client() },
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

fn disabled_linear_commands() -> linear.CommandClient {
  linear.CommandClient(fetch_comments: fn(_, _) { Ok([]) }, post_ack: fn(_, _) {
    Ok(Nil)
  })
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
          domain.Issue(
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
  assert summary.workspace_path == expected_workspace
  assert summary.status == event.Exited(reason.Normal)
  assert summary.token_totals.total == 3

  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-123-42-1", 0, 20, 1000)
  assert event_names(page.events)
    == ["dispatch_started", "worker_started", "worker_exited"]
  assert event_cursors(page.events) == [1, 2, 5]

  let assert Ok(step_page) =
    hub.events_after(hub_subject, "ABC-123-42-1-implement", 0, 20, 1000)
  assert event_names(step_page.events) == ["step_started", "message_update"]
  let assert Some(message_event) =
    find_event(step_page.events, "message_update")
  assert message_event.payload.kind == event.AssistantMessage

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
          runner.PiUpdate(
            event: pi_event.Message,
            message: None,
            raw_json: None,
            turn: Some(1),
            request_id: None,
            method: None,
            pi_session_id: None,
            tokens: domain.zero_token_totals(),
            tool_name: Some("bash"),
            tool_input: Some("gleam test"),
            tool_output: None,
            tool_status: None,
          ),
        )
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          domain.Issue(
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
    hub.events_after(hub_subject, "ABC-TOOL-42-1-implement", 0, 20, 1000)
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
  let assert Ok(hub_subject) = hub.start(20, fn() { 100 })
  let deps =
    dependencies(
      client_with(candidate),
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, emit_update, _, _) {
        emit_update(issue.id, update("message_update", Some("hello")))
        process.sleep(800)
        let assert Ok(#(_, expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        Ok(success(
          domain.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          expected_workspace,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(page_before_exit) =
    wait_for_event_name(
      hub_subject,
      "ABC-123-42-1-implement",
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

  assert wait_for_log(log_subject, "worker_exited", 30)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_retry_uses_unique_session_ids_with_same_clock_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/daemon-retry-sessions")
  let first = issue("retry-id", "ABC-RETRY", "Todo")
  let second = domain.Issue(..first, title: "retry succeeds")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([second]) },
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
            Error(runner.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("boom")),
              workspace_path: Some(expected_workspace),
              tokens: domain.zero_token_totals(),
              final_issue: None,
            ))
          True ->
            Ok(success(
              domain.Issue(
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
  assert wait_for_log(log_subject, "retry_scheduled", 20)

  let assert Ok(failed_summary) =
    wait_for_session(hub_subject, "ABC-RETRY-42-1", 20)
  assert failed_summary.status == event.Exited(reason.Failed)
  let assert Ok(failed_page) =
    hub.events_after(hub_subject, "ABC-RETRY-42-1", 0, 20, 1000)
  assert !list.contains(event_names(failed_page.events), "retry_scheduled")

  process.send(started.data, daemon.RetryTick("retry-id", 1))
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
          domain.Issue(
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
        Error(runner.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("worker_down")),
          workspace_path: Some(expected_workspace),
          tokens: domain.zero_token_totals(),
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
    domain.Issue(..candidate, state: issue_state.from_string_unchecked("Done"))
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 100 })
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([terminal]) },
    )
  let deps =
    dependencies(
      client,
      log_subject,
      hub_subject,
      fn(issue, _, _, _, _, _, _, _) {
        let assert Ok(#(_, _expected_workspace)) =
          workspace.workspace_path(root, issue.identifier)
        process.sleep(2000)
        Ok(success(
          domain.Issue(
            ..issue,
            state: issue_state.from_string_unchecked("Done"),
          ),
          "unreachable",
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "worker_stop_requested", 20)

  let assert Ok(summary) = wait_for_session(hub_subject, "ABC-STOP-42-1", 20)
  assert summary.status == event.Exited(reason.Stopped)
  let assert Ok(page) =
    hub.events_after(hub_subject, "ABC-STOP-42-1", 0, 20, 1000)
  assert list.contains(event_names(page.events), "stop_requested")
  assert !list.contains(event_names(page.events), "retry_scheduled")

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
          Error(runner.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("not used")),
            workspace_path: None,
            tokens: domain.zero_token_totals(),
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
