import birl
import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/orchestrator/daemon
import scherzo/session/hub
import scherzo/tracker
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
    state: state,
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  "---\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\nworkspace:\n  root: "
  <> root
  <> "\nhooks:\n  before_run: \"true\"\npolling:\n  interval_ms: 1000\nagent:\n  max_concurrent_agents: "
  <> int_to_string(max_concurrent)
  <> "\n  max_retry_attempts: 3\n  max_sessions_per_issue: 2\npi:\n  command: fake\n---\nPrompt\n"
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, max_concurrent))
  workflow_path
}

fn success(final: domain.Issue, workspace_path: String) -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(final),
    final_classification: runner.FinalTerminal,
    workspace_path: workspace_path,
    tokens: domain.zero_token_totals(),
    turns: 1,
  )
}

fn base_dependencies(
  client: tracker.Client,
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    agent_runner: fn(issue, _, _, _, _, emit_update, _, _) {
      process.send(log_subject, "agent_run")
      emit_update(
        issue.id,
        runner.PiUpdate(
          event: "turn_finished",
          message: Some("hello"),
          raw_json: None,
          turn: Some(1),
          request_id: None,
          method: None,
          pi_session_id: None,
          tokens: domain.zero_token_totals(),
          tool_name: None,
        ),
      )
      Ok(success(
        domain.Issue(..issue, state: "Done"),
        "test/tmp/daemon/workspace",
      ))
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, _, _) {
      process.send(log_subject, event)
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { hub.start(10, fn() { 42 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn wait_for_event(
  subject: process.Subject(String),
  event: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          case received == event {
            True -> True
            False -> wait_for_event(subject, event, attempts - 1)
          }
        Error(_) -> False
      }
  }
}

pub fn daemon_poll_dispatches_fake_worker_routes_update_and_shutdown_test() {
  let workflow_path = write_workflow("test/tmp/daemon-basic", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        Ok([domain.Issue(..candidate, state: "Done")])
      },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "tick_started", 10)
  assert wait_for_event(log_subject, "dispatch_started", 10)
  assert wait_for_event(log_subject, "pi_event", 10)
  assert wait_for_event(log_subject, "worker_exited", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_retry_timer_requeues_failed_worker_once_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry", 1)
  let first = issue("retry-id", "ABC-2", "Todo")
  let second = domain.Issue(..first, title: "retry succeeds")
  let log_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([second]) },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      agent_runner: fn(issue: domain.Issue, _, _, _, _, _, _, _) {
        process.send(log_subject, "agent_run")
        case issue.title == "retry succeeds" {
          False ->
            Error(runner.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("boom")),
              workspace_path: Some("test/tmp/daemon-retry/workspace"),
              tokens: domain.zero_token_totals(),
              final_issue: None,
            ))
          True ->
            Ok(success(
              domain.Issue(..issue, state: "Done"),
              "test/tmp/daemon-retry/workspace",
            ))
        }
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "retry_scheduled", 20)

  process.send(started.data, daemon.RetryTick("retry-id", 99))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  assert wait_for_event(log_subject, "worker_exited", 20)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
