import birl
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/orchestrator/service
import scherzo/path
import scherzo/tracker
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn issue(state: String) -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Fix tests",
    description: Some("Broken"),
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

fn workflow_text(root: String, command: String, max_concurrent: Int) -> String {
  "---\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\nworkspace:\n  root: "
  <> root
  <> "\nhooks:\n  after_create: |\n    printf populated > POPULATED\n  before_run: |\n    test -f POPULATED\nagent:\n  max_concurrent_agents: "
  <> int_to_string(max_concurrent)
  <> "\n  max_turns: 1\npi:\n  command: \""
  <> command
  <> "\"\n  compatibility_probe: true\n---\nWork on {{ issue.identifier }}\n"
}

fn deps(client: tracker.Client) -> service.Dependencies {
  service.Dependencies(
    tracker: fn(_) { client },
    agent_runner: runner.run_attempt,
    cleanup: fn(root, path, hooks) {
      let _ = root
      let _ = path
      let _ = hooks
      Ok(Nil)
    },
    logger: fn(_line) { Ok(Nil) },
    now_ms: fn() { 0 },
  )
}

pub fn startup_fails_on_missing_workflow_test() {
  let assert Error(err) =
    service.run_once_with_dependencies(
      Some("test/tmp/no-such-workflow.md"),
      deps(empty_tracker()),
    )
  assert err.code == "missing_workflow_file"
}

pub fn paused_config_skips_dispatch_but_loads_workflow_test() {
  let root = "test/tmp/service-paused/workspaces"
  reset_dir("test/tmp/service-paused")
  let workflow_path = "test/tmp/service-paused/WORKFLOW.md"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, fake_pi(), 0))
  let assert Ok(result) =
    service.run_once_with_dependencies(
      Some(workflow_path),
      deps(tracker_with_candidate(issue("Todo"), issue("Done"))),
    )
  assert result.dispatched == 0
  assert contains_log(result.logs, "dispatch_paused")
}

pub fn pi_probe_mode_launches_without_prompt_test() {
  let root = "test/tmp/service-pi-probe/workspaces"
  reset_dir("test/tmp/service-pi-probe")
  let workflow_path = "test/tmp/service-pi-probe/WORKFLOW.md"
  let transcript_path = "test/tmp/service-pi-probe/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, command, 1))
  assert service.start_pi_probe(Some(workflow_path)) == Ok(Nil)
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "set_session_name")
  assert string.contains(contents, "get_session_stats")
  assert !string.contains(contents, "prompt")
}

pub fn fake_end_to_end_service_dispatch_test() {
  let root = "test/tmp/service-integration/workspaces"
  reset_dir("test/tmp/service-integration")
  let workflow_path = "test/tmp/service-integration/WORKFLOW.md"
  let transcript_path = "test/tmp/service-integration/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, command, 1))
  let assert Ok(result) =
    service.run_once_with_dependencies(
      Some(workflow_path),
      deps(tracker_with_candidate(issue("Todo"), issue("Done"))),
    )
  assert result.dispatched == 1
  assert contains_log(result.logs, "dispatch_started")
  assert contains_log(result.logs, "worker_exited")
  assert contains_log(result.logs, "workspace_cleaned")
  assert !contains_log(result.logs, "empty_path")
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "get_state")
  assert string.contains(contents, "prompt")
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn tracker_with_candidate(
  candidate: domain.Issue,
  final: domain.Issue,
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([final]) },
  )
}

fn contains_log(logs: List(String), text: String) -> Bool {
  case logs {
    [] -> False
    [line, ..rest] -> string.contains(line, text) || contains_log(rest, text)
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
