import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/runner
import scherzo/control/client
import scherzo/control/file as control_file
import scherzo/error
import scherzo/handoff
import scherzo/orchestrator/daemon
import scherzo/tracker
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn workflow_text(root: String) -> String {
  "---\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\nworkspace:\n  root: "
  <> root
  <> "\nhooks:\n  before_run: \"true\"\npolling:\n  interval_ms: 1000\nagent:\n  max_concurrent_agents: 0\n  max_retry_attempts: 1\n  max_sessions_per_issue: 1\npi:\n  command: fake\n---\nPrompt\n"
}

fn write_workflow(dir: String) -> #(String, String) {
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) = simplifile.write(workflow_path, workflow_text(root))
  #(workflow_path, root)
}

fn dependencies(
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker: fn(_) {
      tracker.Client(
        fetch_candidate_issues: fn() { Ok([]) },
        fetch_issues_by_states: fn(_) { Ok([]) },
        fetch_issue_states_by_ids: fn(_) { Ok([]) },
      )
    },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    agent_runner: fn(_, _, _, _, _, _) {
      Error(runner.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("not used")),
        workspace_path: None,
      ))
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
