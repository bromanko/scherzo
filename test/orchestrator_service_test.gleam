import birl
import gleam/dict
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/linear
import scherzo/linear_contract
import scherzo/orchestrator/service
import scherzo/path
import scherzo/step_artifact
import scherzo/tracker
import scherzo/workflow_run
import scherzo/workspace_run
import simplifile

pub type CapturedLog {
  CapturedLog(
    level: String,
    event: String,
    fields: List(#(String, String)),
    secrets: List(String),
  )
}

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

fn yaml_config(root: String, extra: String) -> String {
  yaml_config_with_max(root, 1, extra)
}

fn yaml_config_with_max(
  root: String,
  max_concurrent: Int,
  extra: String,
) -> String {
  "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: [Todo]\n  terminal_states: [Done]\nworkspace:\n  root: "
  <> root
  <> "\n  hooks:\n    create: |\n      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n    before_step: |\n      test -d \"$SCHERZO_WORKSPACE_PATH\"\n    after_step: |\n      true\n    remove: |\n      rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n    timeout_ms: 60000\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nagent:\n  max_concurrent_agents: "
  <> int_to_string(max_concurrent)
  <> "\n  max_turns: 1\n"
  <> extra
}

fn command_workflow_yaml(command: String) -> String {
  "version: 1\nid: implementation\nsteps:\n  - id: final_test\n    kind: command\n    run: "
  <> command
  <> "\n    workspace: main\n"
}

fn deps(client: tracker.Client) -> service.Dependencies {
  service.Dependencies(
    tracker: fn(_) { client },
    workflow_run_dependencies: workflow_deps(),
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

fn workflow_deps() -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      _step_id,
      workspace_ref,
      orchestrator,
      _known,
    ) {
      let run_root =
        orchestrator.effective.workspace.root
        <> "/"
        <> workflow_id
        <> "/"
        <> issue.identifier
        <> "/"
        <> run_id
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        workspace_name: workspace_ref.name,
        path: run_root <> "/" <> workspace_ref.name,
        source_workspace_name: workspace_ref.from,
        source_workspace_path: None,
      ))
    },
    after_step: fn(_, _, _, _) { Nil },
    cleanup_run: fn(_, _) { Ok(Nil) },
    command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
      step_artifact.from_command_result(
        step_id,
        0,
        "stdout:" <> step_id,
        "",
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      issue,
      _step_id,
      prompt,
      _effective,
      _tracker,
      workspace_path,
      _emit_update,
      _command_ready,
    ) {
      Ok(runner.WorkerSuccess(
        final_issue: Some(issue),
        final_classification: runner.FinalTerminal,
        workspace_path: workspace_path,
        tokens: domain.zero_token_totals(),
        turns: 1,
        result: domain.ResultArtifact(
          final_response: Some(prompt),
          truncated: False,
          source: "test",
        ),
      ))
    },
  )
}

fn contract_config_text(root: String, active_state: String) -> String {
  "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: ["
  <> active_state
  <> "]\n  terminal_states: [Done]\nworkspace:\n  root: "
  <> root
  <> "\n  hooks:\n    create: |\n      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n"
}

fn contract_team(
  states: List(linear_contract.RemoteState),
) -> linear_contract.RemoteTeam {
  linear_contract.RemoteTeam(
    id: "team-eng",
    key: "ENG",
    name: "Engineering",
    states: states,
    labels: [],
  )
}

fn contract_state(id: String, name: String) -> linear_contract.RemoteState {
  linear_contract.RemoteState(id: id, name: name, type_: "started")
}

fn contract_board(
  states: List(linear_contract.RemoteState),
) -> linear_contract.RemoteBoard {
  linear_contract.RemoteBoard(
    project_id: "project-id",
    project_slug: "TEST",
    project_name: "Test Project",
    teams: [contract_team(states)],
    workspace_labels: [],
  )
}

fn contract_deps(
  result: Result(linear_contract.RemoteBoard, error.TrackerError),
  subject: process.Subject(CapturedLog),
) -> service.ContractCheckDependencies {
  service.ContractCheckDependencies(
    make_contract_client: fn(_) {
      linear.ContractClient(fetch_remote_contract: fn() { result })
    },
    logger: fn(level, event, fields, secrets) {
      process.send(subject, CapturedLog(level, event, fields, secrets))
      Ok(Nil)
    },
  )
}

fn field_value(fields: List(#(String, String)), key: String) -> Option(String) {
  case fields {
    [] -> None
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> Some(value)
        False -> field_value(rest, key)
      }
  }
}

pub fn startup_fails_on_missing_config_test() {
  let assert Error(err) =
    service.run_once_with_dependencies(
      Some("test/tmp/no-such-scherzo.yaml"),
      deps(empty_tracker()),
    )
  assert err.code == "missing_config_file"
}

pub fn paused_config_skips_dispatch_but_loads_workflow_test() {
  let root = "test/tmp/service-paused/workspaces"
  reset_dir("test/tmp/service-paused")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/service-paused/workflows")
  let workflow_path = "test/tmp/service-paused/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, yaml_config_with_max(root, 0, ""))
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-paused/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
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
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/service-pi-probe/workflows")
  let workflow_path = "test/tmp/service-pi-probe/scherzo.yaml"
  let transcript_path = "test/tmp/service-pi-probe/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      yaml_config(
        root,
        "pi:\n  command: \"" <> command <> "\"\n  compatibility_probe: true\n",
      ),
    )
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-pi-probe/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  assert service.start_pi_probe(Some(workflow_path)) == Ok(Nil)
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "set_session_name")
  assert string.contains(contents, "get_session_stats")
  assert !string.contains(contents, "prompt")
}

pub fn linear_contract_check_success_logs_structured_summary_test() {
  let root = "test/tmp/service-contract-ok/workspaces"
  reset_dir("test/tmp/service-contract-ok")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/service-contract-ok/workflows")
  let workflow_path = "test/tmp/service-contract-ok/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, contract_config_text(root, "Todo"))
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-contract-ok/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let log_subject = process.new_subject()
  let result =
    service.start_linear_contract_check_with_dependencies(
      Some(workflow_path),
      contract_deps(
        Ok(
          contract_board([
            contract_state("state-todo", "Todo"),
            contract_state("state-done", "Done"),
          ]),
        ),
        log_subject,
      ),
    )
  assert result == Ok(Nil)
  let assert Ok(CapturedLog(
    level: "info",
    event: "linear_contract_ok",
    fields: fields,
    secrets: secrets,
  )) = process.receive(log_subject, within: 1000)
  assert secrets == ["test-key"]
  assert field_value(fields, "project_slug") == Some("TEST")
  assert field_value(fields, "project_id") == Some("project-id")
  assert field_value(fields, "team_count") == Some("1")
  assert field_value(fields, "state_count") == Some("2")
}

pub fn linear_contract_check_mismatch_logs_diagnostics_and_fails_test() {
  let root = "test/tmp/service-contract-mismatch/workspaces"
  reset_dir("test/tmp/service-contract-mismatch")
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      "test/tmp/service-contract-mismatch/workflows",
    )
  let workflow_path = "test/tmp/service-contract-mismatch/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      contract_config_text(root, "Ready for Agent"),
    )
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-contract-mismatch/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let log_subject = process.new_subject()
  let assert Error(err) =
    service.start_linear_contract_check_with_dependencies(
      Some(workflow_path),
      contract_deps(
        Ok(
          contract_board([
            contract_state("state-todo", "Todo"),
            contract_state("state-done", "Done"),
          ]),
        ),
        log_subject,
      ),
    )
  assert err.code == "linear_contract_mismatch"
  let assert Ok(CapturedLog(
    level: "error",
    event: "linear_contract_mismatch",
    fields: mismatch_fields,
    secrets: _,
  )) = process.receive(log_subject, within: 1000)
  assert field_value(mismatch_fields, "diagnostic_count") == Some("1")
  let assert Ok(CapturedLog(
    level: "error",
    event: "linear_contract_diagnostic",
    fields: diagnostic_fields,
    secrets: _,
  )) = process.receive(log_subject, within: 1000)
  assert field_value(diagnostic_fields, "code") == Some("missing_state")
  assert field_value(diagnostic_fields, "team") == Some("ENG")
  assert field_value(diagnostic_fields, "source")
    == Some("tracker.active_states")
  assert field_value(diagnostic_fields, "name") == Some("Ready for Agent")
}

pub fn linear_contract_check_fetch_error_maps_to_startup_failure_test() {
  let root = "test/tmp/service-contract-fetch-error/workspaces"
  reset_dir("test/tmp/service-contract-fetch-error")
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      "test/tmp/service-contract-fetch-error/workflows",
    )
  let workflow_path = "test/tmp/service-contract-fetch-error/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, contract_config_text(root, "Todo"))
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-contract-fetch-error/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let log_subject = process.new_subject()
  let assert Error(err) =
    service.start_linear_contract_check_with_dependencies(
      Some(workflow_path),
      contract_deps(Error(error.LinearApiStatus(500)), log_subject),
    )
  assert err.code == "linear_api_status"
  assert process.receive(log_subject, within: 50) == Error(Nil)
}

pub fn yaml_once_runs_command_workflow_test() {
  let dir = "test/tmp/service-yaml-once"
  let root = "workspaces"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) = simplifile.write(config_path, yaml_config(root, ""))
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      command_workflow_yaml("sh -c 'exit 1'"),
    )
  let candidate =
    domain.Issue(..issue("Todo"), labels: ["workflow:implementation"])
  let assert Ok(result) =
    service.run_once_with_dependencies(
      Some(config_path),
      deps(tracker_with_candidate(candidate, candidate)),
    )
  assert result.dispatched == 1
  assert contains_log(result.logs, "dispatch_started")
  assert contains_log(result.logs, "worker_exited")
  assert contains_log(result.logs, "workspace_cleaned")
  assert dict.has_key(result.state.completed, "issue-id")
  assert simplifile.is_directory(
      dir <> "/workspaces/implementation/ABC-123/ABC-123-once",
    )
    != Ok(True)
}

pub fn yaml_once_skips_issue_without_workflow_label_test() {
  let dir = "test/tmp/service-yaml-missing-label"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(config_path, yaml_config("workspaces", ""))
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let assert Ok(result) =
    service.run_once_with_dependencies(
      Some(config_path),
      deps(tracker_with_candidate(issue("Todo"), issue("Todo"))),
    )
  assert result.dispatched == 0
  assert contains_log(result.logs, "workflow_route_failed")
}

pub fn yaml_linear_contract_check_uses_orchestrator_config_test() {
  let dir = "test/tmp/service-yaml-contract"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(config_path, yaml_config("workspaces", ""))
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let log_subject = process.new_subject()
  let result =
    service.start_linear_contract_check_with_dependencies(
      Some(config_path),
      contract_deps(
        Ok(
          contract_board([
            contract_state("state-todo", "Todo"),
            contract_state("state-done", "Done"),
          ]),
        ),
        log_subject,
      ),
    )
  assert result == Ok(Nil)
  let assert Ok(CapturedLog(event: "linear_contract_ok", ..)) =
    process.receive(log_subject, within: 1000)
}

pub fn fake_end_to_end_service_dispatch_test() {
  let root = "test/tmp/service-integration/workspaces"
  reset_dir("test/tmp/service-integration")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/service-integration/workflows")
  let workflow_path = "test/tmp/service-integration/scherzo.yaml"
  let assert Ok(Nil) = simplifile.write(workflow_path, yaml_config(root, ""))
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/service-integration/workflows/implementation.yaml",
      command_workflow_yaml("printf ok"),
    )
  let candidate =
    domain.Issue(..issue("Todo"), labels: ["workflow:implementation"])
  let assert Ok(result) =
    service.run_once_with_dependencies(
      Some(workflow_path),
      deps(tracker_with_candidate(candidate, issue("Done"))),
    )
  assert result.dispatched == 1
  assert contains_log(result.logs, "dispatch_started")
  assert contains_log(result.logs, "worker_exited")
  assert contains_log(result.logs, "workspace_cleaned")
  assert !contains_log(result.logs, "empty_path")
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
