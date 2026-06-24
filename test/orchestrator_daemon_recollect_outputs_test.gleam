import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/artifact_repository/command_runner
import scherzo/control/command
import scherzo/error
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/startup_recovery
import scherzo/runtime_bundle
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_run
import scherzo/workflow_run/contract_io
import simplifile
import support/test_helpers

pub fn recollect_outputs_daemon_applies_without_worker_or_terminal_records_test() {
  let dir = "test/tmp/daemon-recollect-outputs/applied"
  let issue = issue()
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let seed = seed_completed_run(workflow_path, root, issue, False)
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let before = ledger_kinds(root)
  let before_finished = count_kind(before, "workflow_run_finished")

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      5000,
    )

  let after = ledger_kinds(root)
  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(message, "recollected workflow outputs for run-1")
  assert list.length(after) == list.length(before) + 1
  assert last(after) == Some("workflow_run_outputs_recorded")
  assert count_kind(after, "workflow_run_finished") == before_finished
  assert process.receive(worker_subject, within: 20) == Error(Nil)
  let assert Ok(output_ref) = latest_output_ref(root)
  assert output_ref == "runs/run-1/recollections/1/outputs.v1.json"
  let assert Ok(step_ref) = latest_step_artifact_ref(root)
  assert step_ref == seed.step_artifact_ref

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn recollect_outputs_daemon_rejects_parked_issue_without_mutation_test() {
  let dir = "test/tmp/daemon-recollect-outputs/parked"
  let issue = issue()
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let _seed = seed_completed_run(workflow_path, root, issue, False)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append(
      ledger_path,
      record.with_id(
        "issue-parked",
        20,
        record.IssueParkedV2(
          issue.id,
          issue.identifier,
          "operator_hold",
          "manual",
          tracker_issue.content_fingerprint(issue),
          20,
        ),
      ),
      True,
    )
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let before = ledger_kinds(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      1000,
    )

  let after = ledger_kinds(root)
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_parked")
  let assert Some(message) = result.message
  assert string.contains(message, "unpark before recollect-outputs")
  assert after == before
  assert process.receive(worker_subject, within: 20) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn recollect_outputs_daemon_rejects_missing_artifact_without_mutation_test() {
  let dir = "test/tmp/daemon-recollect-outputs/rejected"
  let issue = issue()
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let seed = seed_completed_run(workflow_path, root, issue, False)
  let assert Ok(Nil) = delete_artifact(root, seed.step_artifact_ref)
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let before = ledger_kinds(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      1000,
    )

  let after = ledger_kinds(root)
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status)
    == Some("artifact_recovery_failed")
  assert after == before
  assert process.receive(worker_subject, within: 20) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn recollect_outputs_daemon_applies_for_terminal_issue_state_test() {
  let dir = "test/tmp/daemon-recollect-outputs/terminal-state"
  let issue =
    tracker_issue.Issue(
      ..issue(),
      state: issue_state.from_string_unchecked("Done"),
    )
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let _seed = seed_completed_run(workflow_path, root, issue, False)
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(message, "recollected workflow outputs for run-1")
  assert process.receive(worker_subject, within: 20) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn recollect_outputs_daemon_is_idempotent_when_latest_manifest_valid_test() {
  let dir = "test/tmp/daemon-recollect-outputs/idempotent"
  let issue = issue()
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let _seed = seed_completed_run(workflow_path, root, issue, True)
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let before = ledger_kinds(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      1000,
    )

  let after = ledger_kinds(root)
  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(message, "workflow outputs already valid for run-1")
  assert after == before
  assert process.receive(worker_subject, within: 20) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn recollect_outputs_daemon_rejects_current_workflow_unavailable_without_mutation_test() {
  let dir = "test/tmp/daemon-recollect-outputs/workflow-unavailable"
  let issue = issue()
  let #(workflow_path, root) = write_recollect_workflow(dir)
  let _seed = seed_completed_run(workflow_path, root, issue, False)
  let current_issue = tracker_issue.Issue(..issue, labels: ["workflow:missing"])
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      tracker_issue_only(current_issue),
      hub_subject,
      worker_subject,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let before = ledger_kinds(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RecollectWorkflowOutputs("run-1"),
      1000,
    )

  let after = ledger_kinds(root)
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("workflow_unavailable")
  let assert Some(message) = result.message
  assert string.contains(message, "unknown_workflow_label")
  assert after == before
  assert process.receive(worker_subject, within: 20) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

type SeededRun {
  SeededRun(step_artifact_ref: String)
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "LIV-1208",
    title: "Recollect outputs",
    description: None,
    priority: None,
    state: issue_state.todo_state(),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn write_recollect_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = path_absolute(dir <> "/workspaces")
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.write(config_path, "version: 1
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
  sessions_per_task: 3
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
")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
contract:
  version: 1
  outputs:
    findings:
      type: document.markdown
      source:
        step: collect_findings
        field: stdout
steps:
  - id: collect_findings
    kind: command
    run: echo findings
",
    )
  #(config_path, root)
}

fn seed_completed_run(
  workflow_path: String,
  root: String,
  issue: tracker_issue.Issue,
  record_outputs: Bool,
) -> SeededRun {
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let assert recovery.CurrentWorkflow(
    _,
    workflow_id,
    workflow_fingerprint,
    _,
    dag,
    _,
  ) = workflow_observation_for(workflow_path, issue)
  let task_ref =
    workflow_checkpoint.linear_task_ref_for_issue(
      issue.id,
      issue.identifier,
      None,
    )
  let assert Ok(Nil) =
    checkpoint.workflow_started(workflow_checkpoint.WorkflowStarted(
      run_id: "run-1",
      workflow_id: workflow_id,
      workflow_fingerprint: workflow_fingerprint,
      issue_id: issue.id,
      issue_identifier: issue.identifier,
      task_ref: task_ref,
      issue_fingerprint: core.issue_fingerprint(issue),
      observed_updated_at_ms: 10,
      run_root: root,
    ))
  let workspace_path = root <> "/workspace/collect_findings"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace_path)
  let finished =
    workflow_checkpoint.StepFinished(
      run_id: "run-1",
      workflow_id: workflow_id,
      step_id: "collect_findings",
      attempt_index: 1,
      outcome: "completed",
      workspace_name: "main",
      workspace_path: workspace_path,
      token_total: 0,
      turns: 0,
    )
  let artifact =
    step_artifact.StepArtifact(
      step_id: "collect_findings",
      status: step_artifact.StepSucceeded,
      final_response: None,
      exit_code: Some(0),
      command: Some("echo findings"),
      duration_ms: Some(1),
      diagnostic_path: None,
      failure_code: None,
      stdout: "findings",
      stderr: "",
      timed_out: False,
      final_response_truncated: False,
      stdout_truncated: False,
      stderr_truncated: False,
      summary_text: "ok",
      structured_output: None,
    )
  let assert Ok(written) = checkpoint.write_step_artifact(finished, artifact)
  let assert Ok(Nil) = checkpoint.step_finished(finished, written)
  let assert Ok(Nil) =
    checkpoint.workflow_finished(workflow_checkpoint.WorkflowFinished(
      run_id: "run-1",
      workflow_id: workflow_id,
      issue_id: issue.id,
      task_ref: task_ref,
      outcome: "failed",
      token_total: 0,
      turns: 0,
    ))
  case record_outputs {
    True -> {
      let assert Ok(_outputs) =
        contract_io.record_outputs_if_contracted(
          dag,
          "run-1",
          workflow_fingerprint,
          None,
          checkpoint,
          dict.from_list([#("collect_findings", artifact)]),
          dict.new(),
        )
      Nil
    }
    False -> Nil
  }
  SeededRun(step_artifact_ref: written.ref)
}

fn workflow_observation_for(
  workflow_path: String,
  issue: tracker_issue.Issue,
) -> recovery.CurrentWorkflowObservation {
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  startup_recovery.current_workflow_observation(bundle, issue)
}

fn tracker_issue_only(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

fn in_process_dependencies(
  tracker_client: tracker.Client,
  hub_subject: process.Subject(hub.Message),
  worker_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      let legacy =
        adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
      adapter.TrackerAdapter(..legacy, handoff: None)
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    workflow_run_dependencies: workflow_run.Dependencies(
      ..workflow_run.default_dependencies(),
      agent_step: fn(
        issue: tracker_issue.Issue,
        _context,
        _,
        _,
        _effective,
        _,
        _,
        _,
        _,
      ) {
        process.send(worker_subject, "agent_run:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    ),
    publication_command_runner: command_runner.Runner(run: fn(_) {
      Error(command_runner.command_error("unexpected_publication_retry"))
    }),
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn ledger_kinds(root: String) -> List(String) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
  |> list.map(fn(ledger_record) { record.kind(ledger_record.body) })
}

fn count_kind(kinds: List(String), target: String) -> Int {
  kinds
  |> list.filter(fn(kind) { kind == target })
  |> list.length
}

fn last(values: List(a)) -> Option(a) {
  case values {
    [] -> None
    [value] -> Some(value)
    [_, ..rest] -> last(rest)
  }
}

fn delete_artifact(
  root: String,
  ref: String,
) -> Result(Nil, simplifile.FileError) {
  simplifile.delete(root <> "/.scherzo-state/artifacts/" <> ref)
}

fn latest_output_ref(root: String) -> Result(String, Nil) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  case projected.workflow_output_manifests |> dict.get("run-1") {
    Ok(manifest) -> Ok(manifest.artifact_ref)
    Error(Nil) -> Error(Nil)
  }
}

fn latest_step_artifact_ref(root: String) -> Result(String, Nil) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  case first(projected.step_attempts |> dict.values) {
    Some(projection.StepAttemptFinishedStatus(artifact_ref: artifact_ref, ..)) ->
      Ok(artifact_ref)
    _ -> Error(Nil)
  }
}

fn first(values: List(a)) -> Option(a) {
  case values {
    [] -> None
    [value, ..] -> Some(value)
  }
}

@external(erlang, "filename", "absname")
fn path_absolute(path: String) -> String
