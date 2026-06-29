import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/hash
import scherzo/orchestrator/core
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_output_recollection
import scherzo/workflow_run/contract_io
import simplifile
import support/test_helpers

pub fn recollect_outputs_writes_recollection_namespace_test() {
  let root = "test/tmp/workflow-output-recollection/success"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let artifact = successful_artifact("collect_findings", "findings")
  let result =
    run_recollection(
      root,
      dag,
      issue,
      [step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)],
      None,
    )

  let assert Ok(workflow_output_recollection.Recollected(recorded, manifest)) =
    result
  assert recorded.ref == "runs/run-1/recollections/1/outputs.v1.json"
  let assert [output] = manifest.outputs
  let assert Some(ref) = output.value.ref
  assert ref == "runs/run-1/recollections/1/outputs/findings.md"
}

pub fn recollect_outputs_is_idempotent_when_latest_manifest_is_valid_test() {
  let root = "test/tmp/workflow-output-recollection/already-valid"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let recollection =
    workflow_checkpoint.recollection_ledger_writer(root, fn() { 10 }, 1)
  let store = artifact_store.new(root)
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)
  let assert Ok(outputs) =
    contract_io.record_outputs_if_contracted(
      dag,
      "run-1",
      "wf-1",
      None,
      checkpoint,
      dict.from_list([#("collect_findings", artifact)]),
      dict.new(),
    )
  let assert Some(recorded) = outputs.recorded

  let result =
    workflow_output_recollection.execute(
      projection_with_run(
        workflow_dag.id(dag),
        issue,
        [attempt.status],
        Some(recorded),
        "wf-1",
      ),
      "run-1",
      observation(issue, dag, root),
      checkpoint,
      recollection,
      store,
    )

  let assert Ok(workflow_output_recollection.AlreadyValid(existing)) = result
  assert existing.ref == "runs/run-1/outputs.v1.json"
}

pub fn recollect_outputs_uses_latest_valid_manifest_before_recovering_sources_test() {
  let root =
    "test/tmp/workflow-output-recollection/already-valid-missing-source"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let recollection =
    workflow_checkpoint.recollection_ledger_writer(root, fn() { 10 }, 1)
  let store = artifact_store.new(root)
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)
  let assert Ok(outputs) =
    contract_io.record_outputs_if_contracted(
      dag,
      "run-1",
      "wf-1",
      None,
      checkpoint,
      dict.from_list([#("collect_findings", artifact)]),
      dict.new(),
    )
  let assert Some(recorded) = outputs.recorded
  let assert Ok(Nil) =
    delete_artifact(root, status_artifact_ref(attempt.status))

  let result =
    workflow_output_recollection.execute(
      projection_with_run(
        workflow_dag.id(dag),
        issue,
        [attempt.status],
        Some(recorded),
        "wf-1",
      ),
      "run-1",
      observation(issue, dag, root),
      checkpoint,
      recollection,
      store,
    )

  let assert Ok(workflow_output_recollection.AlreadyValid(existing)) = result
  assert existing.ref == "runs/run-1/outputs.v1.json"
}

pub fn recollect_outputs_writes_next_recollection_namespace_after_prior_record_test() {
  let root = "test/tmp/workflow-output-recollection/recollection-index-2"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)
  let assert Ok(outputs) =
    contract_io.record_outputs_if_contracted(
      dag,
      "run-1",
      "wf-1",
      None,
      checkpoint,
      dict.from_list([#("collect_findings", artifact)]),
      dict.new(),
    )
  let assert Some(recorded) = outputs.recorded
  let assert Ok(Nil) = delete_artifact(root, recorded.ref)

  let result =
    execute_with_attempts_and_recollection_index(
      root,
      dag,
      issue,
      [attempt],
      Some(recorded),
      observation(issue, dag, root),
      "wf-1",
      2,
    )

  let assert Ok(workflow_output_recollection.Recollected(new_recorded, manifest)) =
    result
  assert new_recorded.ref == "runs/run-1/recollections/2/outputs.v1.json"
  let assert [output] = manifest.outputs
  let assert Some(ref) = output.value.ref
  assert ref == "runs/run-1/recollections/2/outputs/findings.md"
}

pub fn recollect_outputs_rejects_unknown_run_test() {
  let root = "test/tmp/workflow-output-recollection/unknown-run"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let recollection =
    workflow_checkpoint.recollection_ledger_writer(root, fn() { 10 }, 1)
  let store = artifact_store.new(root)

  let result =
    workflow_output_recollection.execute(
      projection.new(),
      "run-1",
      observation(issue, dag, root),
      checkpoint,
      recollection,
      store,
    )

  assert_error_code(result, "run_not_found")
}

pub fn recollect_outputs_rejects_workflow_drift_test() {
  let root = "test/tmp/workflow-output-recollection/workflow-drift"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)
  let changed = renamed_stdout_dag()

  let result =
    execute_with_attempts(
      root,
      dag,
      issue,
      [attempt],
      None,
      observation(issue, changed, root),
      "wf-1",
    )

  assert_error_code(result, "workflow_drift")
}

pub fn recollect_outputs_rejects_issue_drift_test() {
  let root = "test/tmp/workflow-output-recollection/issue-drift"
  test_helpers.reset_dir(root)
  let dag = stdout_dag()
  let original_issue = issue()
  let current_issue = tracker_issue.Issue(..original_issue, id: "issue-2")
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)

  let result =
    execute_with_attempts(
      root,
      dag,
      original_issue,
      [attempt],
      None,
      observation(current_issue, dag, root),
      "wf-1",
    )

  assert_error_code(result, "issue_drift")
}

pub fn recollect_outputs_rejects_issue_unavailable_current_workflow_test() {
  let root = "test/tmp/workflow-output-recollection/issue-unavailable"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)

  let result =
    execute_with_attempts(
      root,
      dag,
      issue,
      [attempt],
      None,
      recovery.IssueUnavailable,
      "wf-1",
    )

  assert_error_code(result, "issue_unavailable")
  assert_error_message_contains(result, "issue is unavailable")
}

pub fn recollect_outputs_rejects_tracker_refresh_unavailable_current_workflow_test() {
  let root = "test/tmp/workflow-output-recollection/tracker-refresh-unavailable"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)

  let result =
    execute_with_attempts(
      root,
      dag,
      issue,
      [attempt],
      None,
      recovery.TrackerRefreshUnavailable,
      "wf-1",
    )

  assert_error_code(result, "tracker_refresh_unavailable")
  assert_error_message_contains(result, "tracker refresh is unavailable")
}

pub fn recollect_outputs_rejects_workflow_unavailable_current_workflow_test() {
  let root = "test/tmp/workflow-output-recollection/workflow-unavailable"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let artifact = successful_artifact("collect_findings", "findings")
  let attempt =
    step_attempt(root, workflow_dag.id(dag), "collect_findings", artifact)

  let result =
    execute_with_attempts(
      root,
      dag,
      issue,
      [attempt],
      None,
      recovery.WorkflowUnavailable("unknown_workflow_label: missing"),
      "wf-1",
    )

  assert_error_code(result, "workflow_unavailable")
  assert_error_message_contains(
    result,
    "workflow is unavailable: unknown_workflow_label: missing",
  )
}

pub fn recollect_outputs_requires_every_workflow_step_completed_test() {
  let root = "test/tmp/workflow-output-recollection/run-not-complete"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = two_step_stdout_dag()
  let seed =
    step_attempt(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "findings"),
    )
  let pending =
    projection.StepAttemptPending(
      run_id: "run-1",
      workflow_id: workflow_dag.id(dag),
      step_id: "finalize",
      attempt_index: 1,
      workspace_name: "main",
      workspace_path: root <> "/workspace/finalize",
      run_root: root,
      source_workspace_name: None,
      source_workspace_path: None,
      prepared_at_ms: 10,
    )

  let result =
    run_recollection_with_statuses(
      root,
      dag,
      issue,
      [seed.status, pending],
      None,
    )

  assert_error_code(result, "run_not_complete")
}

pub fn recollect_outputs_rejects_failed_source_step_test() {
  let root = "test/tmp/workflow-output-recollection/source-step-failed"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let failed =
    step_attempt_with(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      failed_artifact("collect_findings"),
      "failed_fatal",
      root <> "/workspace/collect_findings",
    )

  let result =
    run_recollection_with_statuses(root, dag, issue, [failed.status], None)

  assert_error_code(result, "source_step_failed")
}

pub fn recollect_outputs_rejects_latest_failed_attempt_after_completed_attempt_test() {
  let root = "test/tmp/workflow-output-recollection/latest-failed-attempt"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let first =
    step_attempt_with_index(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "stale findings"),
      "completed",
      root <> "/workspace/collect_findings/attempt-1",
      1,
    )
  let latest =
    step_attempt_with_index(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      failed_artifact("collect_findings"),
      "failed_fatal",
      root <> "/workspace/collect_findings/attempt-2",
      2,
    )

  let result =
    run_recollection_with_statuses(
      root,
      dag,
      issue,
      [latest.status, first.status],
      None,
    )

  assert_error_code(result, "source_step_failed")
}

pub fn recollect_outputs_uses_latest_completed_attempt_artifact_test() {
  let root = "test/tmp/workflow-output-recollection/latest-completed-attempt"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let first =
    step_attempt_with_index(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "stale findings"),
      "completed",
      root <> "/workspace/collect_findings/attempt-1",
      1,
    )
  let latest =
    step_attempt_with_index(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "latest findings"),
      "completed",
      root <> "/workspace/collect_findings/attempt-2",
      2,
    )

  let result = run_recollection(root, dag, issue, [latest, first], None)

  let assert Ok(workflow_output_recollection.Recollected(recorded, _)) = result
  assert recorded.ref == "runs/run-1/recollections/1/outputs.v1.json"
  let assert Ok(contents) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/recollections/1/outputs/findings.md",
    )
  assert contents == "latest findings"
}

pub fn recollect_outputs_rejects_missing_artifact_test() {
  let root = "test/tmp/workflow-output-recollection/missing-artifact"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let attempt =
    StepAttempt(status: finished_status(
      workflow_dag.id(dag),
      "collect_findings",
      "runs/run-1/collect_findings/attempt-1.json",
      "missing-sha",
      root <> "/workspace/collect_findings",
      "completed",
    ))

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "artifact_recovery_failed")
  assert_error_message_contains(result, "reason=missing")
}

pub fn recollect_outputs_rejects_invalid_artifact_json_test() {
  let root = "test/tmp/workflow-output-recollection/invalid-artifact-json"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let corrupted_contents = "not json"
  let corrupted_ref =
    artifact_store.artifact_ref("run-1", "collect_findings", 1)
  let assert Ok(Nil) =
    artifact_store.restore_filesystem_artifact_bytes(
      root,
      corrupted_ref,
      bit_array.from_string(corrupted_contents),
    )
  let attempt =
    StepAttempt(status: finished_status(
      workflow_dag.id(dag),
      "collect_findings",
      corrupted_ref,
      hash_of(corrupted_contents),
      root <> "/workspace/collect_findings",
      "completed",
    ))

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "artifact_recovery_failed")
  assert_error_message_contains(result, "reason=invalid_json")
}

pub fn recollect_outputs_rejects_artifact_sha_mismatch_test() {
  let root = "test/tmp/workflow-output-recollection/artifact-sha-mismatch"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = stdout_dag()
  let attempt =
    step_attempt(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "findings"),
    )
  let ref = status_artifact_ref(attempt.status)
  let assert Ok(Nil) =
    artifact_store.restore_filesystem_artifact_bytes(
      root,
      ref,
      bit_array.from_string("corrupted artifact"),
    )

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "artifact_recovery_failed")
  assert_error_message_contains(result, "reason=sha_mismatch")
}

pub fn recollect_outputs_rejects_missing_source_workspace_test() {
  let root = "test/tmp/workflow-output-recollection/missing-workspace"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = step_file_dag()
  let missing_workspace = root <> "/missing-workspace"
  let attempt =
    step_attempt_with(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "ignored"),
      "completed",
      missing_workspace,
    )
  let assert Ok(Nil) = simplifile.delete(missing_workspace)

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "workspace_recovery_failed")
}

pub fn recollect_outputs_rejects_invalid_output_json_test() {
  let root = "test/tmp/workflow-output-recollection/invalid-output-json"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = json_stdout_dag()
  let attempt =
    step_attempt(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", "not json"),
    )

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "workflow_output_json_invalid")
}

pub fn recollect_outputs_rejects_invalid_artifact_set_descriptor_test() {
  let root = "test/tmp/workflow-output-recollection/artifact-set-invalid"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = artifact_set_stdout_dag()
  let descriptor = missing_ref_artifact_set_descriptor()
  let attempt =
    step_attempt(
      root,
      workflow_dag.id(dag),
      "collect_findings",
      successful_artifact("collect_findings", descriptor),
    )

  let result =
    run_recollection_with_statuses(root, dag, issue, [attempt.status], None)

  assert_error_code(result, "workflow_output_artifact_set_invalid")
  assert_error_message_contains(
    result,
    "artifact_descriptor_missing_ref_artifact",
  )
}

pub fn recollect_outputs_restores_artifact_set_success_test() {
  let root = "test/tmp/workflow-output-recollection/artifact-set-restored"
  test_helpers.reset_dir(root)
  let issue = issue()
  let dag = artifact_set_stdout_dag()
  let child_ref = "runs/run-1/outputs/reference.txt"
  let assert Ok(Nil) =
    artifact_store.restore_filesystem_artifact_bytes(
      root,
      child_ref,
      bit_array.from_string("retained child\n"),
    )
  let descriptor =
    artifact_set_descriptor_with_child(child_ref, "retained child\n")
  let result =
    run_recollection(
      root,
      dag,
      issue,
      [
        step_attempt(
          root,
          workflow_dag.id(dag),
          "collect_findings",
          successful_artifact("collect_findings", descriptor),
        ),
      ],
      None,
    )

  let assert Ok(workflow_output_recollection.Recollected(recorded, manifest)) =
    result
  assert recorded.ref == "runs/run-1/recollections/1/outputs.v1.json"
  let assert [output] = manifest.outputs
  assert output.value.status == workflow_contract_manifest.Present
  let assert Some(ref) = output.value.ref
  assert ref == "runs/run-1/recollections/1/outputs/visual_bundle.json"
}

type StepAttempt {
  StepAttempt(status: projection.StepAttemptStatus)
}

fn run_recollection(
  root: String,
  dag: workflow_dag.WorkflowDag,
  issue: tracker_issue.Issue,
  attempts: List(StepAttempt),
  output_manifest: Option(workflow_checkpoint.ArtifactWritten),
) -> Result(
  workflow_output_recollection.Outcome,
  workflow_output_recollection.RecollectionError,
) {
  run_recollection_with_statuses(
    root,
    dag,
    issue,
    list.map(attempts, fn(attempt) { attempt.status }),
    output_manifest,
  )
}

fn run_recollection_with_statuses(
  root: String,
  dag: workflow_dag.WorkflowDag,
  issue: tracker_issue.Issue,
  statuses: List(projection.StepAttemptStatus),
  output_manifest: Option(workflow_checkpoint.ArtifactWritten),
) -> Result(
  workflow_output_recollection.Outcome,
  workflow_output_recollection.RecollectionError,
) {
  execute_with_attempts(
    root,
    dag,
    issue,
    list.map(statuses, fn(status) { StepAttempt(status: status) }),
    output_manifest,
    observation(issue, dag, root),
    "wf-1",
  )
}

fn execute_with_attempts(
  root: String,
  dag: workflow_dag.WorkflowDag,
  issue: tracker_issue.Issue,
  attempts: List(StepAttempt),
  output_manifest: Option(workflow_checkpoint.ArtifactWritten),
  current: recovery.CurrentWorkflowObservation,
  workflow_fingerprint: String,
) -> Result(
  workflow_output_recollection.Outcome,
  workflow_output_recollection.RecollectionError,
) {
  execute_with_attempts_and_recollection_index(
    root,
    dag,
    issue,
    attempts,
    output_manifest,
    current,
    workflow_fingerprint,
    1,
  )
}

fn execute_with_attempts_and_recollection_index(
  root: String,
  dag: workflow_dag.WorkflowDag,
  issue: tracker_issue.Issue,
  attempts: List(StepAttempt),
  output_manifest: Option(workflow_checkpoint.ArtifactWritten),
  current: recovery.CurrentWorkflowObservation,
  workflow_fingerprint: String,
  recollection_index: Int,
) -> Result(
  workflow_output_recollection.Outcome,
  workflow_output_recollection.RecollectionError,
) {
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let recollection =
    workflow_checkpoint.recollection_ledger_writer(
      root,
      fn() { 10 },
      recollection_index,
    )
  let store = artifact_store.new(root)

  workflow_output_recollection.execute(
    projection_with_run(
      workflow_dag.id(dag),
      issue,
      list.map(attempts, fn(attempt) { attempt.status }),
      output_manifest,
      workflow_fingerprint,
    ),
    "run-1",
    current,
    checkpoint,
    recollection,
    store,
  )
}

fn step_attempt(
  root: String,
  workflow_id: String,
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> StepAttempt {
  step_attempt_with(
    root,
    workflow_id,
    step_id,
    artifact,
    "completed",
    root <> "/workspace/" <> step_id,
  )
}

fn step_attempt_with(
  root: String,
  workflow_id: String,
  step_id: String,
  artifact: step_artifact.StepArtifact,
  outcome: String,
  workspace_path: String,
) -> StepAttempt {
  step_attempt_with_index(
    root,
    workflow_id,
    step_id,
    artifact,
    outcome,
    workspace_path,
    1,
  )
}

fn step_attempt_with_index(
  root: String,
  workflow_id: String,
  step_id: String,
  artifact: step_artifact.StepArtifact,
  outcome: String,
  workspace_path: String,
  attempt_index: Int,
) -> StepAttempt {
  let store = artifact_store.new(root)
  let _ = simplifile.create_directory_all(workspace_path)
  let assert Ok(written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      workflow_id,
      step_id,
      attempt_index,
      artifact,
    )
  StepAttempt(status: finished_status_with_index(
    workflow_id,
    step_id,
    written.ref,
    written.sha256,
    workspace_path,
    outcome,
    attempt_index,
  ))
}

fn finished_status(
  workflow_id: String,
  step_id: String,
  artifact_ref: String,
  artifact_sha256: String,
  workspace_path: String,
  outcome: String,
) -> projection.StepAttemptStatus {
  finished_status_with_index(
    workflow_id,
    step_id,
    artifact_ref,
    artifact_sha256,
    workspace_path,
    outcome,
    1,
  )
}

fn finished_status_with_index(
  workflow_id: String,
  step_id: String,
  artifact_ref: String,
  artifact_sha256: String,
  workspace_path: String,
  outcome: String,
  attempt_index: Int,
) -> projection.StepAttemptStatus {
  projection.StepAttemptFinishedStatus(
    run_id: "run-1",
    workflow_id: workflow_id,
    step_id: step_id,
    attempt_index: attempt_index,
    outcome: outcome,
    artifact_ref: artifact_ref,
    artifact_sha256: artifact_sha256,
    workspace_name: "main",
    workspace_path: workspace_path,
    run_root: workspace_path,
    source_workspace_name: None,
    source_workspace_path: None,
    token_total: 0,
    turns: 0,
    finished_at_ms: 10,
  )
}

fn status_artifact_ref(status: projection.StepAttemptStatus) -> String {
  case status {
    projection.StepAttemptFinishedStatus(artifact_ref: artifact_ref, ..) ->
      artifact_ref
    _ -> ""
  }
}

fn projection_with_run(
  workflow_id: String,
  issue: tracker_issue.Issue,
  step_statuses: List(projection.StepAttemptStatus),
  output_manifest: Option(workflow_checkpoint.ArtifactWritten),
  workflow_fingerprint: String,
) -> projection.Projection {
  let base = projection.new()
  let output_manifests = case output_manifest {
    Some(workflow_checkpoint.ArtifactWritten(
      ref: recorded_ref,
      sha256: recorded_sha256,
      bytes: recorded_bytes,
    )) ->
      dict.from_list([
        #(
          "run-1",
          projection.WorkflowContractManifestRef(
            workflow_id: workflow_id,
            workflow_fingerprint: workflow_fingerprint,
            artifact_ref: recorded_ref,
            artifact_sha256: recorded_sha256,
            artifact_bytes: recorded_bytes,
            recorded_at_ms: 10,
          ),
        ),
      ])
    None -> dict.new()
  }
  projection.Projection(
    ..base,
    workflow_runs: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunFinished(
          workflow_id: workflow_id,
          issue_id: issue.id,
          outcome: "failed",
          token_total: 0,
          turns: 0,
          finished_at_ms: 10,
          run_root: "test/tmp",
        ),
      ),
    ]),
    workflow_run_provenances: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunProvenance(
          workflow_id: workflow_id,
          workflow_fingerprint: workflow_fingerprint,
          issue_id: issue.id,
          issue_identifier: issue.identifier,
          issue_fingerprint: core.issue_fingerprint(issue),
          observed_updated_at_ms: 10,
          run_root: "test/tmp",
          task_ref: record.linear_task_ref_fields(
            issue.id,
            Some(issue.identifier),
            None,
          ),
        ),
      ),
    ]),
    workflow_task_refs: dict.from_list([
      #(
        "run-1",
        record.linear_task_ref_fields(issue.id, Some(issue.identifier), None),
      ),
    ]),
    workflow_output_manifests: output_manifests,
    step_attempts: step_attempt_dict(step_statuses),
  )
}

fn step_attempt_dict(
  statuses: List(projection.StepAttemptStatus),
) -> dict.Dict(String, projection.StepAttemptStatus) {
  step_attempt_dict_loop(statuses, 0, dict.new())
}

fn step_attempt_dict_loop(
  statuses: List(projection.StepAttemptStatus),
  index: Int,
  acc: dict.Dict(String, projection.StepAttemptStatus),
) -> dict.Dict(String, projection.StepAttemptStatus) {
  case statuses {
    [] -> acc
    [status, ..rest] ->
      step_attempt_dict_loop(
        rest,
        index + 1,
        dict.insert(acc, step_attempt_key(status, index), status),
      )
  }
}

fn step_attempt_key(
  status: projection.StepAttemptStatus,
  index: Int,
) -> String {
  case status {
    projection.StepAttemptPending(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    )
    | projection.StepAttemptRunning(
        run_id: run_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      )
    | projection.StepAttemptFinishedStatus(
        run_id: run_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      )
    | projection.StepAttemptInterruptedStatus(
        run_id: run_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      )
    | projection.StepAttemptSupersededStatus(
        run_id: run_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) ->
      run_id
      <> "\u{001f}"
      <> step_id
      <> "\u{001f}"
      <> int.to_string(attempt_index)
      <> "\u{001f}"
      <> int.to_string(index)
  }
}

fn observation(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  root: String,
) -> recovery.CurrentWorkflowObservation {
  recovery.CurrentWorkflow(
    issue,
    workflow_dag.id(dag),
    "wf-1",
    core.issue_fingerprint(issue),
    dag,
    root,
  )
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

fn stdout_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  dag
}

fn renamed_stdout_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: changed\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  dag
}

fn two_step_stdout_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n  - id: finalize\n    kind: command\n    run: echo finalize\n    depends_on: [collect_findings]\n",
    )
  dag
}

fn step_file_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        path: tmp/findings.md\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  dag
}

fn json_stdout_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    custom_value:\n      kind: value\n      media_type: application/json\n      artifact_type: scherzo.custom.v1\n      required: true\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  dag
}

fn artifact_set_stdout_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    visual_bundle:\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.visual_bundle.v1\n      required: true\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  dag
}

fn successful_artifact(
  step_id: String,
  stdout: String,
) -> step_artifact.StepArtifact {
  step_artifact.StepArtifact(
    step_id: step_id,
    status: step_artifact.StepSucceeded,
    final_response: None,
    exit_code: Some(0),
    command: Some("echo"),
    duration_ms: Some(1),
    diagnostic_path: None,
    failure_code: None,
    stdout: stdout,
    stderr: "",
    timed_out: False,
    final_response_truncated: False,
    stdout_truncated: False,
    stderr_truncated: False,
    summary_text: "ok",
    structured_output: None,
  )
}

fn failed_artifact(step_id: String) -> step_artifact.StepArtifact {
  step_artifact.StepArtifact(
    ..successful_artifact(step_id, "failed"),
    status: step_artifact.StepFailed,
    failure_code: Some("boom"),
  )
}

fn delete_artifact(
  root: String,
  ref: String,
) -> Result(Nil, simplifile.FileError) {
  simplifile.delete(root <> "/.scherzo-state/artifacts/" <> ref)
}

fn assert_error_code(
  result: Result(a, workflow_output_recollection.RecollectionError),
  code: String,
) -> Nil {
  let assert Error(error) = result
  assert workflow_output_recollection.describe_error(error) == code
}

fn assert_error_message_contains(
  result: Result(a, workflow_output_recollection.RecollectionError),
  expected: String,
) -> Nil {
  let assert Error(error) = result
  let assert Some(message) = workflow_output_recollection.error_message(error)
  assert string.contains(message, expected)
}

fn hash_of(contents: String) -> String {
  let bytes = bit_array.from_string(contents)
  hash.sha256_hex_bytes(bytes)
}

fn missing_ref_artifact_set_descriptor() -> String {
  artifact_set_descriptor_with_child(
    "runs/run-1/outputs/reference.txt",
    "retained child\n",
  )
}

fn artifact_set_descriptor_with_child(
  child_ref: String,
  child_contents: String,
) -> String {
  let bytes = bit_array.from_string(child_contents)
  "{\"entries\":[{\"name\":\"reference\",\"kind\":\"file\",\"ref\":\""
  <> child_ref
  <> "\",\"sha256\":\""
  <> hash.sha256_hex_bytes(bytes)
  <> "\",\"bytes\":"
  <> int.to_string(bit_array.byte_size(bytes))
  <> ",\"media_type\":\"text/plain\"}]}"
}
