import gleam/dict
import gleam/option.{None}
import gleam/string
import scherzo/state/projection
import scherzo/state/record

pub fn workflow_records_roundtrip_test() {
  let records = [
    record.with_id(
      "workflow-started",
      1,
      record.WorkflowRunStarted(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        workflow_fingerprint: "wf-sha",
        issue_id: "issue-1",
        issue_identifier: "LIV-59",
        issue_fingerprint: "issue-sha",
        observed_updated_at_ms: 123,
        run_root: "workspace-root/workflow-alpha/LIV-59/run-001",
      ),
    ),
    record.with_id(
      "workflow-finished",
      2,
      record.WorkflowRunFinished(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        issue_id: "issue-1",
        outcome: "completed",
        token_total: 42,
        turns: 3,
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      3,
      record.WorkflowRunInterrupted(
        run_id: "run-002",
        workflow_id: "workflow-alpha",
        issue_id: "issue-1",
        reason: "daemon_restart",
      ),
    ),
    record.with_id(
      "workflow-superseded",
      4,
      record.WorkflowRunSuperseded(
        run_id: "run-003",
        workflow_id: "workflow-alpha",
        issue_id: "issue-1",
        superseded_by_run_id: "run-004",
        reason: "issue_changed",
      ),
    ),
    record.with_id(
      "step-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        step_id: "build",
        attempt_index: 2,
        workspace_name: "main",
        workspace_path: "workspace-root/workflow-alpha/LIV-59/run-001/workspaces/main/steps/build-abcdef123456/attempt-2",
        run_root: "workspace-root/workflow-alpha/LIV-59/run-001",
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "step-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        step_id: "build",
        attempt_index: 2,
        operator_session_id: "workflow-step-run-001-build-a2-abcdef123456",
        external_session_ref: None,
      ),
    ),
    record.with_id(
      "step-finished",
      7,
      record.StepAttemptFinished(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        step_id: "build",
        attempt_index: 2,
        outcome: "completed",
        artifact_ref: "runs/run-001/build-abcdef123456/attempt-2.json",
        artifact_sha256: "artifact-sha",
        workspace_name: "main",
        workspace_path: "workspace-root/workflow-alpha/LIV-59/run-001/workspaces/main/steps/build-abcdef123456/attempt-2",
        token_total: 5,
        turns: 1,
      ),
    ),
    record.with_id(
      "step-interrupted",
      8,
      record.StepAttemptInterrupted(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        step_id: "test",
        attempt_index: 1,
        reason: "daemon_restart",
      ),
    ),
    record.with_id(
      "step-superseded",
      9,
      record.StepAttemptSuperseded(
        run_id: "run-001",
        workflow_id: "workflow-alpha",
        step_id: "test",
        attempt_index: 1,
        superseded_by_attempt_index: 2,
        reason: "retry",
      ),
    ),
  ]

  list_each(records, fn(ledger_record) {
    let assert Ok(decoded) =
      record.decode_string(record.to_string(ledger_record))
    assert decoded == ledger_record
  })
  assert string.contains(
    record.to_string(list_first(records)),
    "workflow_run_started",
  )
}

pub fn projection_tracks_step_attempts_and_completed_workspaces_test() {
  let folded =
    projection.fold([
      record.with_id(
        "run-start",
        1,
        record.WorkflowRunStarted(
          "run-1",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          "issue-sha",
          0,
          "root/run-1",
        ),
      ),
      record.with_id(
        "prepared",
        2,
        record.StepAttemptPrepared(
          "run-1",
          "workflow-alpha",
          "build",
          1,
          "main",
          "root/run-1/workspaces/main/steps/build/attempt-1",
          "root/run-1",
          None,
          None,
        ),
      ),
      record.with_id(
        "started",
        3,
        record.StepAttemptStarted(
          "run-1",
          "workflow-alpha",
          "build",
          1,
          "workflow-step-run-1-build-a1-abc",
          None,
        ),
      ),
      record.with_id(
        "finished",
        4,
        record.StepAttemptFinished(
          "run-1",
          "workflow-alpha",
          "build",
          1,
          "failed_continued",
          "runs/run-1/build/attempt-1.json",
          "sha",
          "main",
          "root/run-1/workspaces/main/steps/build/attempt-1",
          0,
          0,
        ),
      ),
    ])

  assert projection.next_attempt_index(folded, "run-1", "build") == 2
  let assert Ok(workspace) =
    projection.latest_completed_workspace(folded, "run-1", "main")
  assert workspace.path == "root/run-1/workspaces/main/steps/build/attempt-1"
  let assert Ok(status) =
    dict.get(
      folded.step_attempts,
      projection.step_attempt_key("run-1", "build", 1),
    )
  assert projection.dependency_satisfying_attempt(status)
}

pub fn version_one_record_is_rejected_test() {
  let line =
    "{\"schema_version\":1,\"record_id\":\"old\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"LIV-59\",\"workspace_path\":\"work\"}"
  let assert Error(record.UnsupportedVersion(1)) = record.decode_string(line)
}

fn list_each(values: List(a), f: fn(a) -> Nil) -> Nil {
  case values {
    [] -> Nil
    [value, ..rest] -> {
      f(value)
      list_each(rest, f)
    }
  }
}

fn list_first(values: List(a)) -> a {
  let assert [first, ..] = values
  first
}
