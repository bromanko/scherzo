import gleam/dict
import gleam/option.{None, Some}
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import simplifile

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "LIV-59",
    title: "Durable checkpoints",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["workflow:workflow-alpha"],
    blocked_by: [],
    created_at: None,
    updated_at: None,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn workflow_recovery_reuses_finished_artifacts_and_interrupts_running_attempts_test() {
  let root = "test/tmp/recovery-workflow-checkpoint"
  reset_dir(root)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result("a", 0, "done", "", False, [], limits())
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "workflow-alpha",
      "a",
      1,
      artifact,
    )
  let issue_fingerprint = core.issue_fingerprint(issue())
  let folded =
    projection.fold([
      record.with_id(
        "run-started",
        1,
        record.WorkflowRunStarted(
          "run-1",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          "root/run-1",
        ),
      ),
      record.with_id(
        "a-prepared",
        2,
        record.StepAttemptPrepared(
          "run-1",
          "workflow-alpha",
          "a",
          1,
          "main",
          "root/run-1/workspaces/main/steps/a/attempt-1",
          "root/run-1",
          Some("seed"),
          Some("root/seed"),
        ),
      ),
      record.with_id(
        "a-started",
        3,
        record.StepAttemptStarted(
          "run-1",
          "workflow-alpha",
          "a",
          1,
          "workflow-step-run-1-a-a1-ca978112ca1b",
          None,
        ),
      ),
      record.with_id(
        "a-finished",
        4,
        record.StepAttemptFinished(
          "run-1",
          "workflow-alpha",
          "a",
          1,
          "completed",
          stored.ref,
          stored.sha256,
          "main",
          "root/run-1/workspaces/main/steps/a/attempt-1",
          0,
          0,
        ),
      ),
      record.with_id(
        "b-prepared",
        5,
        record.StepAttemptPrepared(
          "run-1",
          "workflow-alpha",
          "b",
          1,
          "main",
          "root/run-1/workspaces/main/steps/b/attempt-1",
          "root/run-1",
          None,
          None,
        ),
      ),
      record.with_id(
        "b-started",
        6,
        record.StepAttemptStarted(
          "run-1",
          "workflow-alpha",
          "b",
          1,
          "workflow-step-run-1-b-a1-3e23e8160039",
          None,
        ),
      ),
    ])

  let candidates = recovery.workflow_candidates(folded)
  let assert [candidate] = candidates
  let observations =
    dict.from_list([
      #(
        "run-1",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
        ),
      ),
    ])

  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      observations,
      store,
      99,
    )
  let assert [resumption] = finalized.resumptions
  let assert Ok(recovered_artifact) =
    dict.get(resumption.completed_artifacts, "a")
  assert recovered_artifact == artifact
  let assert Ok(recovered_workspace) =
    dict.get(resumption.completed_workspaces, "main")
  assert recovered_workspace.path
    == "root/run-1/workspaces/main/steps/a/attempt-1"
  assert recovered_workspace.run_root == "root/run-1"
  assert recovered_workspace.source_workspace_name == Some("seed")
  assert recovered_workspace.source_workspace_path == Some("root/seed")
  assert dict.get(resumption.next_attempt_indexes, "a") == Ok(2)
  assert dict.get(resumption.next_attempt_indexes, "b") == Ok(2)
  let assert [appended] = finalized.records_to_append
  let assert record.StepAttemptInterrupted(
    "run-1",
    "workflow-alpha",
    "b",
    1,
    "daemon_restart",
  ) = appended.body
  assert finalized.warnings == []
}

pub fn workflow_recovery_validates_failed_fatal_artifact_without_promoting_workspace_test() {
  let root = "test/tmp/recovery-workflow-failed-fatal"
  reset_dir(root)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "fatal",
      1,
      "",
      "boom",
      False,
      [],
      limits(),
    )
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-fatal",
      "workflow-alpha",
      "fatal",
      1,
      artifact,
    )
  let issue_fingerprint = core.issue_fingerprint(issue())
  let folded =
    projection.fold([
      record.with_id(
        "run-started",
        1,
        record.WorkflowRunStarted(
          "run-fatal",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          "root/run-fatal",
        ),
      ),
      record.with_id(
        "fatal-prepared",
        2,
        record.StepAttemptPrepared(
          "run-fatal",
          "workflow-alpha",
          "fatal",
          1,
          "main",
          "root/run-fatal/workspaces/main/steps/fatal/attempt-1",
          "root/run-fatal",
          None,
          None,
        ),
      ),
      record.with_id(
        "fatal-finished",
        3,
        record.StepAttemptFinished(
          "run-fatal",
          "workflow-alpha",
          "fatal",
          1,
          "failed_fatal",
          stored.ref,
          stored.sha256,
          "main",
          "root/run-fatal/workspaces/main/steps/fatal/attempt-1",
          0,
          0,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        "run-fatal",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
        ),
      ),
    ])
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates(
      folded,
      [candidate],
      observations,
      store,
      99,
    )
  let assert [resumption] = finalized.resumptions
  let assert Ok(recovered_artifact) =
    dict.get(resumption.completed_artifacts, "fatal")
  assert recovered_artifact == artifact
  assert dict.get(resumption.completed_workspaces, "main") == Error(Nil)
  assert dict.get(resumption.next_attempt_indexes, "fatal") == Ok(2)
  assert finalized.records_to_append == []
}
