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
import scherzo/workflow_dag
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
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn parse_dag(content: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(content)
  dag
}

fn agent_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: a\n    kind: agent\n    prompt: a\n    workspace: main\n  - id: b\n    kind: agent\n    depends_on: [a]\n    prompt: b\n    workspace: main\n",
  )
}

fn fatal_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: fatal\n    kind: command\n    run: fatal\n    workspace: main\n",
  )
}

fn interrupted_command_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: command\n    kind: command\n    run: make changes\n    workspace: main\n",
  )
}

fn source_dag() -> workflow_dag.WorkflowDag {
  parse_dag(
    "version: 1\nid: workflow-alpha\nsteps:\n  - id: seed\n    kind: agent\n    prompt: seed\n    workspace: seed\n  - id: use_seed\n    kind: agent\n    depends_on: [seed]\n    prompt: use\n    workspace:\n      name: derived\n      from: seed\n",
  )
}

pub fn workflow_recovery_reuses_finished_artifacts_and_interrupts_running_attempts_test() {
  let root = "test/tmp/recovery-workflow-checkpoint"
  reset_dir(root)
  let run_root = root <> "/workflow-alpha/LIV-59/run-1"
  let main_workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(main_workspace)
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
          run_root,
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
          main_workspace,
          run_root,
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
          main_workspace,
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
          main_workspace,
          run_root,
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
          agent_dag(),
          root,
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
  assert recovered_workspace.path == main_workspace
  assert recovered_workspace.run_root == run_root
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

pub fn workflow_recovery_missing_artifact_parks_without_resumption_test() {
  let root = "test/tmp/recovery-workflow-missing-artifact"
  reset_dir(root)
  let store = artifact_store.new(root)
  let issue_fingerprint = core.issue_fingerprint(issue())
  let folded =
    projection.fold([
      record.with_id(
        "run-started",
        1,
        record.WorkflowRunStarted(
          "run-missing",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          "root/run-missing",
        ),
      ),
      record.with_id(
        "a-prepared",
        2,
        record.StepAttemptPrepared(
          "run-missing",
          "workflow-alpha",
          "a",
          1,
          "main",
          "root/run-missing/workspaces/main",
          "root/run-missing",
          None,
          None,
        ),
      ),
      record.with_id(
        "a-finished",
        3,
        record.StepAttemptFinished(
          "run-missing",
          "workflow-alpha",
          "a",
          1,
          "completed",
          "runs/run-missing/a/attempt-1.json",
          "missing-sha",
          "main",
          "root/run-missing/workspaces/main",
          0,
          0,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        "run-missing",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
          agent_dag(),
          root,
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

  assert finalized.resumptions == []
  assert has_park_reason(
    finalized.records_to_append,
    "artifact_recovery_failed",
  )
  assert has_workflow_interrupted(finalized.records_to_append, "run-missing")
}

pub fn workflow_recovery_disabled_mode_parks_resumable_run_test() {
  let root = "test/tmp/recovery-workflow-disabled"
  reset_dir(root)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result("a", 0, "done", "", False, [], limits())
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-disabled",
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
          "run-disabled",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          "root/run-disabled",
        ),
      ),
      record.with_id(
        "a-prepared",
        2,
        record.StepAttemptPrepared(
          "run-disabled",
          "workflow-alpha",
          "a",
          1,
          "main",
          "root/run-disabled/workspaces/main",
          "root/run-disabled",
          None,
          None,
        ),
      ),
      record.with_id(
        "a-finished",
        3,
        record.StepAttemptFinished(
          "run-disabled",
          "workflow-alpha",
          "a",
          1,
          "completed",
          stored.ref,
          stored.sha256,
          "main",
          "root/run-disabled/workspaces/main",
          0,
          0,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        "run-disabled",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
          agent_dag(),
          root,
        ),
      ),
    ])
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_mode(
      folded,
      [candidate],
      observations,
      store,
      99,
      recovery.ParkRecoveredWorkflows,
    )

  assert finalized.resumptions == []
  assert has_park_reason(
    finalized.records_to_append,
    "workflow_recovery_disabled",
  )
}

pub fn workflow_recovery_parks_interrupted_command_attempts_test() {
  let root = "test/tmp/recovery-workflow-interrupted-command"
  reset_dir(root)
  let run_root = root <> "/workflow-alpha/LIV-59/run-command"
  let command_workspace = run_root <> "/workspaces/main"
  let store = artifact_store.new(root)
  let issue_fingerprint = core.issue_fingerprint(issue())
  let folded =
    projection.fold([
      record.with_id(
        "run-started",
        1,
        record.WorkflowRunStarted(
          "run-command",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          run_root,
        ),
      ),
      record.with_id(
        "command-prepared",
        2,
        record.StepAttemptPrepared(
          "run-command",
          "workflow-alpha",
          "command",
          1,
          "main",
          command_workspace,
          run_root,
          None,
          None,
        ),
      ),
      record.with_id(
        "command-started",
        3,
        record.StepAttemptStarted(
          "run-command",
          "workflow-alpha",
          "command",
          1,
          "workflow-step-run-command-command-a1-abc",
          None,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        "run-command",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
          interrupted_command_dag(),
          root,
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

  assert finalized.resumptions == []
  assert has_park_reason(
    finalized.records_to_append,
    "unsafe_interrupted_command_step",
  )
  assert has_workflow_interrupted(finalized.records_to_append, "run-command")
}

pub fn workflow_recovery_parks_missing_recovered_source_workspace_test() {
  let root = "test/tmp/recovery-workflow-missing-source-workspace"
  reset_dir(root)
  let run_root = root <> "/workflow-alpha/LIV-59/run-source"
  let seed_workspace = run_root <> "/workspaces/seed"
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "seed",
      0,
      "done",
      "",
      False,
      [],
      limits(),
    )
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-source",
      "workflow-alpha",
      "seed",
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
          "run-source",
          "workflow-alpha",
          "wf-sha",
          "issue-1",
          "LIV-59",
          issue_fingerprint,
          0,
          run_root,
        ),
      ),
      record.with_id(
        "seed-prepared",
        2,
        record.StepAttemptPrepared(
          "run-source",
          "workflow-alpha",
          "seed",
          1,
          "seed",
          seed_workspace,
          run_root,
          None,
          None,
        ),
      ),
      record.with_id(
        "seed-finished",
        3,
        record.StepAttemptFinished(
          "run-source",
          "workflow-alpha",
          "seed",
          1,
          "completed",
          stored.ref,
          stored.sha256,
          "seed",
          seed_workspace,
          0,
          0,
        ),
      ),
    ])

  let assert [candidate] = recovery.workflow_candidates(folded)
  let observations =
    dict.from_list([
      #(
        "run-source",
        recovery.CurrentWorkflow(
          issue(),
          "workflow-alpha",
          "wf-sha",
          issue_fingerprint,
          source_dag(),
          root,
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

  assert finalized.resumptions == []
  assert has_park_reason(
    finalized.records_to_append,
    "workspace_recovery_failed",
  )
  assert has_workflow_interrupted(finalized.records_to_append, "run-source")
}

pub fn workflow_recovery_validates_failed_fatal_artifact_without_promoting_workspace_test() {
  let root = "test/tmp/recovery-workflow-failed-fatal"
  reset_dir(root)
  let run_root = root <> "/workflow-alpha/LIV-59/run-fatal"
  let main_workspace = run_root <> "/workspaces/main"
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
          run_root,
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
          main_workspace,
          run_root,
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
          main_workspace,
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
          fatal_dag(),
          root,
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

fn has_park_reason(records: List(record.LedgerRecord), reason: String) -> Bool {
  case records {
    [] -> False
    [record, ..rest] ->
      case record.body {
        record.IssueParkedV2(reason: parked_reason, ..) ->
          parked_reason == reason || has_park_reason(rest, reason)
        _ -> has_park_reason(rest, reason)
      }
  }
}

fn has_workflow_interrupted(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  case records {
    [] -> False
    [record, ..rest] ->
      case record.body {
        record.WorkflowRunInterrupted(run_id: status_run_id, ..) ->
          status_run_id == run_id || has_workflow_interrupted(rest, run_id)
        _ -> has_workflow_interrupted(rest, run_id)
      }
  }
}
