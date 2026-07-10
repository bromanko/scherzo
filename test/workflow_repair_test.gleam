import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_outcome
import scherzo/workflow_repair
import simplifile
import support/test_helpers

pub fn retry_step_can_repair_interrupted_attempt_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert plan.run_id == "run-1"
  assert plan.selected_step_id == "implement"
  assert plan.failed_attempt_index == 1
  assert plan.next_attempt_index == 2

  assert has_superseded_attempt(plan.records_to_append, "apply_feedback", 1, 2)
  assert has_superseded_attempt(plan.records_to_append, "publish", 1, 2)
  assert has_superseded_candidate_attempt(
    plan.candidate.attempts,
    "implement",
    1,
    2,
  )
  assert has_superseded_candidate_attempt(
    plan.candidate.attempts,
    "validate_before_native_review",
    1,
    2,
  )
  assert has_superseded_candidate_attempt(
    plan.candidate.attempts,
    "apply_feedback",
    1,
    2,
  )
  assert has_superseded_candidate_attempt(
    plan.candidate.attempts,
    "publish",
    1,
    2,
  )
}

pub fn retry_step_total_plan_accepts_scheduled_run_without_tracker_issue_test() {
  let run_id = "schedule-nightly-20260703T000000Z"
  let projection =
    projection.fold([
      record.with_id(
        "scheduled-started",
        1,
        record.WorkflowRunStarted(
          run_id: run_id,
          workflow_id: "implementation",
          workflow_fingerprint: "workflow-fp-1",
          issue_id: "",
          issue_identifier: "nightly",
          issue_fingerprint: "",
          observed_updated_at_ms: 0,
          run_root: "test/tmp/workflow-repair/scheduled/nightly/run",
        ),
      ),
      record.with_id(
        "apply-feedback-prepared",
        2,
        record.StepAttemptPrepared(
          run_id: run_id,
          workflow_id: "implementation",
          step_id: "apply_feedback",
          attempt_index: 1,
          workspace_name: "main",
          workspace_path: "test/tmp/workflow-repair/scheduled/nightly/run/main",
          run_root: "test/tmp/workflow-repair/scheduled/nightly/run",
          source_workspace_name: None,
          source_workspace_path: None,
        ),
      ),
      record.with_id(
        "apply-feedback-started",
        3,
        record.StepAttemptStarted(
          run_id: run_id,
          workflow_id: "implementation",
          step_id: "apply_feedback",
          attempt_index: 1,
          operator_session_id: "session-scheduled",
          external_session_ref: None,
          continuation_capable: False,
        ),
      ),
      record.with_id(
        "apply-feedback-interrupted",
        4,
        record.StepAttemptInterrupted(
          run_id: run_id,
          workflow_id: "implementation",
          step_id: "apply_feedback",
          attempt_index: 1,
          reason: "daemon_shutdown",
        ),
      ),
      record.with_id(
        "workflow-interrupted",
        5,
        record.WorkflowRunInterrupted(
          run_id: run_id,
          workflow_id: "implementation",
          issue_id: "",
          reason: "daemon_shutdown",
        ),
      ),
    ])
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId(run_id),
      Some("apply_feedback"),
      current_scheduled_workflow(dag),
    )

  assert plan.issue_id == ""
  assert plan.issue_identifier == "nightly"
  assert plan.candidate.task_ref.task_remote_id == ""
  assert plan.selected_step_id == "implement"

  let assert Ok(exact_plan) =
    workflow_repair.plan_exact(
      projection,
      command.RetryWorkflowStepRunId(run_id),
      Some("apply_feedback"),
      current_scheduled_workflow(dag),
    )

  assert exact_plan.issue_id == ""
  assert exact_plan.issue_identifier == "nightly"
  assert exact_plan.candidate.task_ref.task_remote_id == ""
  assert exact_plan.selected_step_id == "apply_feedback"
}

pub fn retry_step_reconstructs_missing_workflow_run_provenance_test() {
  let projection = projection.fold(missing_provenance_interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert plan.run_id == "run-1"
  assert plan.issue_identifier == "LIV-509"
  assert plan.candidate.issue_fingerprint
    == tracker_issue.content_fingerprint(issue())
  assert has_provenance_repair(
    plan.records_to_append,
    "retry_step_auto",
    "workflow_run_inputs_recorded:run-1",
  )
}

pub fn retry_step_rejects_ambiguous_missing_provenance_evidence_test() {
  let projection =
    projection.fold(
      missing_provenance_interrupted_run_records_with_input_workflow(
        "other-workflow",
        "workflow-fp-1",
      ),
    )
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error)
    == "workflow_provenance_ambiguous"
}

pub fn retry_step_rejects_missing_provenance_without_run_root_test() {
  let projection =
    projection.fold(missing_provenance_without_run_root_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error)
    == "workflow_provenance_incomplete"
}

pub fn retry_step_rejects_missing_provenance_issue_identifier_drift_test() {
  let projection =
    projection.fold(
      list.append(missing_provenance_interrupted_run_records(), [
        record.with_id(
          "known-drift",
          70,
          record.KnownWorkspace(
            issue_id: "issue-1",
            issue_identifier: "LIV-OLD",
            workspace_path: "test/tmp/workflow-repair/runs/run-1",
          ),
        ),
      ]),
    )
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "issue_drift"
}

pub fn retry_step_rejects_tracker_refresh_unavailable_current_workflow_test() {
  let projection = projection.fold(interrupted_run_records())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      recovery.TrackerRefreshUnavailable,
    )

  assert workflow_repair.describe_error(error) == "tracker_refresh_unavailable"
  assert workflow_repair.error_message(error)
    == Some("tracker refresh is unavailable")
}

pub fn retry_step_interrupted_run_carries_step_recovery_evidence_test() {
  let projection = projection.fold(interrupted_run_records_with_step_recovery())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert plan.candidate.recovery_evidence
    == workflow_outcome.StepRecoveryRecheckRequested
}

pub fn retry_step_accepts_legacy_stateful_issue_fingerprint_when_equivalent_test() {
  let projection =
    projection.fold(
      interrupted_run_records_with_issue_fingerprint(
        legacy_stateful_issue_fingerprint(issue()),
      ),
    )
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert plan.run_id == "run-1"
  assert plan.selected_step_id == "implement"
}

pub fn retry_step_issue_target_selects_latest_repairable_run_by_status_time_test() {
  let projection = projection.fold(latest_repairable_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_boundary_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepIssueRef(command.IssueIdentifier("LIV-509")),
      Some("first"),
      current_workflow(dag),
    )

  assert plan.run_id == "run-2"
  assert plan.selected_step_id == "first"

  let assert Ok(id_plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepIssueRef(command.IssueId("issue-1")),
      Some("first"),
      current_workflow(dag),
    )
  assert id_plan.run_id == "run-2"

  let assert Ok(auto_plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepAutoTarget("LIV-509"),
      Some("first"),
      current_workflow(dag),
    )
  assert auto_plan.run_id == "run-2"
}

pub fn retry_step_accepts_failed_after_recovery_run_test() {
  let projection = projection.fold(failed_after_recovery_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_boundary_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )

  assert plan.run_id == "run-1"
  assert plan.selected_step_id == "first"
  assert plan.failed_attempt_index == 1
}

pub fn retry_step_issue_target_accepts_failed_after_recovery_run_test() {
  let projection = projection.fold(failed_after_recovery_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_boundary_workflow_yaml())

  let assert Ok(identifier_plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepIssueRef(command.IssueIdentifier("LIV-509")),
      Some("first"),
      current_workflow(dag),
    )
  assert identifier_plan.run_id == "run-1"
  assert identifier_plan.selected_step_id == "first"

  let assert Ok(id_plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepIssueRef(command.IssueId("issue-1")),
      Some("first"),
      current_workflow(dag),
    )
  assert id_plan.run_id == "run-1"

  let assert Ok(auto_plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepAutoTarget("LIV-509"),
      Some("first"),
      current_workflow(dag),
    )
  assert auto_plan.run_id == "run-1"
}

pub fn retry_step_normalizes_terminal_failed_stale_agent_attempt_test() {
  let projection = projection.fold(terminal_failed_stale_agent_run_records())
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert plan.selected_step_id == "seed"
  assert plan.failed_attempt_index == 1
  assert plan.next_attempt_index == 2
  assert has_normalization_interruption(
    plan.records_to_append,
    "apply_feedback",
    1,
    "terminal_failure_repair_normalized",
  )
  assert normalization_precedes_repair_request(plan.records_to_append)
  assert has_superseded_attempt(plan.records_to_append, "apply_feedback", 1, 2)
  assert has_superseded_candidate_attempt(
    plan.candidate.attempts,
    "apply_feedback",
    1,
    2,
  )
}

pub fn retry_step_auto_selects_single_terminal_failed_stale_agent_test() {
  let projection = projection.fold(terminal_failed_stale_agent_run_records())
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      None,
      current_workflow(dag),
    )

  assert plan.selected_step_id == "seed"
  assert plan.failed_attempt_index == 1
  assert plan.next_attempt_index == 2
  assert has_normalization_interruption(
    plan.records_to_append,
    "apply_feedback",
    1,
    "terminal_failure_repair_normalized",
  )
  assert normalization_precedes_repair_request(plan.records_to_append)
}

pub fn retry_step_requires_step_when_terminal_failed_run_has_multiple_stale_agents_test() {
  let projection =
    projection.fold(terminal_failed_stale_multi_agent_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_stale_agent_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      None,
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "ambiguous_repair_step"
}

pub fn retry_step_rejects_terminal_failed_stale_command_attempt_test() {
  let projection = projection.fold(terminal_failed_stale_command_run_records())
  let assert Ok(dag) = workflow_dag.parse(stale_command_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "step_not_repairable"
}

pub fn retry_step_rejects_non_failed_terminal_outcomes_test() {
  let assert Ok(dag) = workflow_dag.parse(single_step_workflow_yaml())

  let completed_projection =
    projection.fold(workflow_finished_run_records(workflow_outcome.completed))
  let assert Error(completed_error) =
    workflow_repair.plan(
      completed_projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )
  assert workflow_repair.describe_error(completed_error)
    == "no_failed_workflow_run"

  let recovered_success_projection =
    projection.fold(workflow_finished_run_records(
      workflow_outcome.succeeded_after_recovery,
    ))
  let assert Error(recovered_success_error) =
    workflow_repair.plan(
      recovered_success_projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )
  assert workflow_repair.describe_error(recovered_success_error)
    == "no_failed_workflow_run"

  let cancelled_projection =
    projection.fold(workflow_finished_run_records(workflow_outcome.cancelled))
  let assert Error(cancelled_error) =
    workflow_repair.plan(
      cancelled_projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )
  assert workflow_repair.describe_error(cancelled_error)
    == "no_failed_workflow_run"
}

pub fn retry_step_repairs_cancelled_run_with_interrupted_step_test() {
  let projection =
    projection.fold([
      base_workflow_started_record("workflow-cancelled-after-interruption"),
      prepared_attempt_record_for_run("run-1", "first", 3, "main", 10),
      workflow_finished_record_for_run("run-1", workflow_outcome.cancelled, 20),
      interrupted_attempt_record("run-1", "first", 3, 21),
    ])
  let assert Ok(dag) = workflow_dag.parse(single_step_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )

  assert plan.selected_step_id == "first"
  assert plan.failed_attempt_index == 3
  assert plan.next_attempt_index == 4
}

pub fn retry_step_selected_repeated_step_boundary_uses_latest_attempt_test() {
  let projection = projection.fold(same_step_repeated_boundary_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_boundary_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("first"),
      current_workflow(dag),
    )

  assert plan.failed_attempt_index == 2
  assert plan.next_attempt_index == 3
  assert has_superseded_attempt(plan.records_to_append, "first", 1, 2)
  assert has_superseded_attempt(plan.records_to_append, "first", 2, 3)
}

pub fn retry_step_records_original_requested_target_and_step_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepIssueRef(command.IssueIdentifier("LIV-509")),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert repair_request_matches(
    plan.records_to_append,
    "LIV-509",
    Some("apply_feedback"),
  )
}

pub fn retry_step_rejects_selected_completed_step_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("implement"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "step_not_repairable"
}

pub fn retry_step_requires_step_when_multiple_repair_boundaries_exist_test() {
  let projection = projection.fold(multiple_boundary_run_records())
  let assert Ok(dag) = workflow_dag.parse(multiple_boundary_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      None,
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "ambiguous_repair_step"
}

pub fn retry_step_rejects_selected_failed_continued_step_test() {
  assert_selected_non_repairable("failed_continued")
}

pub fn exact_retry_accepts_terminal_continued_required_output_agent_test() {
  let projection =
    projection.fold(terminal_failed_continued_review_run_records())
  let assert Ok(dag) =
    workflow_dag.parse(terminal_failed_continued_review_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan_exact(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("lane_correctness"),
      current_workflow(dag),
    )

  assert plan.selected_step_id == "lane_correctness"
  assert plan.failed_attempt_index == 1
  assert plan.next_attempt_index == 2
  assert has_superseded_attempt(
    plan.records_to_append,
    "lane_correctness",
    1,
    2,
  )
  assert list.any(plan.candidate.attempts, fn(status) {
    case status {
      projection.StepAttemptFinishedStatus(
        step_id: "lane_test_quality",
        outcome: "completed",
        ..,
      ) -> True
      _ -> False
    }
  })
}

pub fn exact_retry_rejects_terminal_continued_command_step_test() {
  let projection =
    projection.fold(terminal_failed_continued_review_run_records())
  let assert Ok(dag) =
    workflow_dag.parse(terminal_failed_continued_command_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan_exact(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("lane_correctness"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "step_not_repairable"
}

pub fn retry_step_rejects_selected_pending_step_test() {
  assert_selected_non_repairable("pending")
}

pub fn retry_step_rejects_selected_running_step_test() {
  assert_selected_non_repairable("running")
}

pub fn retry_step_rejects_selected_superseded_step_test() {
  assert_selected_non_repairable("superseded")
}

pub fn retry_step_rewinds_or_restarts_when_workflow_fingerprint_drifts_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      recovery.CurrentWorkflow(
        issue: issue(),
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-drifted",
        issue_fingerprint: tracker_issue.content_fingerprint(issue()),
        dag: dag,
        workspace_root: "test/tmp/workflow-repair",
      ),
    )

  assert list.any(plan.records_to_append, fn(body) {
    case body {
      record.WorkflowRunStartedWithTask(..) -> True
      _ -> False
    }
  })
}

pub fn retry_step_accepts_issue_fingerprint_drift_and_records_current_snapshot_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())
  let changed_issue =
    tracker_issue.Issue(..issue(), description: Some("Updated description"))
  let changed_fingerprint = tracker_issue.content_fingerprint(changed_issue)

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      recovery.CurrentWorkflow(
        issue: changed_issue,
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        issue_fingerprint: changed_fingerprint,
        dag: dag,
        workspace_root: "test/tmp/workflow-repair",
      ),
    )

  assert plan.candidate.issue_fingerprint == changed_fingerprint
  assert list.any(plan.records_to_append, fn(body) {
    case body {
      record.WorkflowRunStartedWithTask(issue_fingerprint: fingerprint, ..) ->
        fingerprint == changed_fingerprint
      _ -> False
    }
  })
}

pub fn retry_step_unparks_system_issue_content_drift_park_test() {
  let drift_reason = "issue_content_drift:issue_fingerprint_changed"
  let projection =
    projection.fold(
      list.append(
        interrupted_run_records_with_issue_fingerprint_and_reason(
          tracker_issue.content_fingerprint(issue()),
          drift_reason,
        ),
        [
          record.with_id(
            "issue-content-drift-parked",
            70,
            record.IssueParkedV2(
              "issue-1",
              "LIV-509",
              drift_reason,
              "explicit_unpark_only",
              tracker_issue.content_fingerprint(issue()),
              100,
            ),
          ),
        ],
      ),
    )
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert list.any(plan.records_to_append, fn(body) {
    case body {
      record.IssueUnparked(issue_id, _, reason) ->
        issue_id == "issue-1" && reason == "retry_step"
      _ -> False
    }
  })
}

pub fn retry_step_rejects_task_identity_drift_test() {
  let projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())
  let drifted_issue = tracker_issue.Issue(..issue(), identifier: "LIV-510")

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      recovery.CurrentWorkflow(
        issue: drifted_issue,
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        issue_fingerprint: tracker_issue.content_fingerprint(drifted_issue),
        dag: dag,
        workspace_root: "test/tmp/workflow-repair",
      ),
    )

  assert workflow_repair.describe_error(error) == "issue_drift"
}

pub fn retry_step_is_idempotent_after_selected_boundary_is_superseded_test() {
  let initial_projection = projection.fold(interrupted_run_records())
  let assert Ok(dag) = workflow_dag.parse(interrupted_workflow_yaml())
  let assert Ok(plan) =
    workflow_repair.plan(
      initial_projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  let repaired_projection =
    projection.fold(append_record_bodies(
      interrupted_run_records(),
      plan.records_to_append,
    ))

  let assert Error(error) =
    workflow_repair.plan(
      repaired_projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "no_failed_workflow_run"
}

pub fn retry_step_finalization_rewinds_before_missing_upstream_artifact_test() {
  let root = "test/tmp/workflow-repair-missing-artifact"
  let run_root = recovery_run_root(root)
  let projection =
    projection.fold(recovery_ready_run_records(
      run_root,
      "runs/run-1/seed/attempt-1.json",
      "seed-sha",
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())
  test_helpers.reset_dir(root)
  ensure_directory(run_root <> "/workspaces/seed")

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_recovery_workflow(dag, root),
    )

  let assert Ok(finalized) = finalize_repair_plan(plan, projection, dag, root)

  assert list.length(finalized.resumptions) == 1
  assert finalized.records_to_append == []
}

pub fn retry_step_finalization_rewinds_before_corrupt_upstream_artifact_test() {
  let root = "test/tmp/workflow-repair-corrupt-artifact"
  let run_root = recovery_run_root(root)
  test_helpers.reset_dir(root)
  ensure_directory(run_root <> "/workspaces/seed")
  let store = artifact_store.new(root)
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      command_artifact("seed", "ok"),
    )
  let projection =
    projection.fold(recovery_ready_run_records(
      run_root,
      stored.ref,
      "wrong-sha",
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_recovery_workflow(dag, root),
    )

  let assert Ok(finalized) = finalize_repair_plan(plan, projection, dag, root)

  assert list.length(finalized.resumptions) == 1
  assert finalized.records_to_append == []
}

pub fn retry_step_finalization_rewinds_before_unreadable_upstream_artifact_test() {
  let root = "test/tmp/workflow-repair-unreadable-artifact"
  let run_root = recovery_run_root(root)
  let artifact_ref = "runs/run-1/seed/attempt-1.json"
  test_helpers.reset_dir(root)
  ensure_directory(run_root <> "/workspaces/seed")
  let projection =
    projection.fold(recovery_ready_run_records(
      run_root,
      artifact_ref,
      "seed-sha",
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_recovery_workflow(dag, root),
    )

  let assert Ok(finalized) =
    finalize_repair_plan_with_store(
      plan,
      projection,
      dag,
      root,
      unreadable_artifact_store(),
    )

  assert list.length(finalized.resumptions) == 1
  assert finalized.records_to_append == []
}

pub fn retry_step_finalization_rewinds_before_invalid_upstream_artifact_json_test() {
  let root = "test/tmp/workflow-repair-invalid-artifact-json"
  let run_root = recovery_run_root(root)
  let artifact_ref = "runs/run-1/seed/attempt-1.json"
  let invalid_json = "{not valid step artifact json"
  test_helpers.reset_dir(root)
  ensure_directory(run_root <> "/workspaces/seed")
  ensure_artifact_parent(root, artifact_ref)
  let assert Ok(Nil) =
    simplifile.write(artifact_path_for_root(root, artifact_ref), invalid_json)
  let projection =
    projection.fold(recovery_ready_run_records(
      run_root,
      artifact_ref,
      hash.sha256_hex(invalid_json),
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_recovery_workflow(dag, root),
    )

  let assert Ok(finalized) = finalize_repair_plan(plan, projection, dag, root)

  assert list.length(finalized.resumptions) == 1
  assert finalized.records_to_append == []
}

pub fn retry_step_finalization_ignores_untrusted_local_artifact_refs_by_rewinding_test() {
  let cases = [
    #("empty", "   ", "invalid_ref", "<empty>"),
    #(
      "absolute",
      "/secret/local/attempt-1.json",
      "invalid_ref",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "home",
      "~/secret/attempt-1.json",
      "missing",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "parent",
      "../secret/attempt-1.json",
      "invalid_ref",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "nested-parent",
      "runs/run-1/../secret/attempt-1.json",
      "invalid_ref",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "file-uri",
      "file:///Users/alice/secret/attempt-1.json",
      "missing",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "windows-absolute",
      "C:\\Users\\alice\\secret\\attempt-1.json",
      "missing",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "backslash-parent",
      "runs\\..\\secret\\attempt-1.json",
      "missing",
      "<redacted-local-artifact-ref>",
    ),
    #(
      "control-character",
      "runs/run-1/seed/attempt-1.json\n/secret",
      "missing",
      "<redacted-local-artifact-ref>",
    ),
  ]

  list.each(cases, fn(entry) {
    let #(label, artifact_ref, _reason, _display_ref) = entry
    let root = "test/tmp/workflow-repair-redacted-artifact-ref-" <> label
    let run_root = recovery_run_root(root)
    test_helpers.reset_dir(root)
    ensure_directory(run_root <> "/workspaces/seed")
    let projection =
      projection.fold(recovery_ready_run_records(
        run_root,
        artifact_ref,
        "seed-sha",
      ))
    let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())
    let assert Ok(plan) =
      workflow_repair.plan(
        projection,
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
        current_recovery_workflow(dag, root),
      )
    let assert Ok(finalized) = finalize_repair_plan(plan, projection, dag, root)

    assert list.length(finalized.resumptions) == 1
    assert finalized.records_to_append == []
  })
}

pub fn retry_step_finalization_rejects_missing_workspace_test() {
  let root = "test/tmp/workflow-repair-missing-workspace"
  let run_root = recovery_run_root(root)
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      command_artifact("seed", "ok"),
    )
  let projection =
    projection.fold(recovery_ready_run_records(
      run_root,
      stored.ref,
      stored.sha256,
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      current_recovery_workflow(dag, root),
    )

  let assert Ok(finalized) = finalize_repair_plan(plan, projection, dag, root)

  assert finalized.resumptions == []
  assert has_park_reason(
    finalized.records_to_append,
    "workspace_recovery_failed",
  )
}

pub fn retry_step_finalization_accepts_guarded_recovery_then_rejects_later_corruption_test() {
  let root = "test/tmp/workflow-repair-guarded-recovery"
  let run_root = recovery_run_root(root)
  test_helpers.reset_dir(root)
  ensure_directory(run_root <> "/workspaces/seed")
  let store = artifact_store.new(root)
  let assert Ok(seed_artifact) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      command_artifact("seed", "ok"),
    )
  let assert Ok(_failed_artifact) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "apply_feedback",
      1,
      command_artifact("apply_feedback", "failed"),
    )
  let projection =
    projection.fold(guarded_failed_after_recovery_run_records(
      run_root,
      seed_artifact.ref,
      seed_artifact.sha256,
    ))
  let assert Ok(dag) = workflow_dag.parse(recovery_ready_workflow_yaml())
  let observation = current_recovery_workflow(dag, root)

  let assert Ok(plan) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("apply_feedback"),
      observation,
    )
  let assert Ok(finalized) =
    recovery.finalize_workflow_candidates_with_config(
      projection,
      [plan.candidate],
      dict.from_list([#(plan.run_id, observation)]),
      store,
      99,
      recovery_effective_config(root),
    )

  assert list.length(finalized.resumptions) == 1
  assert finalized.records_to_append == []

  let assert Ok(Nil) =
    simplifile.write(
      artifact_path_for_root(root, seed_artifact.ref),
      "corrupted after guard completed",
    )
  let assert Ok(corrupt_finalized) =
    recovery.finalize_workflow_candidates_with_config(
      projection,
      [plan.candidate],
      dict.from_list([#(plan.run_id, observation)]),
      store,
      100,
      recovery_effective_config(root),
    )

  assert corrupt_finalized.resumptions == []
  assert has_park_reason(
    corrupt_finalized.records_to_append,
    "artifact_recovery_failed",
  )
}

fn assert_selected_non_repairable(status: String) {
  let projection = projection.fold(selected_non_repairable_run_records(status))
  let assert Ok(dag) =
    workflow_dag.parse(selected_non_repairable_workflow_yaml())

  let assert Error(error) =
    workflow_repair.plan(
      projection,
      command.RetryWorkflowStepRunId("run-1"),
      Some("selected"),
      current_workflow(dag),
    )

  assert workflow_repair.describe_error(error) == "step_not_repairable"
}

fn interrupted_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: implement
    kind: command
    run: implement
    run_in: main
  - id: validate_before_native_review
    kind: command
    depends_on: [implement]
    run: validate
    on_failure: continue
    run_in: main
  - id: code_review
    kind: command
    depends_on: [implement]
    run: review
    run_in: review
  - id: apply_feedback
    kind: command
    depends_on: [validate_before_native_review, code_review]
    run: apply
    run_in: main
  - id: publish
    kind: command
    depends_on: [apply_feedback]
    run: publish
    run_in: main
"
}

fn multiple_boundary_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: first
    kind: command
    run: first
    run_in: main
  - id: second
    kind: command
    run: second
    run_in: review
  - id: finish
    kind: command
    depends_on: [first, second]
    run: finish
    run_in: main
"
}

fn single_step_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: first
    kind: command
    run: first
    run_in: main
"
}

fn selected_non_repairable_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: repairable
    kind: command
    run: repairable
    run_in: main
  - id: selected
    kind: command
    run: selected
    run_in: review
  - id: finish
    kind: command
    depends_on: [repairable, selected]
    run: finish
    run_in: main
"
}

fn terminal_failed_continued_review_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: lane_correctness
    kind: agent
    prompt: prompts/review.md
    on_failure: continue
    run_in: review-correctness
    structured_output:
      source:
        type: pi_tool_call
        tool_name: submit_review_lane_draft
      artifact_name: correctness_submission
      required: true
      format: json
      schema:
        type: object
        required: [findings]
  - id: lane_test_quality
    kind: command
    run: review-tests
    run_in: review-test-quality
  - id: finalize_lanes
    kind: command
    depends_on: [lane_correctness, lane_test_quality]
    run: finalize
    run_in: main
"
}

fn terminal_failed_continued_command_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: lane_correctness
    kind: command
    run: review
    on_failure: continue
    run_in: review-correctness
  - id: lane_test_quality
    kind: command
    run: review-tests
    run_in: review-test-quality
  - id: finalize_lanes
    kind: command
    depends_on: [lane_correctness, lane_test_quality]
    run: finalize
    run_in: main
"
}

fn recovery_ready_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: seed
    kind: command
    run: seed
    run_in: seed
  - id: apply_feedback
    kind: agent
    prompt: prompts/task.md
    depends_on: [seed]
    run_in:
      name: derived
      from: seed
"
}

fn multiple_stale_agent_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: first_agent
    kind: agent
    prompt: prompts/first.md
    run_in: main
  - id: second_agent
    kind: agent
    prompt: prompts/second.md
    run_in: review
  - id: finish
    kind: command
    depends_on: [first_agent, second_agent]
    run: finish
    run_in: main
"
}

fn stale_command_workflow_yaml() -> String {
  "version: 1
id: implementation
steps:
  - id: apply_feedback
    kind: command
    run: apply
    run_in: main
"
}

fn current_workflow(
  dag: workflow_dag.WorkflowDag,
) -> recovery.CurrentWorkflowObservation {
  let issue = issue()
  recovery.CurrentWorkflow(
    issue: issue,
    workflow_id: "implementation",
    workflow_fingerprint: "workflow-fp-1",
    issue_fingerprint: tracker_issue.content_fingerprint(issue),
    dag: dag,
    workspace_root: "test/tmp/workflow-repair",
  )
}

fn current_scheduled_workflow(
  dag: workflow_dag.WorkflowDag,
) -> recovery.CurrentWorkflowObservation {
  let issue =
    tracker_issue.Issue(
      id: "",
      identifier: "nightly",
      title: "Scheduled job nightly",
      description: None,
      priority: None,
      state: issue_state.from_string_unchecked("scheduled"),
      branch_name: None,
      url: None,
      labels: [],
      blocked_by: [],
      blocked_by_complete: True,
      created_at: None,
      updated_at: None,
    )
  recovery.CurrentWorkflow(
    issue: issue,
    workflow_id: "implementation",
    workflow_fingerprint: "workflow-fp-1",
    issue_fingerprint: "",
    dag: dag,
    workspace_root: "test/tmp/workflow-repair",
  )
}

fn current_recovery_workflow(
  dag: workflow_dag.WorkflowDag,
  root: String,
) -> recovery.CurrentWorkflowObservation {
  let issue = issue()
  recovery.CurrentWorkflow(
    issue: issue,
    workflow_id: "implementation",
    workflow_fingerprint: "workflow-fp-1",
    issue_fingerprint: tracker_issue.content_fingerprint(issue),
    dag: dag,
    workspace_root: root,
  )
}

fn recovery_effective_config(root: String) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      ..config.default_tracker_config(),
      project_slug: Some("TEST"),
      task_scope: None,
      active_states: issue_state.list_from_strings(["Todo"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: root),
    control: config.default_control_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn artifact_path_for_root(root: String, ref: String) -> String {
  root <> "/.scherzo-state/artifacts/" <> ref
}

fn interrupted_run_records() -> List(record.LedgerRecord) {
  interrupted_run_records_with_issue_fingerprint(
    tracker_issue.content_fingerprint(issue()),
  )
}

fn missing_provenance_interrupted_run_records() -> List(record.LedgerRecord) {
  case interrupted_run_records() {
    [_started, ..rest] -> rest
    [] -> []
  }
}

fn missing_provenance_interrupted_run_records_with_input_workflow(
  workflow_id: String,
  workflow_fingerprint: String,
) -> List(record.LedgerRecord) {
  case missing_provenance_interrupted_run_records() {
    [_inputs, ..rest] -> [
      record.with_id(
        "inputs-drift",
        2,
        record.WorkflowRunInputsRecorded(
          run_id: "run-1",
          workflow_id: workflow_id,
          workflow_fingerprint: workflow_fingerprint,
          artifact_ref: "runs/run-1/inputs.json",
          artifact_sha256: "sha-inputs",
          artifact_bytes: 10,
        ),
      ),
      ..rest
    ]
    [] -> []
  }
}

fn missing_provenance_without_run_root_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "inputs",
      2,
      record.WorkflowRunInputsRecorded(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        artifact_ref: "runs/run-1/inputs.json",
        artifact_sha256: "sha-inputs",
        artifact_bytes: 10,
      ),
    ),
    record.with_id(
      "apply-feedback-interrupted",
      42,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        reason: "daemon_shutdown",
      ),
    ),
    workflow_interrupted_record(60),
  ]
}

fn interrupted_run_records_with_step_recovery() -> List(record.LedgerRecord) {
  list.append(interrupted_run_records(), [
    step_recovery_started_record("run-1", "validate_before_native_review", 25),
    step_recovery_finished_record(
      "run-1",
      "validate_before_native_review",
      26,
      "recheck",
      Some(2),
    ),
  ])
}

fn interrupted_run_records_with_issue_fingerprint(
  issue_fingerprint: String,
) -> List(record.LedgerRecord) {
  interrupted_run_records_with_issue_fingerprint_and_reason(
    issue_fingerprint,
    "daemon_shutdown",
  )
}

fn interrupted_run_records_with_issue_fingerprint_and_reason(
  issue_fingerprint: String,
  interruption_reason: String,
) -> List(record.LedgerRecord) {
  [
    workflow_started_record_for_run_with_issue_fingerprint(
      "run-1",
      "run-started",
      100,
      issue_fingerprint,
    ),
    record.with_id(
      "inputs",
      2,
      record.WorkflowRunInputsRecorded(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        artifact_ref: "runs/run-1/inputs.json",
        artifact_sha256: "sha-inputs",
        artifact_bytes: 10,
      ),
    ),
    finished_attempt_record("implement", 1, "completed", "main", 10),
    finished_attempt_record(
      "validate_before_native_review",
      1,
      "failed_continued",
      "main",
      20,
    ),
    finished_attempt_record("code_review", 1, "completed", "review", 30),
    prepared_attempt_record("apply_feedback", 1, "main", 40),
    record.with_id(
      "apply-feedback-started",
      41,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-apply-feedback-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "apply-feedback-interrupted",
      42,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        reason: interruption_reason,
      ),
    ),
    prepared_attempt_record("publish", 1, "main", 50),
    record.with_id(
      "workflow-interrupted",
      60,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: "issue-1",
        reason: interruption_reason,
      ),
    ),
  ]
}

fn multiple_boundary_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-multiple"),
    prepared_attempt_record_for_run("run-1", "first", 1, "main", 10),
    interrupted_attempt_record("run-1", "first", 1, 11),
    prepared_attempt_record_for_run("run-1", "second", 1, "review", 20),
    interrupted_attempt_record("run-1", "second", 1, 21),
    workflow_interrupted_record(30),
  ]
}

fn latest_repairable_run_records() -> List(record.LedgerRecord) {
  [
    workflow_started_record_for_run("run-1", "workflow-run-1", 100),
    prepared_attempt_record_for_run("run-1", "first", 1, "main", 10),
    interrupted_attempt_record("run-1", "first", 1, 11),
    workflow_interrupted_record_for_run("run-1", 20),
    workflow_started_record_for_run("run-2", "workflow-run-2", 100),
    prepared_attempt_record_for_run("run-2", "first", 1, "main", 30),
    interrupted_attempt_record("run-2", "first", 1, 31),
    workflow_interrupted_record_for_run("run-2", 50),
  ]
}

fn failed_after_recovery_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-failed-after-recovery"),
    finished_attempt_record_for_run(
      "run-1",
      "first",
      1,
      workflow_outcome.failed_fatal,
      "main",
      10,
    ),
    workflow_finished_record_for_run(
      "run-1",
      workflow_outcome.failed_after_recovery,
      20,
    ),
  ]
}

fn terminal_failed_stale_agent_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-terminal-stale-agent"),
    finished_attempt_record_for_run(
      "run-1",
      "seed",
      1,
      workflow_outcome.completed,
      "seed",
      10,
    ),
    prepared_attempt_record_for_run("run-1", "apply_feedback", 1, "derived", 20),
    record.with_id(
      "apply-feedback-started",
      21,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-apply-feedback-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    workflow_finished_record_for_run("run-1", workflow_outcome.failed_fatal, 30),
  ]
}

fn terminal_failed_stale_multi_agent_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-terminal-stale-multi-agent"),
    prepared_attempt_record_for_run("run-1", "first_agent", 1, "main", 10),
    record.with_id(
      "first-agent-started",
      11,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "first_agent",
        attempt_index: 1,
        operator_session_id: "session-first-agent-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    prepared_attempt_record_for_run("run-1", "second_agent", 1, "review", 20),
    record.with_id(
      "second-agent-started",
      21,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "second_agent",
        attempt_index: 1,
        operator_session_id: "session-second-agent-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    workflow_finished_record_for_run("run-1", workflow_outcome.failed_fatal, 30),
  ]
}

fn terminal_failed_stale_command_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-terminal-stale-command"),
    prepared_attempt_record_for_run("run-1", "apply_feedback", 1, "main", 10),
    record.with_id(
      "apply-feedback-command-started",
      11,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-apply-feedback-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    workflow_finished_record_for_run("run-1", workflow_outcome.failed_fatal, 20),
  ]
}

fn workflow_finished_run_records(outcome: String) -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-finished-run"),
    finished_attempt_record_for_run(
      "run-1",
      "first",
      1,
      workflow_outcome.completed,
      "main",
      10,
    ),
    workflow_finished_record_for_run("run-1", outcome, 20),
  ]
}

fn same_step_repeated_boundary_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-repeated-step"),
    prepared_attempt_record_for_run("run-1", "first", 1, "main", 10),
    interrupted_attempt_record("run-1", "first", 1, 11),
    prepared_attempt_record_for_run("run-1", "first", 2, "main", 20),
    interrupted_attempt_record("run-1", "first", 2, 21),
    workflow_interrupted_record(30),
  ]
}

fn terminal_failed_continued_review_run_records() -> List(record.LedgerRecord) {
  [
    base_workflow_started_record("workflow-terminal-continued-review"),
    finished_attempt_record_for_run(
      "run-1",
      "lane_correctness",
      1,
      "failed_continued",
      "review-correctness",
      10,
    ),
    finished_attempt_record_for_run(
      "run-1",
      "lane_test_quality",
      1,
      "completed",
      "review-test-quality",
      11,
    ),
    finished_attempt_record_for_run(
      "run-1",
      "finalize_lanes",
      1,
      workflow_outcome.failed_fatal,
      "main",
      20,
    ),
    workflow_finished_record_for_run("run-1", workflow_outcome.failed_fatal, 30),
  ]
}

fn selected_non_repairable_run_records(
  status: String,
) -> List(record.LedgerRecord) {
  let selected_records = case status {
    "failed_continued" -> [
      finished_attempt_record_for_run(
        "run-1",
        "selected",
        1,
        "failed_continued",
        "review",
        20,
      ),
    ]
    "pending" -> [
      prepared_attempt_record_for_run("run-1", "selected", 1, "review", 20),
    ]
    "running" -> [
      prepared_attempt_record_for_run("run-1", "selected", 1, "review", 20),
      record.with_id(
        "selected-started",
        21,
        record.StepAttemptStarted(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "selected",
          attempt_index: 1,
          operator_session_id: "session-selected-1",
          external_session_ref: None,
          continuation_capable: False,
        ),
      ),
    ]
    _ -> [
      prepared_attempt_record_for_run("run-1", "selected", 1, "review", 20),
      record.with_id(
        "selected-superseded",
        21,
        record.StepAttemptSuperseded(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "selected",
          attempt_index: 1,
          superseded_by_attempt_index: 2,
          reason: "retry_accepted",
        ),
      ),
    ]
  }

  list.append(
    [
      base_workflow_started_record("workflow-selected"),
      prepared_attempt_record_for_run("run-1", "repairable", 1, "main", 10),
      interrupted_attempt_record("run-1", "repairable", 1, 11),
    ],
    list.append(selected_records, [workflow_interrupted_record(30)]),
  )
}

fn recovery_ready_run_records(
  run_root: String,
  seed_artifact_ref: String,
  seed_sha256: String,
) -> List(record.LedgerRecord) {
  [
    record.with_id(
      "run-started",
      1,
      record.WorkflowRunStartedWithTask(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        issue_id: "issue-1",
        issue_identifier: "LIV-509",
        task_ref: record.linear_task_ref_fields(
          "issue-1",
          Some("LIV-509"),
          None,
        ),
        issue_fingerprint: tracker_issue.content_fingerprint(issue()),
        observed_updated_at_ms: 100,
        run_root: run_root,
      ),
    ),
    record.with_id(
      "seed-prepared",
      2,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: run_root <> "/workspaces/seed",
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: seed_artifact_ref,
        artifact_sha256: seed_sha256,
        workspace_name: "seed",
        workspace_path: run_root <> "/workspaces/seed",
        token_total: 1,
        turns: 1,
      ),
    ),
    record.with_id(
      "apply-feedback-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        workspace_name: "derived",
        workspace_path: run_root <> "/workspaces/derived",
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(run_root <> "/workspaces/seed"),
      ),
    ),
    record.with_id(
      "apply-feedback-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-apply-feedback-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "apply-feedback-interrupted",
      7,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        reason: "daemon_shutdown",
      ),
    ),
    workflow_interrupted_record(8),
  ]
}

fn guarded_failed_after_recovery_run_records(
  run_root: String,
  seed_artifact_ref: String,
  seed_sha256: String,
) -> List(record.LedgerRecord) {
  [
    record.with_id(
      "run-started",
      1,
      record.WorkflowRunStartedWithTask(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "workflow-fp-1",
        issue_id: "issue-1",
        issue_identifier: "LIV-509",
        task_ref: record.linear_task_ref_fields(
          "issue-1",
          Some("LIV-509"),
          None,
        ),
        issue_fingerprint: tracker_issue.content_fingerprint(issue()),
        observed_updated_at_ms: 100,
        run_root: run_root,
      ),
    ),
    record.with_id(
      "seed-prepared",
      2,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: run_root <> "/workspaces/seed",
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: seed_artifact_ref,
        artifact_sha256: seed_sha256,
        workspace_name: "seed",
        workspace_path: run_root <> "/workspaces/seed",
        token_total: 1,
        turns: 1,
      ),
    ),
    record.with_id(
      "apply-feedback-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        workspace_name: "derived",
        workspace_path: run_root <> "/workspaces/derived",
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(run_root <> "/workspaces/seed"),
      ),
    ),
    record.with_id(
      "apply-feedback-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-apply-feedback-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "apply-feedback-finished",
      7,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        outcome: "failed_fatal",
        artifact_ref: "runs/run-1/apply_feedback/attempt-1.json",
        artifact_sha256: "failed-attempt-sha",
        workspace_name: "derived",
        workspace_path: run_root <> "/workspaces/derived",
        token_total: 1,
        turns: 1,
      ),
    ),
    step_recovery_started_record("run-1", "apply_feedback", 8),
    record.with_id(
      "step-recovery-finished-run-1-apply_feedback",
      9,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recovery-session-1",
        result: "gave_up",
        summary: "not fixable",
        reason: "needs human help; protected_checkpoint_restored kind=step_attempt_artifact ref="
          <> seed_artifact_ref,
        retry_attempt_index: None,
      ),
    ),
    workflow_finished_record_for_run(
      "run-1",
      workflow_outcome.failed_after_recovery,
      10,
    ),
  ]
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "LIV-509",
    title: "Interrupted retained run",
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

fn legacy_stateful_issue_fingerprint(issue: tracker_issue.Issue) -> String {
  [
    encode_test_string(issue.id),
    encode_test_string(issue.identifier),
    encode_test_string(issue.title),
    "none",
    "none",
    encode_test_string(issue_state.to_string(issue.state)),
    "none",
    encode_test_string("true"),
    "",
  ]
  |> string.join(with: "|")
}

fn encode_test_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

fn finished_attempt_record(
  step_id: String,
  attempt_index: Int,
  outcome: String,
  workspace_name: String,
  at_ms: Int,
) -> record.LedgerRecord {
  finished_attempt_record_for_run(
    "run-1",
    step_id,
    attempt_index,
    outcome,
    workspace_name,
    at_ms,
  )
}

fn finished_attempt_record_for_run(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  outcome: String,
  workspace_name: String,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    step_id <> "-finished-" <> int.to_string(attempt_index),
    at_ms,
    record.StepAttemptFinished(
      run_id: run_id,
      workflow_id: "implementation",
      step_id: step_id,
      attempt_index: attempt_index,
      outcome: outcome,
      artifact_ref: "runs/"
        <> run_id
        <> "/"
        <> step_id
        <> "/attempt-"
        <> int.to_string(attempt_index)
        <> ".json",
      artifact_sha256: "sha-" <> step_id,
      workspace_name: workspace_name,
      workspace_path: workspace_path_for_run(run_id, workspace_name),
      token_total: 1,
      turns: 1,
    ),
  )
}

fn prepared_attempt_record(
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  at_ms: Int,
) -> record.LedgerRecord {
  prepared_attempt_record_for_run(
    "run-1",
    step_id,
    attempt_index,
    workspace_name,
    at_ms,
  )
}

fn prepared_attempt_record_for_run(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    step_id <> "-prepared-" <> int.to_string(attempt_index),
    at_ms,
    record.StepAttemptPrepared(
      run_id: run_id,
      workflow_id: "implementation",
      step_id: step_id,
      attempt_index: attempt_index,
      workspace_name: workspace_name,
      workspace_path: workspace_path_for_run(run_id, workspace_name),
      run_root: "test/tmp/workflow-repair/runs/" <> run_id,
      source_workspace_name: None,
      source_workspace_path: None,
    ),
  )
}

fn interrupted_attempt_record(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    step_id <> "-interrupted-" <> int.to_string(attempt_index),
    at_ms,
    record.StepAttemptInterrupted(
      run_id: run_id,
      workflow_id: "implementation",
      step_id: step_id,
      attempt_index: attempt_index,
      reason: "daemon_shutdown",
    ),
  )
}

fn step_recovery_started_record(
  run_id: String,
  step_id: String,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    "step-recovery-started-" <> run_id <> "-" <> step_id,
    at_ms,
    record.WorkflowStepRecoveryStarted(
      run_id,
      "implementation",
      step_id,
      1,
      1,
      "recovery-session-1",
      Some("test-model"),
      "artifacts://prompt.md",
    ),
  )
}

fn step_recovery_finished_record(
  run_id: String,
  step_id: String,
  at_ms: Int,
  result: String,
  retry_attempt_index: Option(Int),
) -> record.LedgerRecord {
  record.with_id(
    "step-recovery-finished-" <> run_id <> "-" <> step_id,
    at_ms,
    record.WorkflowStepRecoveryFinished(
      run_id,
      "implementation",
      step_id,
      1,
      1,
      "recovery-session-1",
      result,
      "summary",
      "reason",
      retry_attempt_index,
    ),
  )
}

fn workflow_finished_record_for_run(
  run_id: String,
  outcome: String,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    "workflow-finished-" <> run_id <> "-" <> int.to_string(at_ms),
    at_ms,
    record.WorkflowRunFinished(
      run_id: run_id,
      workflow_id: "implementation",
      issue_id: "issue-1",
      outcome: outcome,
      token_total: 1,
      turns: 1,
    ),
  )
}

fn workflow_interrupted_record(at_ms: Int) -> record.LedgerRecord {
  workflow_interrupted_record_for_run("run-1", at_ms)
}

fn workflow_interrupted_record_for_run(
  run_id: String,
  at_ms: Int,
) -> record.LedgerRecord {
  record.with_id(
    "workflow-interrupted-" <> run_id <> "-" <> int.to_string(at_ms),
    at_ms,
    record.WorkflowRunInterrupted(
      run_id: run_id,
      workflow_id: "implementation",
      issue_id: "issue-1",
      reason: "daemon_shutdown",
    ),
  )
}

fn base_workflow_started_record(record_id: String) -> record.LedgerRecord {
  workflow_started_record_for_run("run-1", record_id, 100)
}

fn workflow_started_record_for_run(
  run_id: String,
  record_id: String,
  observed_updated_at_ms: Int,
) -> record.LedgerRecord {
  workflow_started_record_for_run_with_issue_fingerprint(
    run_id,
    record_id,
    observed_updated_at_ms,
    tracker_issue.content_fingerprint(issue()),
  )
}

fn workflow_started_record_for_run_with_issue_fingerprint(
  run_id: String,
  record_id: String,
  observed_updated_at_ms: Int,
  issue_fingerprint: String,
) -> record.LedgerRecord {
  record.with_id(
    record_id,
    1,
    record.WorkflowRunStartedWithTask(
      run_id: run_id,
      workflow_id: "implementation",
      workflow_fingerprint: "workflow-fp-1",
      issue_id: "issue-1",
      issue_identifier: "LIV-509",
      task_ref: record.linear_task_ref_fields("issue-1", Some("LIV-509"), None),
      issue_fingerprint: issue_fingerprint,
      observed_updated_at_ms: observed_updated_at_ms,
      run_root: "test/tmp/workflow-repair/runs/" <> run_id,
    ),
  )
}

fn workspace_path_for_run(run_id: String, workspace_name: String) -> String {
  "test/tmp/workflow-repair/runs/" <> run_id <> "/workspaces/" <> workspace_name
}

fn append_record_bodies(
  existing: List(record.LedgerRecord),
  bodies: List(record.RecordBody),
) -> List(record.LedgerRecord) {
  append_record_bodies_loop(existing, bodies, list.length(existing) + 1)
}

fn append_record_bodies_loop(
  existing: List(record.LedgerRecord),
  bodies: List(record.RecordBody),
  sequence: Int,
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> existing
    [body, ..rest] ->
      append_record_bodies_loop(
        list.append(existing, [
          record.with_id(
            "repair-" <> int.to_string(sequence),
            1000 + sequence,
            body,
          ),
        ]),
        rest,
        sequence + 1,
      )
  }
}

fn finalize_repair_plan(
  plan: workflow_repair.RepairPlan,
  projection_state: projection.Projection,
  dag: workflow_dag.WorkflowDag,
  root: String,
) -> Result(recovery.WorkflowFinalization, recovery.RecoveryError) {
  finalize_repair_plan_with_store(
    plan,
    projection_state,
    dag,
    root,
    artifact_store.new(root),
  )
}

fn finalize_repair_plan_with_store(
  plan: workflow_repair.RepairPlan,
  projection_state: projection.Projection,
  dag: workflow_dag.WorkflowDag,
  root: String,
  store: artifact_store.Store,
) -> Result(recovery.WorkflowFinalization, recovery.RecoveryError) {
  recovery.finalize_workflow_candidates(
    projection_state,
    [plan.candidate],
    dict.from_list([
      #(plan.run_id, current_recovery_workflow(dag, root)),
    ]),
    store,
    99,
  )
}

fn recovery_run_root(root: String) -> String {
  root <> "/runs/run-1"
}

fn ensure_directory(path: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn ensure_artifact_parent(root: String, ref: String) -> Nil {
  let assert Ok(parent) = path.dirname(artifact_path_for_root(root, ref))
  ensure_directory(parent)
}

fn command_artifact(
  step_id: String,
  stdout: String,
) -> step_artifact.StepArtifact {
  step_artifact.from_command_result(
    step_id,
    0,
    stdout,
    "",
    False,
    [],
    artifact_limits(),
  )
}

fn unreadable_artifact_store() -> artifact_store.Store {
  artifact_store.custom(
    "unreadable-test",
    artifact_store.StoreCallbacks(
      write: fn(_, _) { Ok(Nil) },
      read: fn(_) {
        Error(artifact_store.ArtifactIo("permission denied: /secret/local/path"))
      },
      write_bytes: fn(_, _) { Ok(Nil) },
      write_immutable_bytes: fn(_, _) { Ok(artifact_store.ImmutableWritten) },
      read_bytes: fn(_) {
        Error(artifact_store.ArtifactIo("permission denied: /secret/local/path"))
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://test/" <> ref,
          display_path: ".scherzo-state/artifacts/" <> ref,
          local_path: None,
        ))
      },
    ),
  )
}

fn artifact_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn has_provenance_repair(
  records: List(record.RecordBody),
  repair_mode: String,
  evidence: String,
) -> Bool {
  list.any(records, fn(body) {
    case body {
      record.WorkflowRunProvenanceRepaired(
        repair_mode: body_repair_mode,
        source_evidence: source_evidence,
        ..,
      ) ->
        body_repair_mode == repair_mode
        && list.contains(source_evidence, evidence)
      _ -> False
    }
  })
}

fn has_superseded_attempt(
  records: List(record.RecordBody),
  step_id: String,
  attempt_index: Int,
  superseded_by_attempt_index: Int,
) -> Bool {
  list.any(records, fn(body) {
    case body {
      record.StepAttemptSuperseded(
        step_id: body_step_id,
        attempt_index: body_attempt_index,
        superseded_by_attempt_index: body_superseded_by_attempt_index,
        ..,
      ) ->
        body_step_id == step_id
        && body_attempt_index == attempt_index
        && body_superseded_by_attempt_index == superseded_by_attempt_index
      _ -> False
    }
  })
}

fn has_superseded_candidate_attempt(
  attempts: List(projection.StepAttemptStatus),
  step_id: String,
  attempt_index: Int,
  superseded_by_attempt_index: Int,
) -> Bool {
  list.any(attempts, fn(status) {
    case status {
      projection.StepAttemptSupersededStatus(
        step_id: body_step_id,
        attempt_index: body_attempt_index,
        superseded_by_attempt_index: body_superseded_by_attempt_index,
        ..,
      ) ->
        body_step_id == step_id
        && body_attempt_index == attempt_index
        && body_superseded_by_attempt_index == superseded_by_attempt_index
      _ -> False
    }
  })
}

fn has_normalization_interruption(
  records: List(record.RecordBody),
  step_id: String,
  attempt_index: Int,
  reason: String,
) -> Bool {
  list.any(records, fn(body) {
    case body {
      record.StepAttemptInterrupted(
        step_id: body_step_id,
        attempt_index: body_attempt_index,
        reason: body_reason,
        ..,
      ) ->
        body_step_id == step_id
        && body_attempt_index == attempt_index
        && body_reason == reason
      _ -> False
    }
  })
}

fn normalization_precedes_repair_request(
  records: List(record.RecordBody),
) -> Bool {
  case records {
    [
      record.StepAttemptInterrupted(
        reason: "terminal_failure_repair_normalized",
        ..,
      ),
      record.WorkflowRepairRequested(..),
      ..
    ] -> True
    _ -> False
  }
}

fn repair_request_matches(
  records: List(record.RecordBody),
  requested_target: String,
  requested_step_id: Option(String),
) -> Bool {
  list.any(records, fn(body) {
    case body {
      record.WorkflowRepairRequested(
        requested_target: body_requested_target,
        requested_step_id: body_requested_step_id,
        ..,
      ) ->
        body_requested_target == requested_target
        && body_requested_step_id == requested_step_id
      _ -> False
    }
  })
}

fn has_park_reason(
  records: List(record.LedgerRecord),
  expected_prefix: String,
) -> Bool {
  list.any(records, fn(ledger_record) {
    case ledger_record.body {
      record.IssueParkedV2(reason: reason, ..) ->
        string.starts_with(reason, expected_prefix)
      _ -> False
    }
  })
}
