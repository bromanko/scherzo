import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/step_artifact
import scherzo/workflow_dag
import scherzo/workflow_outcome
import scherzo/workflow_recovery_planner as planner
import scherzo/workflow_scheduler

// This suite intentionally exercises only the standalone pure planner contract.
// Real projection and artifact-store adapter tests belong to the later integration
// seam and must cover prepared, started, finished, interrupted, superseded,
// missing artifact, corrupt artifact, and top-level run terminal records before
// constructing planner.VerifiedArtifact values.

fn fan_in_yaml() -> String {
  "version: 1
id: review-flow
concurrency: 3
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    run_in:
      name: main
  - id: test_after_implement
    depends_on: [implement]
    kind: command
    run: gleam test
    on_failure: continue
    run_in:
      name: main
      from: main
  - id: code_review
    depends_on: [implement]
    kind: agent
    prompt: prompts/code-review.md
    run_in:
      name: review
      from: main
  - id: security_review
    depends_on: [implement]
    kind: agent
    prompt: prompts/security-review.md
    run_in:
      name: security
      from: main
  - id: apply_feedback
    depends_on: [test_after_implement, code_review, security_review]
    kind: agent
    prompt: prompts/apply-feedback.md
    run_in:
      name: main
      from: main
"
}

fn independent_roots_yaml() -> String {
  "version: 1
id: parallel-roots
concurrency: 2
steps:
  - id: docs
    kind: agent
    prompt: prompts/docs.md
    run_in:
      name: docs
  - id: tests
    kind: command
    run: gleam test
    run_in:
      name: test
  - id: final
    depends_on: [docs, tests]
    kind: agent
    prompt: prompts/final.md
    run_in:
      name: final
"
}

fn same_workspace_roots_yaml() -> String {
  "version: 1
id: same-workspace-roots
concurrency: 2
steps:
  - id: docs
    kind: agent
    prompt: prompts/docs.md
    run_in:
      name: main
  - id: tests
    kind: command
    run: gleam test
    run_in:
      name: main
  - id: final
    depends_on: [docs, tests]
    kind: agent
    prompt: prompts/final.md
    run_in:
      name: main
"
}

fn failure_policy_continue_with_downstream_yaml() -> String {
  "version: 1
id: review-flow
concurrency: 1
steps:
  - id: test_after_implement
    kind: command
    run: gleam test
    on_failure: continue
    run_in:
      name: main
  - id: later
    depends_on: [test_after_implement]
    kind: command
    run: later
    run_in:
      name: main
"
}

fn failure_policy_default_with_downstream_yaml() -> String {
  "version: 1
id: review-flow
concurrency: 1
steps:
  - id: test_after_implement
    kind: command
    run: gleam test
    run_in:
      name: main
  - id: later
    depends_on: [test_after_implement]
    kind: command
    run: later
    run_in:
      name: main
"
}

fn single_step_yaml() -> String {
  "version: 1
id: review-flow
concurrency: 1
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    run_in:
      name: main
"
}

fn parse_dag(yaml: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(yaml)
  dag
}

fn fan_in_dag() -> workflow_dag.WorkflowDag {
  parse_dag(fan_in_yaml())
}

fn success_artifact(
  step_id: String,
  attempt_index: Int,
) -> planner.VerifiedArtifact {
  planner.VerifiedArtifact(
    artifact_ref: artifact_ref(step_id, attempt_index),
    artifact_sha256: "sha256-" <> step_id <> "-" <> int.to_string(attempt_index),
    artifact: step_artifact.StepArtifact(
      step_id: step_id,
      status: step_artifact.StepSucceeded,
      final_response: None,
      exit_code: Some(0),
      command: Some("command " <> step_id),
      duration_ms: None,
      diagnostic_path: None,
      failure_code: None,
      stdout: "ok",
      stderr: "",
      timed_out: False,
      final_response_truncated: False,
      stdout_truncated: False,
      stderr_truncated: False,
      summary_text: "success " <> step_id,
      structured_output: None,
    ),
  )
}

fn failure_artifact(
  step_id: String,
  attempt_index: Int,
) -> planner.VerifiedArtifact {
  planner.VerifiedArtifact(
    artifact_ref: artifact_ref(step_id, attempt_index),
    artifact_sha256: "sha256-" <> step_id <> "-" <> int.to_string(attempt_index),
    artifact: step_artifact.StepArtifact(
      step_id: step_id,
      status: step_artifact.StepFailed,
      final_response: None,
      exit_code: Some(1),
      command: Some("command " <> step_id),
      duration_ms: None,
      diagnostic_path: None,
      failure_code: None,
      stdout: "",
      stderr: "failed",
      timed_out: False,
      final_response_truncated: False,
      stdout_truncated: False,
      stderr_truncated: False,
      summary_text: "failure " <> step_id,
      structured_output: None,
    ),
  )
}

fn artifact_ref(step_id: String, attempt_index: Int) -> String {
  "runs/run-1/"
  <> step_id
  <> "/attempt-"
  <> int.to_string(attempt_index)
  <> ".json"
}

fn base_run(
  step_attempts: List(planner.StepAttemptFacts),
) -> planner.WorkflowRunFacts {
  run_for("review-flow", step_attempts)
}

fn run_for(
  workflow_id: String,
  step_attempts: List(planner.StepAttemptFacts),
) -> planner.WorkflowRunFacts {
  planner.WorkflowRunFacts(
    run_id: "run-1",
    workflow_id: workflow_id,
    workflow_fingerprint: "workflow-fp-a",
    issue_id: "issue-1",
    issue_identifier: "LIV-55",
    issue_fingerprint: "issue-fp-a",
    observed_updated_at_ms: 1000,
    run_root: "test/tmp/workflow-recovery/run-1",
    cleanup_recorded: False,
    recovery_evidence: workflow_outcome.NoStepRecovery,
    run_status: planner.RunActive,
    step_attempts: step_attempts,
  )
}

fn current_ok() -> planner.CurrentWorkflowObservation {
  current_for("review-flow")
}

fn current_for(workflow_id: String) -> planner.CurrentWorkflowObservation {
  planner.CurrentWorkflowObservation(
    workflow_id: workflow_id,
    workflow_fingerprint: "workflow-fp-a",
    issue_fingerprint: "issue-fp-a",
  )
}

fn input(
  dag: workflow_dag.WorkflowDag,
  run: planner.WorkflowRunFacts,
  current: planner.CurrentWorkflowObservation,
) -> planner.PlannerInput {
  planner.PlannerInput(
    run: run,
    dag: dag,
    current: current,
    policy: planner.default_policy(),
    now_ms: 2000,
  )
}

fn plan(
  dag: workflow_dag.WorkflowDag,
  run: planner.WorkflowRunFacts,
  current: planner.CurrentWorkflowObservation,
) -> planner.RecoveryPlan {
  planner.plan_run(input(dag, run, current))
}

fn completed(step_id: String, attempt_index: Int) -> planner.StepAttemptFacts {
  finished(
    step_id,
    attempt_index,
    planner.RecordedCompleted,
    success_artifact(step_id, attempt_index),
  )
}

fn failed_continued(
  step_id: String,
  attempt_index: Int,
) -> planner.StepAttemptFacts {
  finished(
    step_id,
    attempt_index,
    planner.RecordedFailedContinued,
    failure_artifact(step_id, attempt_index),
  )
}

fn failed_fatal(
  step_id: String,
  attempt_index: Int,
) -> planner.StepAttemptFacts {
  finished(
    step_id,
    attempt_index,
    planner.RecordedFailedFatal,
    failure_artifact(step_id, attempt_index),
  )
}

fn finished(
  step_id: String,
  attempt_index: Int,
  outcome: planner.RecordedStepOutcome,
  artifact: planner.VerifiedArtifact,
) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptFinished(
      outcome: outcome,
      artifact: artifact,
      workspace_name: "main",
      workspace_path: workspace_path(step_id, attempt_index),
      token_total: 10,
      turns: 1,
    ),
  )
}

fn prepared(step_id: String, attempt_index: Int) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptPrepared(
      workspace_name: "main",
      workspace_path: workspace_path(step_id, attempt_index),
      run_root: "test/tmp/workflow-recovery/run-1",
      source_workspace_name: None,
      source_workspace_path: None,
    ),
  )
}

fn started(
  step_id: String,
  attempt_index: Int,
  external_ref: Option(String),
) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptStarted(
      workspace_name: "main",
      workspace_path: workspace_path(step_id, attempt_index),
      run_root: "test/tmp/workflow-recovery/run-1",
      operator_session_id: "session-"
        <> step_id
        <> "-"
        <> int.to_string(attempt_index),
      external_session_ref: external_ref,
    ),
  )
}

fn interrupted(
  step_id: String,
  attempt_index: Int,
) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptInterrupted("daemon_restart_during_step"),
  )
}

fn interrupted_before_start(
  step_id: String,
  attempt_index: Int,
) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptInterrupted(planner.interruption_reason_to_string(
      planner.DaemonRestartBeforeStepStart,
    )),
  )
}

fn superseded(step_id: String, attempt_index: Int) -> planner.StepAttemptFacts {
  planner.StepAttemptFacts(
    run_id: "run-1",
    workflow_id: "review-flow",
    step_id: step_id,
    attempt_index: attempt_index,
    status: planner.AttemptSuperseded(
      superseded_by_attempt_index: attempt_index + 1,
      reason: "current_issue_or_workflow_changed",
    ),
  )
}

fn workspace_path(step_id: String, attempt_index: Int) -> String {
  "test/tmp/workflow-recovery/run-1/workspaces/main/steps/"
  <> step_id
  <> "/attempt-"
  <> int.to_string(attempt_index)
}

fn state_for(
  plan: planner.RecoveryPlan,
  step_id: String,
) -> planner.StepRecoveryState {
  let assert Ok(state) = dict.get(plan.step_states, step_id)
  state
}

fn preserved_for(
  plan: planner.RecoveryPlan,
  step_id: String,
) -> planner.VerifiedArtifact {
  let assert Ok(artifact) = dict.get(plan.preserved_artifacts, step_id)
  artifact
}

fn assert_no_start_steps(plan: planner.RecoveryPlan) -> Nil {
  assert plan.start_steps == []
}

fn start_ids(plan: planner.RecoveryPlan) -> List(String) {
  list.map(plan.start_steps, fn(start) { start.step_id })
}

fn scheduler_ready_ids(
  dag: workflow_dag.WorkflowDag,
  state: workflow_scheduler.SchedulerState,
) -> List(String) {
  workflow_scheduler.ready_steps(dag, state)
  |> list.map(fn(step) { step.id })
}

fn scheduler_state_with(
  dag: workflow_dag.WorkflowDag,
  overrides: List(#(String, workflow_scheduler.StepRuntime)),
) -> workflow_scheduler.SchedulerState {
  let overrides = dict.from_list(overrides)
  let statuses =
    workflow_dag.steps(dag)
    |> list.map(fn(step) {
      let status =
        dict.get(overrides, step.id)
        |> result_unwrap(workflow_scheduler.Pending)
      #(step.id, status)
    })
    |> dict.from_list
  let failure_policies =
    workflow_dag.steps(dag)
    |> list.map(fn(step) { #(step.id, step.on_failure) })
    |> dict.from_list
  workflow_scheduler.SchedulerState(
    statuses: statuses,
    failure_policies: failure_policies,
    cancelling: False,
  )
}

fn blocked_for(
  plan: planner.RecoveryPlan,
  step_id: String,
) -> planner.BlockedStep {
  let assert Ok(blocked) =
    list.find(plan.blocked_steps, fn(blocked) { blocked.step_id == step_id })
  blocked
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

pub fn unattempted_step_is_classified_and_ready_test() {
  let dag = fan_in_dag()
  let recovery = plan(dag, base_run([]), current_ok())

  assert state_for(recovery, "implement")
    == planner.StepUnattempted("implement")
  assert start_ids(recovery) == ["implement"]
  assert recovery.outcome == planner.Continuable
}

pub fn completed_attempt_preserves_artifact_and_unblocks_ready_steps_test() {
  let dag = fan_in_dag()
  let artifact = success_artifact("implement", 1)
  let recovery =
    plan(
      dag,
      base_run([
        finished("implement", 1, planner.RecordedCompleted, artifact),
      ]),
      current_ok(),
    )

  assert state_for(recovery, "implement")
    == planner.StepCompleted("implement", 1, artifact)
  assert preserved_for(recovery, "implement") == artifact
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn failed_continued_preserves_artifact_and_satisfies_dependencies_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        failed_continued("test_after_implement", 1),
        completed("code_review", 1),
        completed("security_review", 1),
      ]),
      current_ok(),
    )

  let assert planner.StepFailedContinued(_, 1, artifact) =
    state_for(recovery, "test_after_implement")
  assert preserved_for(recovery, "test_after_implement") == artifact
  assert start_ids(recovery) == ["apply_feedback"]
}

pub fn failed_fatal_preserves_artifact_and_blocks_starts_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        failed_fatal("test_after_implement", 1),
      ]),
      current_ok(),
    )

  let assert planner.StepFailedFatal(_, 1, artifact) =
    state_for(recovery, "test_after_implement")
  assert preserved_for(recovery, "test_after_implement") == artifact
  assert_no_start_steps(recovery)
  assert recovery.workflow_finish_records
    == [
      planner.WorkflowFinishRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        issue_id: "issue-1",
        outcome: planner.WorkflowFailedFatal,
      ),
    ]
}

pub fn prepared_attempt_gets_interruption_intent_and_restarts_test() {
  let dag = fan_in_dag()
  let recovery = plan(dag, base_run([prepared("implement", 1)]), current_ok())

  let assert planner.StepNeedsInterruptionBeforeStart(
    step_id: "implement",
    attempt_index: 1,
    workspace_path: _,
  ) = state_for(recovery, "implement")
  assert recovery.interruption_records
    == [
      planner.InterruptionRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        step_id: "implement",
        attempt_index: 1,
        reason: planner.DaemonRestartBeforeStepStart,
      ),
    ]
  assert planner.interruption_reason_to_string(
      planner.DaemonRestartBeforeStepStart,
    )
    == "daemon_restart_before_step_start"
  assert start_ids(recovery) == ["implement"]
  assert recovery.inspection_requests == []
  assert recovery.park_requests == []
}

pub fn started_command_attempt_gets_interruption_inspection_and_park_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        started("test_after_implement", 1, None),
      ]),
      current_ok(),
    )

  let assert planner.StepNeedsInterruptionAfterStart(
    step_id: "test_after_implement",
    attempt_index: 1,
    workspace_path: _,
    operator_session_id: "session-test_after_implement-1",
    external_session_ref: None,
  ) = state_for(recovery, "test_after_implement")
  assert recovery.interruption_records
    == [
      planner.InterruptionRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        step_id: "test_after_implement",
        attempt_index: 1,
        reason: planner.DaemonRestartDuringStep,
      ),
    ]
  assert planner.interruption_reason_to_string(planner.DaemonRestartDuringStep)
    == "daemon_restart_during_step"
  assert list.length(recovery.inspection_requests) == 1
  assert list.length(recovery.park_requests) == 1
  assert_no_start_steps(recovery)
}

pub fn started_agent_attempt_rewinds_without_inspection_and_restarts_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        started("code_review", 1, Some("pi-session")),
      ]),
      current_ok(),
    )

  assert recovery.interruption_records
    == [
      planner.InterruptionRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        step_id: "code_review",
        attempt_index: 1,
        reason: planner.DaemonRestartDuringStep,
      ),
    ]
  assert recovery.inspection_requests == []
  assert recovery.park_requests == []
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn already_interrupted_attempt_does_not_duplicate_interruption_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([completed("implement", 1), interrupted("code_review", 1)]),
      current_ok(),
    )

  let assert planner.StepAlreadyInterrupted("code_review", 1, _) =
    state_for(recovery, "code_review")
  assert recovery.interruption_records == []
  assert recovery.inspection_requests == []
  assert recovery.park_requests == []
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn already_interrupted_command_before_start_restarts_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        interrupted_before_start("test_after_implement", 1),
      ]),
      current_ok(),
    )

  let assert planner.StepAlreadyInterrupted(
    "test_after_implement",
    1,
    "daemon_restart_before_step_start",
  ) = state_for(recovery, "test_after_implement")
  assert recovery.interruption_records == []
  assert recovery.inspection_requests == []
  assert recovery.park_requests == []
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn superseded_attempt_blocks_dependencies_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        completed("test_after_implement", 1),
        superseded("code_review", 1),
        completed("security_review", 1),
      ]),
      current_ok(),
    )

  let assert planner.StepSuperseded("code_review", 1, _) =
    state_for(recovery, "code_review")
  assert_no_start_steps(recovery)
  let blocked = blocked_for(recovery, "apply_feedback")
  assert list.contains(blocked.blockers, "code_review")
}

pub fn recorded_failed_fatal_is_not_recomputed_from_continue_policy_test() {
  let dag = parse_dag(failure_policy_continue_with_downstream_yaml())
  let recovery =
    plan(dag, base_run([failed_fatal("test_after_implement", 1)]), current_ok())

  let assert planner.StepFailedFatal("test_after_implement", 1, _) =
    state_for(recovery, "test_after_implement")
  assert_no_start_steps(recovery)
  let blocked = blocked_for(recovery, "later")
  assert list.contains(blocked.blockers, "test_after_implement")
}

pub fn recorded_failed_continued_is_not_recomputed_from_default_policy_test() {
  let dag = parse_dag(failure_policy_default_with_downstream_yaml())
  let recovery =
    plan(
      dag,
      base_run([failed_continued("test_after_implement", 1)]),
      current_ok(),
    )

  let assert planner.StepFailedContinued("test_after_implement", 1, _) =
    state_for(recovery, "test_after_implement")
  assert start_ids(recovery) == ["later"]
}

pub fn workflow_id_drift_blocks_ready_start_and_cleanup_test() {
  let dag = fan_in_dag()
  let current =
    planner.CurrentWorkflowObservation(
      workflow_id: "other-flow",
      workflow_fingerprint: "workflow-fp-a",
      issue_fingerprint: "issue-fp-a",
    )
  let recovery = plan(dag, base_run([]), current)

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.WorkflowIdMismatch("review-flow", "other-flow") -> True
      _ -> False
    }
  })
  assert recovery.outcome == planner.DriftBlocked
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []
  assert list.length(recovery.inspection_requests) == 1
}

pub fn workflow_fingerprint_drift_blocks_ready_start_and_cleanup_test() {
  let dag = fan_in_dag()
  let current =
    planner.CurrentWorkflowObservation(
      workflow_id: "review-flow",
      workflow_fingerprint: "workflow-fp-b",
      issue_fingerprint: "issue-fp-a",
    )
  let recovery = plan(dag, base_run([]), current)

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.WorkflowFingerprintDrift("workflow-fp-a", "workflow-fp-b") -> True
      _ -> False
    }
  })
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []
}

pub fn issue_fingerprint_drift_blocks_ready_start_and_cleanup_test() {
  let dag = fan_in_dag()
  let current =
    planner.CurrentWorkflowObservation(
      workflow_id: "review-flow",
      workflow_fingerprint: "workflow-fp-a",
      issue_fingerprint: "issue-fp-b",
    )
  let recovery = plan(dag, base_run([]), current)

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.IssueFingerprintDrift("issue-fp-a", "issue-fp-b") -> True
      _ -> False
    }
  })
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []
}

pub fn issue_fingerprint_drift_blocks_agent_restart_test() {
  let dag = fan_in_dag()
  let current =
    planner.CurrentWorkflowObservation(
      workflow_id: "review-flow",
      workflow_fingerprint: "workflow-fp-a",
      issue_fingerprint: "issue-fp-b",
    )
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        started("code_review", 1, Some("pi-session")),
      ]),
      current,
    )

  assert_no_start_steps(recovery)
}

pub fn issue_unavailable_blocks_ready_start_and_cleanup_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([]),
      planner.IssueUnavailable("linear_fetch_missing_issue"),
    )

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.IssueUnavailableDrift("linear_fetch_missing_issue") -> True
      _ -> False
    }
  })
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []
  assert recovery.park_requests
    == [
      planner.ParkRequest(
        issue_id: "issue-1",
        issue_identifier: "LIV-55",
        reason: planner.ParkDriftBlocked,
        release_policy: planner.park_release_policy_manual,
        issue_fingerprint: None,
      ),
    ]
}

pub fn workflow_unavailable_blocks_ready_start_and_cleanup_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(dag, base_run([]), planner.WorkflowUnavailable("workflow_not_found"))

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.WorkflowUnavailableDrift("workflow_not_found") -> True
      _ -> False
    }
  })
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []
}

pub fn unknown_step_drift_blocks_active_cleanup_but_finished_run_can_clean_test() {
  let dag = parse_dag(single_step_yaml())
  let unknown_attempt = completed("old_step", 1)
  let run = base_run([unknown_attempt])
  let current =
    planner.CurrentWorkflowObservation(
      workflow_id: "review-flow",
      workflow_fingerprint: "workflow-fp-b",
      issue_fingerprint: "issue-fp-a",
    )
  let recovery = plan(dag, run, current)

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.UnknownStepAttempt("old_step") -> True
      _ -> False
    }
  })
  assert_no_start_steps(recovery)
  assert recovery.cleanup_run_roots == []

  let finished_run =
    planner.WorkflowRunFacts(
      ..run,
      run_status: planner.RunFinished(planner.WorkflowCompleted, 10, 2),
    )
  let finished_recovery = plan(dag, finished_run, current)
  assert finished_recovery.cleanup_run_roots
    == [
      planner.CleanupRunRoot(
        "run-1",
        "issue-1",
        "test/tmp/workflow-recovery/run-1",
      ),
    ]
}

pub fn malformed_attempt_identity_blocks_starts_and_is_not_classified_test() {
  let dag = fan_in_dag()
  let bad_run_attempt =
    planner.StepAttemptFacts(..completed("implement", 1), run_id: "other-run")
  let bad_workflow_attempt =
    planner.StepAttemptFacts(
      ..completed("code_review", 1),
      workflow_id: "other-flow",
    )
  let recovery =
    plan(dag, base_run([bad_run_attempt, bad_workflow_attempt]), current_ok())

  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.AttemptRunIdMismatch(step_id: "implement", ..) -> True
      _ -> False
    }
  })
  assert list.any(recovery.drift_errors, fn(error) {
    case error {
      planner.AttemptWorkflowIdMismatch(step_id: "code_review", ..) -> True
      _ -> False
    }
  })
  assert state_for(recovery, "implement")
    == planner.StepUnattempted("implement")
  assert_no_start_steps(recovery)
}

pub fn latest_attempt_index_wins_over_older_failure_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([failed_fatal("implement", 1), completed("implement", 2)]),
      current_ok(),
    )

  let assert planner.StepCompleted("implement", 2, _) =
    state_for(recovery, "implement")
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn older_superseded_attempt_does_not_block_newer_completed_attempt_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([superseded("implement", 1), completed("implement", 2)]),
      current_ok(),
    )

  let assert planner.StepCompleted("implement", 2, _) =
    state_for(recovery, "implement")
  assert recovery.inspection_requests == []
  assert recovery.park_requests == []
  assert start_ids(recovery)
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn latest_started_attempt_overrides_older_completed_artifact_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([completed("implement", 1), started("implement", 2, None)]),
      current_ok(),
    )

  let assert planner.StepNeedsInterruptionAfterStart("implement", 2, _, _, _) =
    state_for(recovery, "implement")
  assert dict.get(recovery.preserved_artifacts, "implement") == Error(Nil)
  assert start_ids(recovery) == ["implement"]
  let blocked = blocked_for(recovery, "code_review")
  assert list.contains(blocked.blockers, "implement")
}

pub fn duplicate_attempt_index_is_deterministic_and_warns_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([started("implement", 1, None), interrupted("implement", 1)]),
      current_ok(),
    )

  let assert planner.StepAlreadyInterrupted("implement", 1, _) =
    state_for(recovery, "implement")
  assert recovery.interruption_records == []
  assert recovery.warnings == ["duplicate_step_attempt_index:implement"]
}

pub fn active_all_completed_run_emits_finish_record_not_cleanup_test() {
  let dag = parse_dag(single_step_yaml())
  let recovery = plan(dag, base_run([completed("implement", 1)]), current_ok())

  assert recovery.outcome == planner.TerminalRecordNeeded
  assert recovery.workflow_finish_records
    == [
      planner.WorkflowFinishRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        issue_id: "issue-1",
        outcome: planner.WorkflowCompleted,
      ),
    ]
  assert recovery.cleanup_run_roots == []
}

pub fn active_completed_run_after_recovery_emits_recovered_finish_record_test() {
  let dag = parse_dag(single_step_yaml())
  let recovery =
    plan(
      dag,
      planner.WorkflowRunFacts(
        ..base_run([completed("implement", 1)]),
        recovery_evidence: workflow_outcome.StepRecoveryRan,
      ),
      current_ok(),
    )

  assert recovery.outcome == planner.TerminalRecordNeeded
  assert recovery.workflow_finish_records
    == [
      planner.WorkflowFinishRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        issue_id: "issue-1",
        outcome: planner.WorkflowSucceededAfterRecovery,
      ),
    ]
}

pub fn active_fatal_run_emits_failed_finish_record_not_cleanup_test() {
  let dag = parse_dag(single_step_yaml())
  let recovery =
    plan(dag, base_run([failed_fatal("implement", 1)]), current_ok())

  assert recovery.outcome == planner.TerminalRecordNeeded
  assert recovery.workflow_finish_records
    == [
      planner.WorkflowFinishRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        issue_id: "issue-1",
        outcome: planner.WorkflowFailedFatal,
      ),
    ]
  assert recovery.cleanup_run_roots == []
}

pub fn active_fatal_run_after_recovery_emits_recovered_failed_finish_record_test() {
  let dag = parse_dag(single_step_yaml())
  let recovery =
    plan(
      dag,
      planner.WorkflowRunFacts(
        ..base_run([failed_fatal("implement", 1)]),
        recovery_evidence: workflow_outcome.StepRecoveryRecheckRequested,
      ),
      current_ok(),
    )

  assert recovery.outcome == planner.TerminalRecordNeeded
  assert recovery.workflow_finish_records
    == [
      planner.WorkflowFinishRecordIntent(
        run_id: "run-1",
        workflow_id: "review-flow",
        issue_id: "issue-1",
        outcome: planner.WorkflowFailedAfterRecovery,
      ),
    ]
}

pub fn recovered_finished_statuses_classify_as_terminal_success_and_failure_test() {
  let dag = parse_dag(single_step_yaml())
  let base = base_run([])
  let recovered_success =
    planner.WorkflowRunFacts(
      ..base,
      run_status: planner.RunFinished(
        planner.WorkflowSucceededAfterRecovery,
        1,
        1,
      ),
    )
  let recovered_failure =
    planner.WorkflowRunFacts(
      ..base,
      run_status: planner.RunFinished(planner.WorkflowFailedAfterRecovery, 1, 1),
    )

  assert plan(dag, recovered_success, current_ok()).outcome
    == planner.TerminalSucceeded
  assert plan(dag, recovered_failure, current_ok()).outcome
    == planner.TerminalFailed
}

pub fn durable_finished_runs_request_cleanup_once_for_each_terminal_outcome_test() {
  let dag = parse_dag(single_step_yaml())
  let base = base_run([])
  let completed_run =
    planner.WorkflowRunFacts(
      ..base,
      run_status: planner.RunFinished(planner.WorkflowCompleted, 1, 1),
    )
  let failed_run =
    planner.WorkflowRunFacts(
      ..base,
      run_status: planner.RunFinished(planner.WorkflowFailedFatal, 1, 1),
    )
  let cancelled_run =
    planner.WorkflowRunFacts(
      ..base,
      run_status: planner.RunFinished(planner.WorkflowCancelled, 1, 1),
    )
  let cleaned_run =
    planner.WorkflowRunFacts(..completed_run, cleanup_recorded: True)

  let completed_recovery = plan(dag, completed_run, current_ok())
  assert completed_recovery.outcome == planner.TerminalSucceeded
  assert list.length(completed_recovery.cleanup_run_roots) == 1
  assert plan(dag, failed_run, current_ok()).outcome == planner.TerminalFailed
  assert list.length(plan(dag, failed_run, current_ok()).cleanup_run_roots) == 1
  assert plan(dag, cancelled_run, current_ok()).outcome
    == planner.TerminalCancelled
  assert list.length(plan(dag, cancelled_run, current_ok()).cleanup_run_roots)
    == 1
  assert plan(dag, cleaned_run, current_ok()).cleanup_run_roots == []
}

pub fn finished_cleanup_failure_run_root_is_retried_until_cleanup_recorded_test() {
  let dag = parse_dag(single_step_yaml())
  let run =
    planner.WorkflowRunFacts(
      ..base_run([]),
      run_status: planner.RunFinished(planner.WorkflowFailedFatal, 1, 1),
      run_root: "test/tmp/workflow-cleanup-failed-run-root",
      cleanup_recorded: False,
    )
  let recovery = plan(dag, run, current_ok())

  assert recovery.outcome == planner.TerminalFailed
  assert recovery.cleanup_run_roots
    == [
      planner.CleanupRunRoot(
        run_id: "run-1",
        issue_id: "issue-1",
        run_root: "test/tmp/workflow-cleanup-failed-run-root",
      ),
    ]

  let cleaned_recovery =
    plan(
      dag,
      planner.WorkflowRunFacts(..run, cleanup_recorded: True),
      current_ok(),
    )
  assert cleaned_recovery.cleanup_run_roots == []
}

pub fn run_interrupted_and_superseded_are_terminal_for_planner_but_not_cleaned_test() {
  let dag = parse_dag(single_step_yaml())
  let interrupted_run =
    planner.WorkflowRunFacts(
      ..base_run([]),
      run_status: planner.RunInterrupted("daemon_restart"),
    )
  let superseded_run =
    planner.WorkflowRunFacts(
      ..base_run([]),
      run_status: planner.RunSuperseded("run-2", "issue_changed"),
    )

  let interrupted_recovery = plan(dag, interrupted_run, current_ok())
  assert interrupted_recovery.outcome == planner.AlreadyInterrupted
  assert interrupted_recovery.cleanup_run_roots == []
  assert interrupted_recovery.start_steps == []

  let superseded_recovery = plan(dag, superseded_run, current_ok())
  assert superseded_recovery.outcome == planner.AlreadySuperseded
  assert superseded_recovery.cleanup_run_roots == []
  assert superseded_recovery.start_steps == []
}

pub fn durable_finished_run_does_not_start_recovery_test() {
  let dag = parse_dag(single_step_yaml())
  let run =
    planner.WorkflowRunFacts(
      ..base_run([started("implement", 1, Some("pi-session"))]),
      run_status: planner.RunFinished(planner.WorkflowCompleted, 1, 1),
    )
  let recovery = plan(dag, run, current_ok())

  assert recovery.outcome == planner.TerminalSucceeded
  assert list.length(recovery.cleanup_run_roots) == 1
}

pub fn scheduler_parity_for_fan_out_after_completed_root_test() {
  let dag = fan_in_dag()
  let recovery = plan(dag, base_run([completed("implement", 1)]), current_ok())
  let scheduler_state =
    scheduler_state_with(dag, [
      #("implement", workflow_scheduler.Succeeded),
    ])

  assert start_ids(recovery) == scheduler_ready_ids(dag, scheduler_state)
}

pub fn scheduler_parity_for_failed_continued_fan_in_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        failed_continued("test_after_implement", 1),
        completed("code_review", 1),
        completed("security_review", 1),
      ]),
      current_ok(),
    )
  let scheduler_state =
    scheduler_state_with(dag, [
      #("implement", workflow_scheduler.Succeeded),
      #("test_after_implement", workflow_scheduler.FailedContinued),
      #("code_review", workflow_scheduler.Succeeded),
      #("security_review", workflow_scheduler.Succeeded),
    ])

  assert start_ids(recovery) == scheduler_ready_ids(dag, scheduler_state)
}

pub fn scheduler_parity_for_fatal_failure_test() {
  let dag = fan_in_dag()
  let recovery =
    plan(
      dag,
      base_run([
        completed("implement", 1),
        failed_fatal("test_after_implement", 1),
      ]),
      current_ok(),
    )
  let scheduler_state =
    scheduler_state_with(dag, [
      #("implement", workflow_scheduler.Succeeded),
      #("test_after_implement", workflow_scheduler.FailedFatal),
    ])

  assert start_ids(recovery) == scheduler_ready_ids(dag, scheduler_state)
}

pub fn independent_roots_match_scheduler_capacity_test() {
  let dag = parse_dag(independent_roots_yaml())
  let recovery =
    plan(dag, run_for("parallel-roots", []), current_for("parallel-roots"))
  let scheduler_state = scheduler_state_with(dag, [])

  assert start_ids(recovery) == scheduler_ready_ids(dag, scheduler_state)
  assert start_ids(recovery) == ["docs", "tests"]
}

pub fn same_workspace_roots_match_scheduler_serialization_test() {
  let dag = parse_dag(same_workspace_roots_yaml())
  let recovery =
    plan(
      dag,
      run_for("same-workspace-roots", []),
      current_for("same-workspace-roots"),
    )
  let scheduler_state = scheduler_state_with(dag, [])

  assert start_ids(recovery) == scheduler_ready_ids(dag, scheduler_state)
  assert start_ids(recovery) == ["docs"]
}

pub fn artifact_boundary_preserves_verified_ref_hash_and_payload_test() {
  let dag = parse_dag(single_step_yaml())
  let artifact = failure_artifact("implement", 1)
  let recovery =
    plan(
      dag,
      base_run([finished("implement", 1, planner.RecordedFailedFatal, artifact)]),
      current_ok(),
    )
  let preserved = preserved_for(recovery, "implement")

  assert preserved.artifact_ref == "runs/run-1/implement/attempt-1.json"
  assert preserved.artifact_sha256 == "sha256-implement-1"
  assert preserved.artifact.stderr == "failed"
  assert preserved.artifact.summary_text == "failure implement"
}

pub fn policy_can_disable_ready_starting_without_changing_classification_test() {
  let dag = fan_in_dag()
  let run = base_run([])
  let policy =
    planner.RecoveryPolicy(
      allow_starting_ready_pending_steps: False,
      park_unsafe_interruptions: True,
    )
  let recovery =
    planner.plan_run(planner.PlannerInput(
      run: run,
      dag: dag,
      current: current_ok(),
      policy: policy,
      now_ms: 2000,
    ))

  assert state_for(recovery, "implement")
    == planner.StepUnattempted("implement")
  assert_no_start_steps(recovery)
  assert recovery.outcome == planner.InProgressBlocked
}

pub fn repeated_planning_is_idempotent_for_same_input_test() {
  let dag = fan_in_dag()
  let planner_input =
    input(
      dag,
      base_run([completed("implement", 1), started("code_review", 1, None)]),
      current_ok(),
    )

  let first = planner.plan_run(planner_input)
  let second = planner.plan_run(planner_input)

  assert first.start_steps == second.start_steps
  assert first.interruption_records == second.interruption_records
  assert first.workflow_finish_records == second.workflow_finish_records
  assert first.cleanup_run_roots == second.cleanup_run_roots
  assert first.drift_errors == second.drift_errors
}
