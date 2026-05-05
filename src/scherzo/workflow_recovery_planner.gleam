import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/step_artifact
import scherzo/workflow_dag
import scherzo/workflow_scheduler

pub const start_reason_dependencies_complete_after_startup = "dependencies_complete_after_startup"

pub const park_release_policy_manual = "manual"

pub type PlannerInput {
  PlannerInput(
    run: WorkflowRunFacts,
    dag: workflow_dag.WorkflowDag,
    current: CurrentWorkflowObservation,
    policy: RecoveryPolicy,
    now_ms: Int,
  )
}

pub type WorkflowRunFacts {
  WorkflowRunFacts(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    cleanup_recorded: Bool,
    run_status: DurableRunStatus,
    step_attempts: List(StepAttemptFacts),
  )
}

pub type DurableRunStatus {
  RunActive
  RunFinished(outcome: WorkflowRunOutcome, token_total: Int, turns: Int)
  RunInterrupted(reason: String)
  RunSuperseded(superseded_by_run_id: String, reason: String)
}

pub type WorkflowRunOutcome {
  WorkflowCompleted
  WorkflowFailedFatal
  WorkflowCancelled
}

pub type CurrentWorkflowObservation {
  CurrentWorkflowObservation(
    workflow_id: String,
    workflow_fingerprint: String,
    issue_fingerprint: String,
  )
  IssueUnavailable(reason: String)
  WorkflowUnavailable(reason: String)
}

pub type StepAttemptFacts {
  StepAttemptFacts(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    status: DurableStepAttemptStatus,
  )
}

pub type DurableStepAttemptStatus {
  AttemptPrepared(
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
  )
  AttemptStarted(
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    operator_session_id: String,
    external_session_ref: Option(String),
  )
  AttemptFinished(
    outcome: RecordedStepOutcome,
    artifact: VerifiedArtifact,
    workspace_name: String,
    workspace_path: String,
    token_total: Int,
    turns: Int,
  )
  AttemptInterrupted(reason: String)
  AttemptSuperseded(superseded_by_attempt_index: Int, reason: String)
}

pub type RecordedStepOutcome {
  RecordedCompleted
  RecordedFailedContinued
  RecordedFailedFatal
}

pub type VerifiedArtifact {
  VerifiedArtifact(
    artifact_ref: String,
    artifact_sha256: String,
    artifact: step_artifact.StepArtifact,
  )
}

pub type StepRecoveryState {
  StepUnattempted(step_id: String)
  StepCompleted(step_id: String, attempt_index: Int, artifact: VerifiedArtifact)
  StepFailedContinued(
    step_id: String,
    attempt_index: Int,
    artifact: VerifiedArtifact,
  )
  StepFailedFatal(
    step_id: String,
    attempt_index: Int,
    artifact: VerifiedArtifact,
  )
  StepNeedsInterruptionBeforeStart(
    step_id: String,
    attempt_index: Int,
    workspace_path: String,
  )
  StepNeedsInterruptionAfterStart(
    step_id: String,
    attempt_index: Int,
    workspace_path: String,
    operator_session_id: String,
    external_session_ref: Option(String),
  )
  StepAlreadyInterrupted(step_id: String, attempt_index: Int, reason: String)
  StepSuperseded(step_id: String, attempt_index: Int, reason: String)
}

pub type RunRecoveryOutcome {
  Continuable
  DriftBlocked
  NeedsInspection
  InProgressBlocked
  TerminalSucceeded
  TerminalFailed
  TerminalCancelled
  TerminalRecordNeeded
  AlreadyInterrupted
  AlreadySuperseded
}

pub type RecoveryPlan {
  RecoveryPlan(
    run_id: String,
    issue_id: String,
    outcome: RunRecoveryOutcome,
    step_states: Dict(String, StepRecoveryState),
    preserved_artifacts: Dict(String, VerifiedArtifact),
    start_steps: List(StartStep),
    blocked_steps: List(BlockedStep),
    interruption_records: List(InterruptionRecordIntent),
    workflow_finish_records: List(WorkflowFinishRecordIntent),
    inspection_requests: List(InspectionRequest),
    park_requests: List(ParkRequest),
    cleanup_run_roots: List(CleanupRunRoot),
    session_recovery_candidates: List(SessionRecoveryCandidate),
    drift_errors: List(DriftError),
    warnings: List(String),
  )
}

pub type StartStep {
  StartStep(step_id: String, workspace_name: String, reason: String)
}

pub type BlockedStep {
  BlockedStep(step_id: String, blockers: List(String))
}

pub type InterruptionRecordIntent {
  InterruptionRecordIntent(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    reason: InterruptionReason,
  )
}

pub type InterruptionReason {
  DaemonRestartBeforeStepStart
  DaemonRestartDuringStep
}

pub type WorkflowFinishRecordIntent {
  WorkflowFinishRecordIntent(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    outcome: WorkflowRunOutcome,
  )
}

pub type InspectionRequest {
  InspectionRequest(
    issue_id: String,
    issue_identifier: String,
    run_id: String,
    reason: InspectionReason,
  )
}

pub type InspectionReason {
  DriftRequiresInspection
  StepInterruptedAfterStart(step_id: String, attempt_index: Int)
  StepAlreadyInterruptedNeedsInspection(step_id: String, attempt_index: Int)
  StepSupersededNeedsInspection(step_id: String, attempt_index: Int)
}

pub type ParkRequest {
  ParkRequest(
    issue_id: String,
    issue_identifier: String,
    reason: ParkReason,
    release_policy: String,
    issue_fingerprint: Option(String),
  )
}

pub type ParkReason {
  ParkDriftBlocked
  ParkStepInterruptedAfterStart(step_id: String, attempt_index: Int)
  ParkStepAlreadyInterrupted(step_id: String, attempt_index: Int)
  ParkStepSuperseded(step_id: String, attempt_index: Int)
}

pub type CleanupRunRoot {
  CleanupRunRoot(run_id: String, issue_id: String, run_root: String)
}

pub type SessionRecoveryCandidate {
  SessionRecoveryCandidate(
    step_id: String,
    attempt_index: Int,
    operator_session_id: String,
    external_session_ref: Option(String),
    workspace_path: String,
  )
}

pub type DriftError {
  WorkflowIdMismatch(recorded: String, current: String)
  WorkflowFingerprintDrift(recorded: String, current: String)
  IssueFingerprintDrift(recorded: String, current: String)
  IssueUnavailableDrift(reason: String)
  WorkflowUnavailableDrift(reason: String)
  UnknownStepAttempt(step_id: String)
  AttemptRunIdMismatch(
    step_id: String,
    attempt_index: Int,
    recorded_run_id: String,
    expected_run_id: String,
  )
  AttemptWorkflowIdMismatch(
    step_id: String,
    attempt_index: Int,
    recorded_workflow_id: String,
    expected_workflow_id: String,
  )
}

pub type RecoveryPolicy {
  RecoveryPolicy(
    allow_starting_ready_pending_steps: Bool,
    park_unsafe_interruptions: Bool,
  )
}

type AttemptValidation {
  AttemptValidation(
    valid_attempts: List(StepAttemptFacts),
    drift_errors: List(DriftError),
    warnings: List(String),
  )
}

type StepClassification {
  StepClassification(
    step_states: Dict(String, StepRecoveryState),
    warnings: List(String),
  )
}

type FinishDecision {
  FinishDecision(
    outcome: RunRecoveryOutcome,
    records: List(WorkflowFinishRecordIntent),
  )
}

pub fn default_policy() -> RecoveryPolicy {
  RecoveryPolicy(
    allow_starting_ready_pending_steps: True,
    park_unsafe_interruptions: True,
  )
}

pub fn interruption_reason_to_string(reason: InterruptionReason) -> String {
  case reason {
    DaemonRestartBeforeStepStart -> "daemon_restart_before_step_start"
    DaemonRestartDuringStep -> "daemon_restart_during_step"
  }
}

pub fn inspection_reason_to_string(reason: InspectionReason) -> String {
  case reason {
    DriftRequiresInspection -> "drift_requires_inspection"
    StepInterruptedAfterStart(step_id, attempt_index) ->
      "step_interrupted_after_start:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
    StepAlreadyInterruptedNeedsInspection(step_id, attempt_index) ->
      "step_already_interrupted:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
    StepSupersededNeedsInspection(step_id, attempt_index) ->
      "step_superseded:" <> step_id <> ":" <> int.to_string(attempt_index)
  }
}

pub fn park_reason_to_string(reason: ParkReason) -> String {
  case reason {
    ParkDriftBlocked -> "drift_blocked"
    ParkStepInterruptedAfterStart(step_id, attempt_index) ->
      "step_interrupted_after_start:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
    ParkStepAlreadyInterrupted(step_id, attempt_index) ->
      "step_already_interrupted:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
    ParkStepSuperseded(step_id, attempt_index) ->
      "step_superseded:" <> step_id <> ":" <> int.to_string(attempt_index)
  }
}

pub fn plan_run(input: PlannerInput) -> RecoveryPlan {
  let _ = input.now_ms
  let base_drift_errors = observation_drift_errors(input.run, input.current)
  let AttemptValidation(valid_attempts, attempt_drift_errors, attempt_warnings) =
    validate_attempts(input.run, input.dag, input.run.step_attempts)
  let drift_errors = list.append(base_drift_errors, attempt_drift_errors)
  let StepClassification(step_states, classification_warnings) =
    classify_steps(input.dag.steps, valid_attempts)
  let preserved_artifacts = preserve_artifacts(step_states)
  let interruption_records =
    interruption_intents(input.dag.steps, input.run, step_states)
  let session_recovery_candidates =
    session_candidates(input, step_states, drift_errors)
  let inspection_requests =
    inspection_requests(input, step_states, drift_errors)
  let park_requests = park_requests(input, step_states, drift_errors)
  let start_steps = start_steps(input, step_states, drift_errors)
  let workflow_finish =
    finish_decision(
      input.run,
      input.dag,
      step_states,
      drift_errors,
      start_steps,
    )
  let cleanup_run_roots = cleanup_requests(input.run)
  let warnings = list.append(attempt_warnings, classification_warnings)

  RecoveryPlan(
    run_id: input.run.run_id,
    issue_id: input.run.issue_id,
    outcome: workflow_finish.outcome,
    step_states: step_states,
    preserved_artifacts: preserved_artifacts,
    start_steps: start_steps,
    blocked_steps: blocked_steps(input.dag, step_states, drift_errors),
    interruption_records: interruption_records,
    workflow_finish_records: workflow_finish.records,
    inspection_requests: inspection_requests,
    park_requests: park_requests,
    cleanup_run_roots: cleanup_run_roots,
    session_recovery_candidates: session_recovery_candidates,
    drift_errors: drift_errors,
    warnings: warnings,
  )
}

fn observation_drift_errors(
  run: WorkflowRunFacts,
  current: CurrentWorkflowObservation,
) -> List(DriftError) {
  case current {
    IssueUnavailable(reason) -> [IssueUnavailableDrift(reason)]
    WorkflowUnavailable(reason) -> [WorkflowUnavailableDrift(reason)]
    CurrentWorkflowObservation(
      workflow_id: workflow_id,
      workflow_fingerprint: workflow_fingerprint,
      issue_fingerprint: issue_fingerprint,
    ) -> {
      []
      |> add_if(
        run.workflow_id != workflow_id,
        WorkflowIdMismatch(run.workflow_id, workflow_id),
      )
      |> add_if(
        run.workflow_fingerprint != workflow_fingerprint,
        WorkflowFingerprintDrift(run.workflow_fingerprint, workflow_fingerprint),
      )
      |> add_if(
        run.issue_fingerprint != issue_fingerprint,
        IssueFingerprintDrift(run.issue_fingerprint, issue_fingerprint),
      )
      |> list.reverse
    }
  }
}

fn validate_attempts(
  run: WorkflowRunFacts,
  dag: workflow_dag.WorkflowDag,
  attempts: List(StepAttemptFacts),
) -> AttemptValidation {
  let step_ids = list.map(dag.steps, fn(step) { step.id })
  let result =
    list.fold(attempts, AttemptValidation([], [], []), fn(validation, attempt) {
      validate_attempt(run, step_ids, validation, attempt)
    })
  AttemptValidation(
    valid_attempts: list.reverse(result.valid_attempts),
    drift_errors: list.reverse(result.drift_errors),
    warnings: list.reverse(result.warnings),
  )
}

fn validate_attempt(
  run: WorkflowRunFacts,
  step_ids: List(String),
  validation: AttemptValidation,
  attempt: StepAttemptFacts,
) -> AttemptValidation {
  let run_matches = attempt.run_id == run.run_id
  let workflow_matches = attempt.workflow_id == run.workflow_id
  let step_known = list.contains(step_ids, attempt.step_id)
  let drift_errors =
    []
    |> add_if(
      !run_matches,
      AttemptRunIdMismatch(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        recorded_run_id: attempt.run_id,
        expected_run_id: run.run_id,
      ),
    )
    |> add_if(
      !workflow_matches,
      AttemptWorkflowIdMismatch(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        recorded_workflow_id: attempt.workflow_id,
        expected_workflow_id: run.workflow_id,
      ),
    )
    |> add_if(!step_known, UnknownStepAttempt(attempt.step_id))

  let warnings = drift_errors |> list.map(drift_warning)
  let valid_attempts = case run_matches && workflow_matches && step_known {
    True -> [attempt, ..validation.valid_attempts]
    False -> validation.valid_attempts
  }

  AttemptValidation(
    valid_attempts: valid_attempts,
    drift_errors: list.append(drift_errors, validation.drift_errors),
    warnings: list.append(warnings, validation.warnings),
  )
}

fn classify_steps(
  steps: List(workflow_dag.WorkflowStep),
  attempts: List(StepAttemptFacts),
) -> StepClassification {
  let result =
    list.fold(
      steps,
      StepClassification(dict.new(), []),
      fn(classification, step) {
        let step_attempts = attempts_for_step(attempts, step.id)
        let warnings = case duplicate_attempt_index(step_attempts) {
          True -> [
            "duplicate_step_attempt_index:" <> step.id,
            ..classification.warnings
          ]
          False -> classification.warnings
        }
        let state = classify_step(step.id, step_attempts)
        StepClassification(
          step_states: dict.insert(classification.step_states, step.id, state),
          warnings: warnings,
        )
      },
    )
  StepClassification(
    step_states: result.step_states,
    warnings: list.reverse(result.warnings),
  )
}

fn attempts_for_step(
  attempts: List(StepAttemptFacts),
  step_id: String,
) -> List(StepAttemptFacts) {
  list.filter(attempts, fn(attempt) { attempt.step_id == step_id })
}

fn classify_step(
  step_id: String,
  attempts: List(StepAttemptFacts),
) -> StepRecoveryState {
  case attempts {
    [] -> StepUnattempted(step_id)
    _ -> {
      let sorted = list.sort(attempts, by: compare_attempts)
      let assert Ok(latest) = last(sorted)
      classify_latest_attempt(latest)
    }
  }
}

fn classify_latest_attempt(attempt: StepAttemptFacts) -> StepRecoveryState {
  case attempt.status {
    AttemptPrepared(workspace_path: workspace_path, ..) ->
      StepNeedsInterruptionBeforeStart(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        workspace_path: workspace_path,
      )
    AttemptStarted(
      workspace_path: workspace_path,
      operator_session_id: operator_session_id,
      external_session_ref: external_session_ref,
      ..,
    ) ->
      StepNeedsInterruptionAfterStart(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        workspace_path: workspace_path,
        operator_session_id: operator_session_id,
        external_session_ref: external_session_ref,
      )
    AttemptFinished(outcome: outcome, artifact: artifact, ..) ->
      case outcome {
        RecordedCompleted ->
          StepCompleted(
            step_id: attempt.step_id,
            attempt_index: attempt.attempt_index,
            artifact: artifact,
          )
        RecordedFailedContinued ->
          StepFailedContinued(
            step_id: attempt.step_id,
            attempt_index: attempt.attempt_index,
            artifact: artifact,
          )
        RecordedFailedFatal ->
          StepFailedFatal(
            step_id: attempt.step_id,
            attempt_index: attempt.attempt_index,
            artifact: artifact,
          )
      }
    AttemptInterrupted(reason) ->
      StepAlreadyInterrupted(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        reason: reason,
      )
    AttemptSuperseded(reason: reason, ..) ->
      StepSuperseded(
        step_id: attempt.step_id,
        attempt_index: attempt.attempt_index,
        reason: reason,
      )
  }
}

fn compare_attempts(a: StepAttemptFacts, b: StepAttemptFacts) -> Order {
  case int.compare(a.attempt_index, b.attempt_index) {
    Eq -> string.compare(status_sort_key(a.status), status_sort_key(b.status))
    order -> order
  }
}

fn status_sort_key(status: DurableStepAttemptStatus) -> String {
  case status {
    AttemptPrepared(..) -> "1_prepared"
    AttemptStarted(..) -> "2_started"
    AttemptFinished(RecordedCompleted, ..) -> "3_finished_completed"
    AttemptFinished(RecordedFailedContinued, ..) ->
      "3_finished_failed_continued"
    AttemptFinished(RecordedFailedFatal, ..) -> "3_finished_failed_fatal"
    AttemptInterrupted(..) -> "4_interrupted"
    AttemptSuperseded(..) -> "5_superseded"
  }
}

fn duplicate_attempt_index(attempts: List(StepAttemptFacts)) -> Bool {
  duplicate_attempt_index_loop(attempts, [])
}

fn duplicate_attempt_index_loop(
  attempts: List(StepAttemptFacts),
  seen: List(Int),
) -> Bool {
  case attempts {
    [] -> False
    [attempt, ..rest] ->
      case list.contains(seen, attempt.attempt_index) {
        True -> True
        False ->
          duplicate_attempt_index_loop(rest, [attempt.attempt_index, ..seen])
      }
  }
}

fn preserve_artifacts(
  states: Dict(String, StepRecoveryState),
) -> Dict(String, VerifiedArtifact) {
  states
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(step_id, state) = entry
    case state {
      StepCompleted(artifact: artifact, ..)
      | StepFailedContinued(artifact: artifact, ..)
      | StepFailedFatal(artifact: artifact, ..) -> Ok(#(step_id, artifact))
      _ -> Error(Nil)
    }
  })
  |> dict.from_list
}

fn interruption_intents(
  steps: List(workflow_dag.WorkflowStep),
  run: WorkflowRunFacts,
  states: Dict(String, StepRecoveryState),
) -> List(InterruptionRecordIntent) {
  case run.run_status {
    RunActive ->
      steps
      |> list.filter_map(fn(step) {
        case step_state(states, step.id) {
          StepNeedsInterruptionBeforeStart(step_id, attempt_index, _) ->
            Ok(InterruptionRecordIntent(
              run_id: run.run_id,
              workflow_id: run.workflow_id,
              step_id: step_id,
              attempt_index: attempt_index,
              reason: DaemonRestartBeforeStepStart,
            ))
          StepNeedsInterruptionAfterStart(step_id, attempt_index, _, _, _) ->
            Ok(InterruptionRecordIntent(
              run_id: run.run_id,
              workflow_id: run.workflow_id,
              step_id: step_id,
              attempt_index: attempt_index,
              reason: DaemonRestartDuringStep,
            ))
          _ -> Error(Nil)
        }
      })
    _ -> []
  }
}

fn session_candidates(
  input: PlannerInput,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
) -> List(SessionRecoveryCandidate) {
  case
    input.run.run_status,
    list.is_empty(drift_errors),
    workflow_definition_matches(input.run, input.current)
  {
    RunActive, True, True ->
      input.dag.steps
      |> list.filter_map(fn(step) {
        case step_state(states, step.id) {
          StepNeedsInterruptionAfterStart(
            step_id,
            attempt_index,
            workspace_path,
            operator_session_id,
            external_session_ref,
          ) ->
            case is_agent_step(input.dag, step_id) {
              True ->
                Ok(SessionRecoveryCandidate(
                  step_id: step_id,
                  attempt_index: attempt_index,
                  operator_session_id: operator_session_id,
                  external_session_ref: external_session_ref,
                  workspace_path: workspace_path,
                ))
              False -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    _, _, _ -> []
  }
}

fn inspection_requests(
  input: PlannerInput,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
) -> List(InspectionRequest) {
  case input.run.run_status {
    RunActive -> {
      let drift_requests = case list.is_empty(drift_errors) {
        True -> []
        False -> [inspection_request(input.run, DriftRequiresInspection)]
      }
      let step_requests =
        input.dag.steps
        |> list.filter_map(fn(step) {
          case step_state(states, step.id) {
            StepNeedsInterruptionAfterStart(step_id, attempt_index, _, _, _) ->
              Ok(inspection_request(
                input.run,
                StepInterruptedAfterStart(step_id, attempt_index),
              ))
            StepAlreadyInterrupted(step_id, attempt_index, _) ->
              Ok(inspection_request(
                input.run,
                StepAlreadyInterruptedNeedsInspection(step_id, attempt_index),
              ))
            StepSuperseded(step_id, attempt_index, _) ->
              Ok(inspection_request(
                input.run,
                StepSupersededNeedsInspection(step_id, attempt_index),
              ))
            _ -> Error(Nil)
          }
        })
      list.append(drift_requests, step_requests)
    }
    _ -> []
  }
}

fn inspection_request(
  run: WorkflowRunFacts,
  reason: InspectionReason,
) -> InspectionRequest {
  InspectionRequest(
    issue_id: run.issue_id,
    issue_identifier: run.issue_identifier,
    run_id: run.run_id,
    reason: reason,
  )
}

fn park_requests(
  input: PlannerInput,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
) -> List(ParkRequest) {
  case input.run.run_status, input.policy.park_unsafe_interruptions {
    RunActive, True -> {
      let fingerprint = current_issue_fingerprint(input.current)
      let drift_requests = case list.is_empty(drift_errors) {
        True -> []
        False -> [park_request(input.run, ParkDriftBlocked, fingerprint)]
      }
      let step_requests =
        input.dag.steps
        |> list.filter_map(fn(step) {
          case step_state(states, step.id) {
            StepNeedsInterruptionAfterStart(step_id, attempt_index, _, _, _) ->
              Ok(park_request(
                input.run,
                ParkStepInterruptedAfterStart(step_id, attempt_index),
                fingerprint,
              ))
            StepAlreadyInterrupted(step_id, attempt_index, _) ->
              Ok(park_request(
                input.run,
                ParkStepAlreadyInterrupted(step_id, attempt_index),
                fingerprint,
              ))
            StepSuperseded(step_id, attempt_index, _) ->
              Ok(park_request(
                input.run,
                ParkStepSuperseded(step_id, attempt_index),
                fingerprint,
              ))
            _ -> Error(Nil)
          }
        })
      list.append(drift_requests, step_requests)
    }
    _, _ -> []
  }
}

fn park_request(
  run: WorkflowRunFacts,
  reason: ParkReason,
  issue_fingerprint: Option(String),
) -> ParkRequest {
  ParkRequest(
    issue_id: run.issue_id,
    issue_identifier: run.issue_identifier,
    reason: reason,
    release_policy: park_release_policy_manual,
    issue_fingerprint: issue_fingerprint,
  )
}

fn start_steps(
  input: PlannerInput,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
) -> List(StartStep) {
  case
    input.run.run_status,
    input.policy.allow_starting_ready_pending_steps,
    list.is_empty(drift_errors),
    !has_unresolved_state(states),
    !has_fatal_state(states)
  {
    RunActive, True, True, True, True -> {
      let scheduler_state = scheduler_state(input.dag, states)
      workflow_scheduler.ready_steps(input.dag, scheduler_state)
      |> list.map(fn(step) {
        StartStep(
          step_id: step.id,
          workspace_name: step.workspace.name,
          reason: start_reason_dependencies_complete_after_startup,
        )
      })
    }
    _, _, _, _, _ -> []
  }
}

fn scheduler_state(
  dag: workflow_dag.WorkflowDag,
  states: Dict(String, StepRecoveryState),
) -> workflow_scheduler.SchedulerState {
  let statuses =
    dag.steps
    |> list.map(fn(step) {
      #(step.id, scheduler_runtime_for_state(step_state(states, step.id)))
    })
    |> dict.from_list
  let failure_policies =
    dag.steps
    |> list.map(fn(step) { #(step.id, step.on_failure) })
    |> dict.from_list
  workflow_scheduler.SchedulerState(
    statuses: statuses,
    failure_policies: failure_policies,
    cancelling: False,
  )
}

fn scheduler_runtime_for_state(
  state: StepRecoveryState,
) -> workflow_scheduler.StepRuntime {
  case state {
    StepCompleted(..) -> workflow_scheduler.Succeeded
    StepFailedContinued(..) -> workflow_scheduler.FailedContinued
    StepFailedFatal(..) -> workflow_scheduler.FailedFatal
    _ -> workflow_scheduler.Pending
  }
}

fn finish_decision(
  run: WorkflowRunFacts,
  dag: workflow_dag.WorkflowDag,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
  start_steps: List(StartStep),
) -> FinishDecision {
  case run.run_status {
    RunFinished(WorkflowCompleted, _, _) ->
      FinishDecision(TerminalSucceeded, [])
    RunFinished(WorkflowFailedFatal, _, _) -> FinishDecision(TerminalFailed, [])
    RunFinished(WorkflowCancelled, _, _) ->
      FinishDecision(TerminalCancelled, [])
    RunInterrupted(_) -> FinishDecision(AlreadyInterrupted, [])
    RunSuperseded(_, _) -> FinishDecision(AlreadySuperseded, [])
    RunActive ->
      case list.is_empty(drift_errors) {
        False -> FinishDecision(DriftBlocked, [])
        True ->
          case has_fatal_state(states) {
            True ->
              FinishDecision(TerminalRecordNeeded, [
                WorkflowFinishRecordIntent(
                  run_id: run.run_id,
                  workflow_id: run.workflow_id,
                  issue_id: run.issue_id,
                  outcome: WorkflowFailedFatal,
                ),
              ])
            False ->
              case all_steps_dependency_complete(dag.steps, states) {
                True ->
                  FinishDecision(TerminalRecordNeeded, [
                    WorkflowFinishRecordIntent(
                      run_id: run.run_id,
                      workflow_id: run.workflow_id,
                      issue_id: run.issue_id,
                      outcome: WorkflowCompleted,
                    ),
                  ])
                False ->
                  case has_unresolved_state(states) {
                    True -> FinishDecision(NeedsInspection, [])
                    False ->
                      case list.is_empty(start_steps) {
                        False -> FinishDecision(Continuable, [])
                        True -> FinishDecision(InProgressBlocked, [])
                      }
                  }
              }
          }
      }
  }
}

fn cleanup_requests(run: WorkflowRunFacts) -> List(CleanupRunRoot) {
  case run.run_status, run.cleanup_recorded {
    RunFinished(_, _, _), False -> [
      CleanupRunRoot(
        run_id: run.run_id,
        issue_id: run.issue_id,
        run_root: run.run_root,
      ),
    ]
    _, _ -> []
  }
}

fn blocked_steps(
  dag: workflow_dag.WorkflowDag,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
) -> List(BlockedStep) {
  let workflow_fatal = has_fatal_state(states)
  dag.steps
  |> list.filter_map(fn(step) {
    let blockers = blockers_for_step(step, states, drift_errors, workflow_fatal)
    case blockers {
      [] -> Error(Nil)
      _ -> Ok(BlockedStep(step_id: step.id, blockers: blockers))
    }
  })
}

fn blockers_for_step(
  step: workflow_dag.WorkflowStep,
  states: Dict(String, StepRecoveryState),
  drift_errors: List(DriftError),
  workflow_fatal: Bool,
) -> List(String) {
  let dependency_blockers =
    step.depends_on
    |> list.filter(fn(dep_id) {
      !dependency_complete(step_state(states, dep_id))
    })
  let state_blockers = self_blockers(step_state(states, step.id))
  let drift_blockers = case list.is_empty(drift_errors) {
    True -> []
    False -> ["run_drift"]
  }
  let fatal_blockers = case
    workflow_fatal && !complete_or_fatal(step_state(states, step.id))
  {
    True -> ["workflow_failed_fatal"]
    False -> []
  }
  []
  |> list.append(dependency_blockers)
  |> list.append(state_blockers)
  |> list.append(drift_blockers)
  |> list.append(fatal_blockers)
}

fn self_blockers(state: StepRecoveryState) -> List(String) {
  case state {
    StepNeedsInterruptionBeforeStart(step_id, _, _) -> [
      "step_needs_interruption:" <> step_id,
    ]
    StepNeedsInterruptionAfterStart(step_id, _, _, _, _) -> [
      "step_needs_interruption:" <> step_id,
    ]
    StepAlreadyInterrupted(step_id, _, _) -> ["step_interrupted:" <> step_id]
    StepSuperseded(step_id, _, _) -> ["step_superseded:" <> step_id]
    _ -> []
  }
}

fn dependency_complete(state: StepRecoveryState) -> Bool {
  case state {
    StepCompleted(..) | StepFailedContinued(..) -> True
    _ -> False
  }
}

fn complete_or_fatal(state: StepRecoveryState) -> Bool {
  case state {
    StepCompleted(..) | StepFailedContinued(..) | StepFailedFatal(..) -> True
    _ -> False
  }
}

fn all_steps_dependency_complete(
  steps: List(workflow_dag.WorkflowStep),
  states: Dict(String, StepRecoveryState),
) -> Bool {
  steps
  |> list.all(fn(step) { dependency_complete(step_state(states, step.id)) })
}

fn has_unresolved_state(states: Dict(String, StepRecoveryState)) -> Bool {
  states
  |> dict.values
  |> list.any(fn(state) {
    case state {
      StepNeedsInterruptionBeforeStart(..)
      | StepNeedsInterruptionAfterStart(..)
      | StepAlreadyInterrupted(..)
      | StepSuperseded(..) -> True
      _ -> False
    }
  })
}

fn has_fatal_state(states: Dict(String, StepRecoveryState)) -> Bool {
  states
  |> dict.values
  |> list.any(fn(state) {
    case state {
      StepFailedFatal(..) -> True
      _ -> False
    }
  })
}

fn step_state(
  states: Dict(String, StepRecoveryState),
  step_id: String,
) -> StepRecoveryState {
  dict.get(states, step_id)
  |> result.unwrap(StepUnattempted(step_id))
}

fn is_agent_step(dag: workflow_dag.WorkflowDag, step_id: String) -> Bool {
  case workflow_dag.step_by_id(dag, step_id) {
    Ok(workflow_dag.WorkflowStep(kind: workflow_dag.AgentStep(_), ..)) -> True
    _ -> False
  }
}

fn workflow_definition_matches(
  run: WorkflowRunFacts,
  current: CurrentWorkflowObservation,
) -> Bool {
  case current {
    CurrentWorkflowObservation(workflow_id, workflow_fingerprint, _) ->
      run.workflow_id == workflow_id
      && run.workflow_fingerprint == workflow_fingerprint
    _ -> False
  }
}

fn current_issue_fingerprint(
  current: CurrentWorkflowObservation,
) -> Option(String) {
  case current {
    CurrentWorkflowObservation(issue_fingerprint: issue_fingerprint, ..) ->
      Some(issue_fingerprint)
    _ -> None
  }
}

fn add_if(values: List(a), condition: Bool, value: a) -> List(a) {
  case condition {
    True -> [value, ..values]
    False -> values
  }
}

fn last(values: List(a)) -> Result(a, Nil) {
  case values {
    [] -> Error(Nil)
    [value] -> Ok(value)
    [_, ..rest] -> last(rest)
  }
}

fn drift_warning(error: DriftError) -> String {
  case error {
    WorkflowIdMismatch(recorded, current) ->
      "workflow_id_mismatch:" <> recorded <> ":" <> current
    WorkflowFingerprintDrift(recorded, current) ->
      "workflow_fingerprint_drift:" <> recorded <> ":" <> current
    IssueFingerprintDrift(recorded, current) ->
      "issue_fingerprint_drift:" <> recorded <> ":" <> current
    IssueUnavailableDrift(reason) -> "issue_unavailable:" <> reason
    WorkflowUnavailableDrift(reason) -> "workflow_unavailable:" <> reason
    UnknownStepAttempt(step_id) -> "unknown_step_attempt:" <> step_id
    AttemptRunIdMismatch(
      step_id,
      attempt_index,
      recorded_run_id,
      expected_run_id,
    ) ->
      "attempt_run_id_mismatch:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
      <> ":"
      <> recorded_run_id
      <> ":"
      <> expected_run_id
    AttemptWorkflowIdMismatch(
      step_id,
      attempt_index,
      recorded_workflow_id,
      expected_workflow_id,
    ) ->
      "attempt_workflow_id_mismatch:"
      <> step_id
      <> ":"
      <> int.to_string(attempt_index)
      <> ":"
      <> recorded_workflow_id
      <> ":"
      <> expected_workflow_id
  }
}
