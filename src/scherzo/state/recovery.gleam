import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/hash
import scherzo/path
import scherzo/runtime/identity
import scherzo/runtime/reason
import scherzo/runtime/recovery_policy
import scherzo/runtime/state as orchestrator_state
import scherzo/state/artifact_store
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt
import scherzo/workflow_dag
import scherzo/workflow_outcome
import simplifile

pub type RecoveredRetry {
  RecoveredRetry(
    issue_id: String,
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
}

pub type CleanupRequest {
  CleanupRequest(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
}

pub type OutboxReplay {
  OutboxReplay(
    outbox_id: String,
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub type RecoveredWorkspaceSummary {
  RecoveredWorkspaceSummary(
    workflow_id: String,
    run_id: String,
    run_root: String,
    workspace_name: String,
    path: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    attempt_index: Int,
  )
}

pub type RecoveredContractManifest {
  RecoveredContractManifest(ref: String, sha256: String, bytes: Int)
}

pub type RecoveredWorkflowRun {
  RecoveredWorkflowRun(
    issue: tracker_issue.Issue,
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    run_root: String,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    completed_artifacts: Dict(String, step_artifact.StepArtifact),
    completed_workspaces: Dict(String, RecoveredWorkspaceSummary),
    next_attempt_indexes: Dict(String, Int),
    pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
    contract_input_manifest: Option(RecoveredContractManifest),
    contract_output_manifest: Option(RecoveredContractManifest),
  )
}

pub type WorkflowRecoveryCandidate {
  WorkflowRecoveryCandidate(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    task_ref: record.TaskRefFields,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    attempts: List(projection.StepAttemptStatus),
    contract_input_manifest: Option(RecoveredContractManifest),
    contract_output_manifest: Option(RecoveredContractManifest),
  )
}

pub type CurrentWorkflowObservation {
  CurrentWorkflow(
    issue: tracker_issue.Issue,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_fingerprint: String,
    dag: workflow_dag.WorkflowDag,
    workspace_root: String,
  )
  IssueUnavailable
  WorkflowUnavailable(reason: String)
}

pub type WorkflowRecoveryMode {
  ResumeRecoveredWorkflows
  ParkRecoveredWorkflows
  ResumeExplicitRetryStep
}

type SessionRecoveryConfig {
  SessionRecoveryConfig(enabled: Bool, recovery_prompt: String)
}

pub type WorkflowFinalization {
  WorkflowFinalization(
    records_to_append: List(record.LedgerRecord),
    resumptions: List(RecoveredWorkflowRun),
    warnings: List(String),
    diagnostics: List(WorkflowRecoveryDiagnostic),
  )
}

pub opaque type ArtifactRecoveryFailure {
  ArtifactRecoveryFailure(
    step_id: String,
    artifact_ref: String,
    reason: String,
    expected_sha256: Option(String),
    current_sha256: Option(String),
  )
}

pub opaque type WorkflowRecoveryDiagnostic {
  ArtifactRecoveryDiagnostic(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    detail: ArtifactRecoveryFailure,
  )
}

pub type RecoveryPlan {
  RecoveryPlan(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(RecoveredRetry),
    records_to_append: List(record.LedgerRecord),
    cleanup_workspaces: List(CleanupRequest),
    outbox_to_replay: List(OutboxReplay),
    warnings: List(String),
    workflow_resumptions: List(RecoveredWorkflowRun),
  )
}

pub type RecoveryError {
  MissingOutboxPayload(outbox_id: String)
  InvalidRecordSemantics(reason: String)
  StepArtifactRecoveryFailed(detail: ArtifactRecoveryFailure)
  UnsafeWorkflowRecovery(reason: String)
  WorkspaceRecoveryFailed(reason: String)
}

type UnsafeRecoveryReason {
  RecoverySessionFactMissing
  RecoverySessionFactAmbiguous
  UnknownInterruptedStep(step_id: String)
  UnsafeInterruptedCommandStep(step_id: String)
}

type Build {
  Build(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(RecoveredRetry),
    record_bodies: List(record.RecordBody),
    cleanup_workspaces: List(CleanupRequest),
    warnings: List(String),
    auto_unparked_issue_ids: List(String),
  )
}

type OutboxRecovery {
  OutboxRecovery(
    outbox_to_replay: List(OutboxReplay),
    record_bodies: List(record.RecordBody),
    warnings: List(String),
  )
}

pub fn known_issue_ids(projection: projection.Projection) -> List(String) {
  projection.known_issue_ids(projection)
}

pub fn known_task_refs(
  projection: projection.Projection,
) -> List(record.TaskRefFields) {
  projection.known_task_refs(projection)
}

fn default_session_recovery_config() -> SessionRecoveryConfig {
  SessionRecoveryConfig(enabled: False, recovery_prompt: "")
}

pub fn workflow_candidates(
  projection: projection.Projection,
) -> List(WorkflowRecoveryCandidate) {
  let recovery_evidence_by_run = step_recovery_evidence_by_run(projection)

  projection.active_workflow_runs(projection)
  |> list.filter_map(fn(entry) {
    let #(run_id, status) = entry
    case status {
      projection.WorkflowRunActive(
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
        _,
      ) ->
        Ok(WorkflowRecoveryCandidate(
          run_id: run_id,
          workflow_id: workflow_id,
          workflow_fingerprint: workflow_fingerprint,
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          task_ref: workflow_task_ref_or_legacy(
            projection,
            run_id,
            issue_id,
            issue_identifier,
          ),
          issue_fingerprint: issue_fingerprint,
          observed_updated_at_ms: observed_updated_at_ms,
          run_root: run_root,
          recovery_evidence: recovery_evidence_or_default(
            recovery_evidence_by_run,
            run_id,
          ),
          attempts: attempts_for_run(projection, run_id),
          contract_input_manifest: projection.workflow_input_manifest(
            projection,
            run_id,
          )
            |> option.map(projection_manifest_to_recovered),
          contract_output_manifest: projection.workflow_output_manifest(
            projection,
            run_id,
          )
            |> option.map(projection_manifest_to_recovered),
        ))
      _ -> Error(Nil)
    }
  })
}

fn projection_manifest_to_recovered(
  manifest: projection.WorkflowContractManifestRef,
) -> RecoveredContractManifest {
  RecoveredContractManifest(
    ref: manifest.artifact_ref,
    sha256: manifest.artifact_sha256,
    bytes: manifest.artifact_bytes,
  )
}

pub fn finalize_workflow_candidates(
  projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
) -> Result(WorkflowFinalization, RecoveryError) {
  finalize_workflow_candidates_with_config_and_mode(
    projection,
    candidates,
    observations,
    artifact_store,
    now_ms,
    None,
    default_session_recovery_config(),
    ResumeRecoveredWorkflows,
  )
}

pub fn finalize_workflow_candidates_with_config(
  projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
  config: config_types.EffectiveConfig,
) -> Result(WorkflowFinalization, RecoveryError) {
  finalize_workflow_candidates_with_config_and_mode(
    projection,
    candidates,
    observations,
    artifact_store,
    now_ms,
    Some(config),
    SessionRecoveryConfig(
      enabled: config.pi.session_persistence.enabled,
      recovery_prompt: config.pi.session_persistence.recovery_prompt,
    ),
    ResumeRecoveredWorkflows,
  )
}

pub fn finalize_retry_step_candidates_with_config(
  projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
  config: config_types.EffectiveConfig,
) -> Result(WorkflowFinalization, RecoveryError) {
  finalize_workflow_candidates_with_config_and_mode(
    projection,
    candidates,
    observations,
    artifact_store,
    now_ms,
    Some(config),
    SessionRecoveryConfig(
      enabled: config.pi.session_persistence.enabled,
      recovery_prompt: config.pi.session_persistence.recovery_prompt,
    ),
    ResumeExplicitRetryStep,
  )
}

pub fn finalize_workflow_candidates_with_mode(
  projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
  mode: WorkflowRecoveryMode,
) -> Result(WorkflowFinalization, RecoveryError) {
  finalize_workflow_candidates_with_config_and_mode(
    projection,
    candidates,
    observations,
    artifact_store,
    now_ms,
    None,
    default_session_recovery_config(),
    mode,
  )
}

fn finalize_workflow_candidates_with_config_and_mode(
  _projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
  effective_config: Option(config_types.EffectiveConfig),
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
) -> Result(WorkflowFinalization, RecoveryError) {
  finalize_workflow_candidates_loop(
    candidates,
    observations,
    artifact_store,
    now_ms,
    effective_config,
    session_recovery,
    mode,
    [],
    [],
    [],
    [],
  )
}

pub fn plan(
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  refreshed_issues: List(tracker_issue.Issue),
  now_ms: Int,
) -> Result(RecoveryPlan, RecoveryError) {
  let outbox_recovery = replayable_outbox(projection)
  let issue_by_id = recovery_policy.issues_by_id(refreshed_issues)
  let base = recovery_policy.new_state(config)
  let build =
    Build(
      runtime: restore_counters(base, projection),
      retry_timers: [],
      record_bodies: list.reverse(outbox_recovery.record_bodies),
      cleanup_workspaces: [],
      warnings: list.reverse(outbox_recovery.warnings),
      auto_unparked_issue_ids: [],
    )
  let build = restore_parked(build, projection, issue_by_id)
  let build = restore_retries(build, projection, config, issue_by_id, now_ms)
  let build =
    recover_interrupted_runs(build, projection, config, issue_by_id, now_ms)
  Ok(
    RecoveryPlan(
      runtime: build.runtime,
      retry_timers: list.reverse(build.retry_timers),
      records_to_append: ledger_records(
        now_ms,
        list.reverse(build.record_bodies),
      ),
      cleanup_workspaces: list.reverse(build.cleanup_workspaces),
      outbox_to_replay: outbox_recovery.outbox_to_replay,
      warnings: list.reverse(build.warnings),
      workflow_resumptions: [],
    ),
  )
}

pub fn describe_error(error: RecoveryError) -> String {
  case error {
    MissingOutboxPayload(outbox_id) -> "outbox_payload_missing:" <> outbox_id
    InvalidRecordSemantics(reason) -> reason
    StepArtifactRecoveryFailed(detail) ->
      artifact_recovery_failure_message(detail)
    UnsafeWorkflowRecovery(reason) -> "unsafe_workflow_recovery:" <> reason
    WorkspaceRecoveryFailed(reason) -> "workspace_recovery_failed:" <> reason
  }
}

fn unsafe_recovery_reason_to_string(reason: UnsafeRecoveryReason) -> String {
  case reason {
    RecoverySessionFactMissing -> "recovery_session_fact_missing"
    RecoverySessionFactAmbiguous -> "recovery_session_fact_ambiguous"
    UnknownInterruptedStep(step_id) -> "unknown_interrupted_step:" <> step_id
    UnsafeInterruptedCommandStep(step_id) ->
      "unsafe_interrupted_command_step:" <> step_id
  }
}

pub fn workflow_recovery_diagnostic_reason(
  diagnostic: WorkflowRecoveryDiagnostic,
) -> String {
  case diagnostic {
    ArtifactRecoveryDiagnostic(..) -> "artifact_recovery_failed"
  }
}

pub fn workflow_recovery_diagnostic_message(
  diagnostic: WorkflowRecoveryDiagnostic,
) -> String {
  case diagnostic {
    ArtifactRecoveryDiagnostic(detail: detail, ..) ->
      artifact_recovery_failure_message(detail)
  }
}

pub fn workflow_recovery_diagnostic_record_body(
  diagnostic: WorkflowRecoveryDiagnostic,
) -> record.RecordBody {
  case diagnostic {
    ArtifactRecoveryDiagnostic(run_id, workflow_id, issue_id, detail) ->
      record.WorkflowRunDiagnostic(
        run_id,
        workflow_id,
        issue_id,
        artifact_recovery_failure_message(detail),
      )
  }
}

fn workflow_task_ref_or_legacy(
  projection: projection.Projection,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
) -> record.TaskRefFields {
  case projection.workflow_task_ref(projection, run_id) {
    Ok(task_ref) -> task_ref
    Error(Nil) ->
      record.linear_task_ref_fields(issue_id, Some(issue_identifier), None)
  }
}

fn recovery_evidence_or_default(
  evidence_by_run: Dict(String, workflow_outcome.RecoveryEvidence),
  run_id: String,
) -> workflow_outcome.RecoveryEvidence {
  case dict.get(evidence_by_run, run_id) {
    Ok(evidence) -> evidence
    Error(Nil) -> workflow_outcome.NoStepRecovery
  }
}

fn observation_or_issue_unavailable(
  observations: Dict(String, CurrentWorkflowObservation),
  run_id: String,
) -> CurrentWorkflowObservation {
  case dict.get(observations, run_id) {
    Ok(observation) -> observation
    Error(Nil) -> IssueUnavailable
  }
}

fn attempts_for_run(
  projection: projection.Projection,
  run_id: String,
) -> List(projection.StepAttemptStatus) {
  projection.step_attempts
  |> dict.values
  |> list.filter(fn(status) {
    let #(status_run_id, _, _) = attempt_identity(status)
    status_run_id == run_id
  })
}

pub fn step_recovery_evidence_for_run(
  projection: projection.Projection,
  run_id: String,
) -> workflow_outcome.RecoveryEvidence {
  step_recovery_evidence_by_run(projection)
  |> recovery_evidence_or_default(run_id)
}

fn step_recovery_evidence_by_run(
  projection: projection.Projection,
) -> Dict(String, workflow_outcome.RecoveryEvidence) {
  projection.step_recoveries
  |> dict.values
  |> list.fold(dict.new(), fn(evidence_by_run, status) {
    let run_id = step_recovery_status_run_id(status)
    let evidence = recovery_evidence_or_default(evidence_by_run, run_id)
    dict.insert(
      evidence_by_run,
      run_id,
      combine_recovery_evidence(evidence, recovery_evidence_from_status(status)),
    )
  })
}

fn step_recovery_status_run_id(
  status: projection.StepRecoveryStatus,
) -> String {
  case status {
    projection.StepRecoveryStartedStatus(run_id: run_id, ..)
    | projection.StepRecoveryFinishedStatus(run_id: run_id, ..) -> run_id
  }
}

fn recovery_evidence_from_status(
  status: projection.StepRecoveryStatus,
) -> workflow_outcome.RecoveryEvidence {
  case status {
    projection.StepRecoveryFinishedStatus(result: "recheck", ..) ->
      workflow_outcome.StepRecoveryRecheckRequested
    projection.StepRecoveryStartedStatus(..)
    | projection.StepRecoveryFinishedStatus(..) ->
      workflow_outcome.StepRecoveryRan
  }
}

fn combine_recovery_evidence(
  left: workflow_outcome.RecoveryEvidence,
  right: workflow_outcome.RecoveryEvidence,
) -> workflow_outcome.RecoveryEvidence {
  case left, right {
    workflow_outcome.StepRecoveryRecheckRequested, _
    | _, workflow_outcome.StepRecoveryRecheckRequested
    -> workflow_outcome.StepRecoveryRecheckRequested
    workflow_outcome.StepRecoveryRan, _ | _, workflow_outcome.StepRecoveryRan ->
      workflow_outcome.StepRecoveryRan
    _, _ -> workflow_outcome.NoStepRecovery
  }
}

fn finalize_workflow_candidates_loop(
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  store: artifact_store.Store,
  now_ms: Int,
  effective_config: Option(config_types.EffectiveConfig),
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
  record_bodies: List(record.RecordBody),
  resumptions: List(RecoveredWorkflowRun),
  warnings: List(String),
  diagnostics: List(WorkflowRecoveryDiagnostic),
) -> Result(WorkflowFinalization, RecoveryError) {
  case candidates {
    [] ->
      Ok(WorkflowFinalization(
        records_to_append: ledger_records(now_ms, list.reverse(record_bodies)),
        resumptions: list.reverse(resumptions),
        warnings: list.reverse(warnings),
        diagnostics: list.reverse(diagnostics),
      ))
    [candidate, ..rest] -> {
      let observation =
        observation_or_issue_unavailable(observations, candidate.run_id)
      use finalized <- result.try(finalize_one_workflow_candidate(
        candidate,
        observation,
        store,
        effective_config,
        session_recovery,
        mode,
      ))
      let #(bodies, resumption, candidate_warnings, candidate_diagnostics) =
        finalized
      finalize_workflow_candidates_loop(
        rest,
        observations,
        store,
        now_ms,
        effective_config,
        session_recovery,
        mode,
        list.append(list.reverse(bodies), record_bodies),
        append_optional_resumption(resumptions, resumption),
        list.append(list.reverse(candidate_warnings), warnings),
        list.append(list.reverse(candidate_diagnostics), diagnostics),
      )
    }
  }
}

fn finalize_one_workflow_candidate(
  candidate: WorkflowRecoveryCandidate,
  observation: CurrentWorkflowObservation,
  store: artifact_store.Store,
  effective_config: Option(config_types.EffectiveConfig),
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
) -> Result(
  #(
    List(record.RecordBody),
    Option(RecoveredWorkflowRun),
    List(String),
    List(WorkflowRecoveryDiagnostic),
  ),
  RecoveryError,
) {
  case observation {
    IssueUnavailable ->
      Ok(
        #(
          park_candidate_bodies(
            candidate,
            candidate.issue_identifier,
            "issue_unavailable",
            candidate.issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [
            "workflow_recovery_parked_issue_unavailable:" <> candidate.run_id,
          ],
          [],
        ),
      )
    WorkflowUnavailable(reason) ->
      Ok(
        #(
          park_candidate_bodies(
            candidate,
            candidate.issue_identifier,
            "workflow_unavailable:" <> reason,
            candidate.issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [
            "workflow_recovery_parked_workflow_unavailable:" <> candidate.run_id,
          ],
          [],
        ),
      )
    CurrentWorkflow(
      issue,
      workflow_id,
      workflow_fingerprint,
      issue_fingerprint,
      dag,
      workspace_root,
    ) ->
      case
        workflow_attempt.recovery_drift_reason(
          candidate.run_id,
          candidate.workflow_id,
          workflow_id,
          candidate.workflow_fingerprint,
          workflow_fingerprint,
          candidate.issue_fingerprint,
          issue_fingerprint,
        )
      {
        Some(#(reason, warning)) ->
          Ok(
            #(
              park_candidate_bodies(
                candidate,
                issue.identifier,
                reason,
                issue_fingerprint,
                candidate.observed_updated_at_ms,
              ),
              None,
              [warning],
              [],
            ),
          )
        None ->
          case mode {
            ParkRecoveredWorkflows ->
              Ok(
                #(
                  park_candidate_bodies(
                    candidate,
                    issue.identifier,
                    "workflow_recovery_disabled",
                    issue_fingerprint,
                    candidate.observed_updated_at_ms,
                  ),
                  None,
                  [
                    "workflow_recovery_parked_disabled:" <> candidate.run_id,
                  ],
                  [],
                ),
              )
            ResumeRecoveredWorkflows | ResumeExplicitRetryStep ->
              case
                recovery_issue_state_drift_for_mode(
                  mode,
                  effective_config,
                  issue,
                  candidate.run_id,
                )
              {
                Some(#(reason, warning)) ->
                  Ok(
                    #(
                      park_candidate_bodies(
                        candidate,
                        issue.identifier,
                        reason,
                        issue_fingerprint,
                        candidate.observed_updated_at_ms,
                      ),
                      None,
                      [warning],
                      [],
                    ),
                  )
                None ->
                  finalize_resumable_workflow_candidate(
                    candidate,
                    issue,
                    issue_fingerprint,
                    store,
                    dag,
                    workspace_root,
                    session_recovery,
                  )
              }
          }
      }
  }
}

fn recovery_issue_state_drift_for_mode(
  mode: WorkflowRecoveryMode,
  effective_config: Option(config_types.EffectiveConfig),
  issue: tracker_issue.Issue,
  run_id: String,
) -> Option(#(String, String)) {
  case mode {
    ResumeRecoveredWorkflows ->
      workflow_attempt.recovery_issue_state_drift(
        effective_config,
        issue,
        run_id,
      )
    ResumeExplicitRetryStep ->
      workflow_attempt.recovery_terminal_issue_state_drift(
        effective_config,
        issue,
        run_id,
      )
    ParkRecoveredWorkflows -> None
  }
}

fn finalize_resumable_workflow_candidate(
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  issue_fingerprint: String,
  store: artifact_store.Store,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  session_recovery: SessionRecoveryConfig,
) -> Result(
  #(
    List(record.RecordBody),
    Option(RecoveredWorkflowRun),
    List(String),
    List(WorkflowRecoveryDiagnostic),
  ),
  RecoveryError,
) {
  case
    recover_completed_attempts(
      candidate,
      issue,
      store,
      dag,
      workspace_root,
      session_recovery,
    )
  {
    Ok(recovered) -> {
      let #(resumption, bodies) = recovered
      Ok(#(bodies, Some(resumption), [], []))
    }
    Error(StepArtifactRecoveryFailed(detail)) ->
      Ok(
        #(
          artifact_recovery_park_candidate_bodies(
            candidate,
            issue.identifier,
            detail,
            issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [artifact_recovery_failure_warning(candidate.run_id, detail)],
          [artifact_recovery_diagnostic(candidate, detail)],
        ),
      )
    Error(UnsafeWorkflowRecovery(reason)) ->
      Ok(
        #(
          park_candidate_bodies(
            candidate,
            issue.identifier,
            reason,
            issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [
            "workflow_recovery_parked_" <> reason <> ":" <> candidate.run_id,
          ],
          [],
        ),
      )
    Error(WorkspaceRecoveryFailed(reason)) ->
      Ok(
        #(
          park_candidate_bodies(
            candidate,
            issue.identifier,
            "workspace_recovery_failed",
            issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [
            "workflow_recovery_parked_workspace_recovery_failed:"
            <> candidate.run_id
            <> ":"
            <> reason,
          ],
          [],
        ),
      )
    Error(error) -> Error(error)
  }
}

fn recover_completed_attempts(
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  store: artifact_store.Store,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  session_recovery: SessionRecoveryConfig,
) -> Result(#(RecoveredWorkflowRun, List(record.RecordBody)), RecoveryError) {
  let attempts = list.sort(candidate.attempts, by: compare_attempts_for_replay)
  recover_attempts_loop(
    attempts,
    candidate,
    issue,
    store,
    dag,
    workspace_root,
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    [],
    session_recovery,
  )
}

fn recover_step_artifact(
  store: artifact_store.Store,
  step_id: String,
  artifact_ref: String,
  expected_sha256: String,
) -> Result(step_artifact.StepArtifact, RecoveryError) {
  case artifact_store.read_artifact_unverified(store, artifact_ref) {
    Error(error) ->
      Error(
        StepArtifactRecoveryFailed(artifact_read_failure_detail(
          step_id,
          artifact_ref,
          error,
        )),
      )
    Ok(contents) -> {
      let current_sha256 = hash.sha256_hex(contents)
      case current_sha256 == expected_sha256 {
        False ->
          Error(
            StepArtifactRecoveryFailed(ArtifactRecoveryFailure(
              step_id: step_id,
              artifact_ref: artifact_ref,
              reason: "sha_mismatch",
              expected_sha256: Some(expected_sha256),
              current_sha256: Some(current_sha256),
            )),
          )
        True ->
          artifact_store.decode_step_artifact_contents(contents)
          |> result.map_error(fn(error) {
            StepArtifactRecoveryFailed(artifact_decode_failure_detail(
              step_id,
              artifact_ref,
              error,
            ))
          })
      }
    }
  }
}

fn artifact_read_failure_detail(
  step_id: String,
  artifact_ref: String,
  error: artifact_store.ArtifactError,
) -> ArtifactRecoveryFailure {
  ArtifactRecoveryFailure(
    step_id: step_id,
    artifact_ref: artifact_ref,
    reason: artifact_read_failure_reason(error),
    expected_sha256: None,
    current_sha256: None,
  )
}

fn artifact_decode_failure_detail(
  step_id: String,
  artifact_ref: String,
  error: artifact_store.ArtifactError,
) -> ArtifactRecoveryFailure {
  ArtifactRecoveryFailure(
    step_id: step_id,
    artifact_ref: artifact_ref,
    reason: artifact_decode_failure_reason(error),
    expected_sha256: None,
    current_sha256: None,
  )
}

fn artifact_read_failure_reason(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.MissingStepArtifact(_) -> "missing"
    artifact_store.ArtifactIo(_) -> "unreadable"
    artifact_store.InvalidArtifactRef(_) -> "invalid_ref"
    artifact_store.DecodeArtifactFailed(_) -> "invalid_json"
    artifact_store.CorruptStepArtifact(_) -> "sha_mismatch"
    artifact_store.ArtifactWriteFailed(_) -> "read_failed"
    artifact_store.DirectorySyncUnsupported(_) -> "read_failed"
  }
}

fn artifact_decode_failure_reason(
  error: artifact_store.ArtifactError,
) -> String {
  case error {
    artifact_store.DecodeArtifactFailed(_) -> "invalid_json"
    _ -> artifact_read_failure_reason(error)
  }
}

fn artifact_recovery_failure_message(
  detail: ArtifactRecoveryFailure,
) -> String {
  "artifact_recovery_failed: step_id="
  <> detail.step_id
  <> " artifact_ref="
  <> sanitized_artifact_ref(detail.artifact_ref)
  <> " reason="
  <> detail.reason
  <> optional_detail_field("expected_sha256", detail.expected_sha256)
  <> optional_detail_field("current_sha256", detail.current_sha256)
}

fn artifact_recovery_failure_warning(
  run_id: String,
  detail: ArtifactRecoveryFailure,
) -> String {
  "workflow_recovery_parked_artifact_recovery_failed:"
  <> run_id
  <> ":"
  <> artifact_recovery_failure_message(detail)
}

fn artifact_recovery_diagnostic(
  candidate: WorkflowRecoveryCandidate,
  detail: ArtifactRecoveryFailure,
) -> WorkflowRecoveryDiagnostic {
  ArtifactRecoveryDiagnostic(
    run_id: candidate.run_id,
    workflow_id: candidate.workflow_id,
    issue_id: candidate.issue_id,
    detail: detail,
  )
}

fn optional_detail_field(name: String, value: Option(String)) -> String {
  case value {
    Some(value) -> " " <> name <> "=" <> value
    None -> ""
  }
}

fn sanitized_artifact_ref(ref: String) -> String {
  let trimmed = string.trim(ref)
  case trimmed == "" {
    True -> "<empty>"
    False ->
      case artifact_ref_looks_local(trimmed) {
        True -> "<redacted-local-artifact-ref>"
        False -> trimmed
      }
  }
}

fn artifact_ref_looks_local(ref: String) -> Bool {
  let lower = string.lowercase(ref)
  string.starts_with(ref, "/")
  || string.starts_with(ref, "~")
  || string.starts_with(ref, "\\")
  || string.contains(lower, "://")
  || string.starts_with(lower, "file:")
  || has_forward_parent_segment(ref)
  || has_backslash_parent_segment(ref)
  || has_windows_absolute_prefix(ref)
  || contains_control_character(ref)
}

fn has_forward_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn has_backslash_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "..\\")
  || string.ends_with(value, "\\..")
  || string.contains(value, "\\..\\")
}

fn has_windows_absolute_prefix(value: String) -> Bool {
  string.length(value) >= 3
  && is_ascii_letter(string.slice(value, 0, 1))
  && string.slice(value, 1, 1) == ":"
  && windows_separator(string.slice(value, 2, 1))
}

fn windows_separator(value: String) -> Bool {
  value == "\\" || value == "/"
}

fn contains_control_character(value: String) -> Bool {
  string.contains(value, "\n")
  || string.contains(value, "\r")
  || string.contains(value, "\t")
}

fn is_ascii_letter(value: String) -> Bool {
  case value {
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    _ -> False
  }
}

fn recover_attempts_loop(
  attempts: List(projection.StepAttemptStatus),
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  store: artifact_store.Store,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  artifacts: Dict(String, step_artifact.StepArtifact),
  workspaces: Dict(String, RecoveredWorkspaceSummary),
  next_indexes: Dict(String, Int),
  continuations: Dict(String, workflow_attempt.PiContinuation),
  bodies: List(record.RecordBody),
  session_recovery: SessionRecoveryConfig,
) -> Result(#(RecoveredWorkflowRun, List(record.RecordBody)), RecoveryError) {
  case attempts {
    [] -> {
      use _ <- result.try(validate_recovered_workflow_filesystem(
        candidate,
        dag,
        workspace_root,
        artifacts,
        workspaces,
      ))
      Ok(#(
        RecoveredWorkflowRun(
          issue: issue,
          run_id: candidate.run_id,
          workflow_id: candidate.workflow_id,
          workflow_fingerprint: candidate.workflow_fingerprint,
          run_root: candidate.run_root,
          recovery_evidence: candidate.recovery_evidence,
          completed_artifacts: artifacts,
          completed_workspaces: workspaces,
          next_attempt_indexes: next_indexes,
          pi_session_continuations: continuations,
          contract_input_manifest: candidate.contract_input_manifest,
          contract_output_manifest: candidate.contract_output_manifest,
        ),
        list.reverse(bodies),
      ))
    }
    [attempt, ..rest] -> {
      let #(run_id, step_id, attempt_index) = attempt_identity(attempt)
      let next_indexes =
        update_next_index(next_indexes, step_id, attempt_index + 1)
      case attempt {
        projection.StepAttemptFinishedStatus(
          workflow_id: workflow_id,
          outcome: outcome,
          artifact_ref: artifact_ref,
          artifact_sha256: artifact_sha256,
          workspace_name: workspace_name,
          workspace_path: workspace_path,
          run_root: run_root,
          source_workspace_name: source_workspace_name,
          source_workspace_path: source_workspace_path,
          ..,
        ) -> {
          use artifact <- result.try(recover_step_artifact(
            store,
            step_id,
            artifact_ref,
            artifact_sha256,
          ))
          let artifacts = dict.insert(artifacts, step_id, artifact)
          let run_root = case string.trim(run_root) == "" {
            True -> candidate.run_root
            False -> run_root
          }
          let workspaces = case
            outcome == "completed" || outcome == "failed_continued"
          {
            True -> {
              let workspace =
                RecoveredWorkspaceSummary(
                  workflow_id: workflow_id,
                  run_id: run_id,
                  run_root: run_root,
                  workspace_name: workspace_name,
                  path: workspace_path,
                  source_workspace_name: source_workspace_name,
                  source_workspace_path: source_workspace_path,
                  attempt_index: attempt_index,
                )
              dict.insert(workspaces, workspace_name, workspace)
            }
            False -> workspaces
          }
          recover_attempts_loop(
            rest,
            candidate,
            issue,
            store,
            dag,
            workspace_root,
            artifacts,
            workspaces,
            next_indexes,
            continuations,
            bodies,
            session_recovery,
          )
        }
        projection.StepAttemptPending(workflow_id: workflow_id, ..) ->
          case interrupted_step_is_safe_to_retry(dag, step_id) {
            Error(reason) ->
              Error(
                UnsafeWorkflowRecovery(unsafe_recovery_reason_to_string(reason)),
              )
            Ok(Nil) ->
              recover_attempts_loop(
                rest,
                candidate,
                issue,
                store,
                dag,
                workspace_root,
                artifacts,
                workspaces,
                next_indexes,
                continuations,
                [
                  record.StepAttemptInterrupted(
                    run_id,
                    workflow_id,
                    step_id,
                    attempt_index,
                    "daemon_restart",
                  ),
                  ..bodies
                ],
                session_recovery,
              )
          }
        projection.StepAttemptRunning(workflow_id: workflow_id, ..) ->
          recover_running_or_interrupted_attempt(
            attempt,
            rest,
            candidate,
            issue,
            store,
            dag,
            workspace_root,
            artifacts,
            workspaces,
            next_indexes,
            continuations,
            bodies,
            session_recovery,
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            "daemon_restart",
          )
        projection.StepAttemptInterruptedStatus(
          workflow_id: workflow_id,
          reason: reason,
          ..,
        ) ->
          recover_running_or_interrupted_attempt(
            attempt,
            rest,
            candidate,
            issue,
            store,
            dag,
            workspace_root,
            artifacts,
            workspaces,
            next_indexes,
            continuations,
            bodies,
            session_recovery,
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            reason,
          )
        _ ->
          recover_attempts_loop(
            rest,
            candidate,
            issue,
            store,
            dag,
            workspace_root,
            artifacts,
            workspaces,
            next_indexes,
            continuations,
            bodies,
            session_recovery,
          )
      }
    }
  }
}

fn recover_running_or_interrupted_attempt(
  attempt: projection.StepAttemptStatus,
  rest: List(projection.StepAttemptStatus),
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  store: artifact_store.Store,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  artifacts: Dict(String, step_artifact.StepArtifact),
  workspaces: Dict(String, RecoveredWorkspaceSummary),
  next_indexes: Dict(String, Int),
  continuations: Dict(String, workflow_attempt.PiContinuation),
  bodies: List(record.RecordBody),
  session_recovery: SessionRecoveryConfig,
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  interrupt_reason: String,
) -> Result(#(RecoveredWorkflowRun, List(record.RecordBody)), RecoveryError) {
  case attempt_continuation_capable(attempt) {
    True -> {
      use continuation <- result.try(build_continuation(
        attempt,
        candidate,
        issue,
        dag,
        workspace_root,
        session_recovery,
      ))
      recover_attempts_loop(
        rest,
        candidate,
        issue,
        store,
        dag,
        workspace_root,
        artifacts,
        workspaces,
        dict.insert(next_indexes, step_id, attempt_index),
        dict.insert(continuations, step_id, continuation),
        bodies,
        session_recovery,
      )
    }
    False ->
      case interrupted_step_is_safe_to_retry(dag, step_id) {
        Error(reason) ->
          Error(
            UnsafeWorkflowRecovery(unsafe_recovery_reason_to_string(reason)),
          )
        Ok(Nil) ->
          recover_attempts_loop(
            rest,
            candidate,
            issue,
            store,
            dag,
            workspace_root,
            artifacts,
            workspaces,
            next_indexes,
            continuations,
            [
              record.StepAttemptInterrupted(
                run_id,
                workflow_id,
                step_id,
                attempt_index,
                interrupt_reason,
              ),
              ..bodies
            ],
            session_recovery,
          )
      }
  }
}

fn build_continuation(
  attempt: projection.StepAttemptStatus,
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  session_recovery: SessionRecoveryConfig,
) -> Result(workflow_attempt.PiContinuation, RecoveryError) {
  case session_recovery.enabled {
    False ->
      Error(UnsafeWorkflowRecovery("recovery_session_persistence_disabled"))
    True ->
      case continuation_fields(attempt) {
        Error(reason) ->
          Error(
            UnsafeWorkflowRecovery(unsafe_recovery_reason_to_string(reason)),
          )
        Ok(fields) -> {
          let #(
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            session_id,
            session_file,
            fact_count,
          ) = fields
          case is_agent_step(dag, step_id) {
            False ->
              Error(UnsafeWorkflowRecovery(
                "unsafe_interrupted_command_step:" <> step_id,
              ))
            True -> {
              use _ <- result.try(validate_continuation_facts(
                workspace_root,
                candidate.run_root,
                workspace_path,
                session_id,
                session_file,
                fact_count,
              ))
              let attempt_context =
                workflow_attempt.StepAttemptContext(
                  run_id: candidate.run_id,
                  issue_id: candidate.issue_id,
                  issue_identifier: issue.identifier,
                  workflow_id: candidate.workflow_id,
                  workflow_fingerprint: candidate.workflow_fingerprint,
                  step_id: step_id,
                  workspace_name: workspace_name,
                  attempt_index: attempt_index,
                  workspace_path: workspace_path,
                  continuation_capable: True,
                  continuation_session_file: Some(session_file),
                )
              let recovery_prompt =
                workflow_attempt.render_recovery_prompt(
                  session_recovery.recovery_prompt,
                  attempt_context,
                )
              Ok(workflow_attempt.PiContinuation(
                run_id: candidate.run_id,
                issue_id: candidate.issue_id,
                issue_identifier: issue.identifier,
                workflow_id: candidate.workflow_id,
                workflow_fingerprint: candidate.workflow_fingerprint,
                step_id: step_id,
                workspace_name: workspace_name,
                attempt_index: attempt_index,
                workspace_path: workspace_path,
                session_id: session_id,
                session_file: session_file,
                recovery_prompt: recovery_prompt,
              ))
            }
          }
        }
      }
  }
}

fn attempt_continuation_capable(status: projection.StepAttemptStatus) -> Bool {
  case status {
    projection.StepAttemptRunning(continuation_capable: value, ..) -> value
    projection.StepAttemptInterruptedStatus(continuation_capable: value, ..) ->
      value
    _ -> False
  }
}

fn continuation_fields(
  status: projection.StepAttemptStatus,
) -> Result(
  #(String, Int, String, String, String, String, Int),
  UnsafeRecoveryReason,
) {
  case status {
    projection.StepAttemptRunning(
      step_id: step_id,
      attempt_index: attempt_index,
      workspace_name: workspace_name,
      workspace_path: workspace_path,
      pi_session_id: pi_session_id,
      pi_session_file: pi_session_file,
      pi_session_fact_count: fact_count,
      ..,
    ) ->
      require_session_fields(
        step_id,
        attempt_index,
        workspace_name,
        workspace_path,
        pi_session_id,
        pi_session_file,
        fact_count,
      )
    projection.StepAttemptInterruptedStatus(
      step_id: step_id,
      attempt_index: attempt_index,
      workspace_name: workspace_name,
      workspace_path: workspace_path,
      pi_session_id: pi_session_id,
      pi_session_file: pi_session_file,
      pi_session_fact_count: fact_count,
      ..,
    ) ->
      require_session_fields(
        step_id,
        attempt_index,
        workspace_name,
        workspace_path,
        pi_session_id,
        pi_session_file,
        fact_count,
      )
    _ -> Error(RecoverySessionFactMissing)
  }
}

fn require_session_fields(
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  workspace_path: String,
  pi_session_id: Option(String),
  pi_session_file: Option(String),
  fact_count: Int,
) -> Result(
  #(String, Int, String, String, String, String, Int),
  UnsafeRecoveryReason,
) {
  case fact_count {
    0 -> Error(RecoverySessionFactMissing)
    1 ->
      case pi_session_id, pi_session_file {
        Some(session_id), Some(session_file) ->
          Ok(#(
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            session_id,
            session_file,
            fact_count,
          ))
        _, _ -> Error(RecoverySessionFactMissing)
      }
    _ -> Error(RecoverySessionFactAmbiguous)
  }
}

fn validate_continuation_facts(
  workspace_root: String,
  run_root: String,
  workspace_path: String,
  session_id: String,
  session_file: String,
  fact_count: Int,
) -> Result(Nil, RecoveryError) {
  case fact_count != 1 {
    True -> Error(UnsafeWorkflowRecovery("recovery_session_fact_ambiguous"))
    False ->
      case string.trim(session_id) == "" {
        True -> Error(UnsafeWorkflowRecovery("recovery_session_fact_missing"))
        False ->
          case string.trim(session_file) == "" {
            True ->
              Error(UnsafeWorkflowRecovery("recovery_session_file_missing"))
            False -> {
              use _ <- result.try(validate_continuation_workspace(
                workspace_root,
                run_root,
                workspace_path,
              ))
              case simplifile.is_file(session_file) {
                Ok(True) -> Ok(Nil)
                _ ->
                  Error(UnsafeWorkflowRecovery("recovery_session_file_missing"))
              }
            }
          }
      }
  }
}

fn validate_continuation_workspace(
  workspace_root: String,
  run_root: String,
  workspace_path: String,
) -> Result(Nil, RecoveryError) {
  let root_abs = path.absolute_or_original(workspace_root)
  let run_root_abs = path.absolute_or_original(run_root)
  let workspace_abs = path.absolute_or_original(workspace_path)
  case
    string.trim(workspace_abs) == ""
    || workspace_abs == root_abs
    || workspace_abs == run_root_abs
  {
    True -> Error(UnsafeWorkflowRecovery("recovery_workspace_unsafe"))
    False ->
      case
        path.contains(root_abs, workspace_abs)
        && path.contains(root_abs, run_root_abs)
        && path.contains(run_root_abs, workspace_abs)
      {
        False -> Error(UnsafeWorkflowRecovery("recovery_workspace_unsafe"))
        True ->
          case simplifile.is_directory(workspace_abs) {
            Ok(True) -> Ok(Nil)
            _ -> Error(UnsafeWorkflowRecovery("recovery_workspace_missing"))
          }
      }
  }
}

fn is_agent_step(dag: workflow_dag.WorkflowDag, step_id: String) -> Bool {
  case workflow_dag.step_by_id(dag, step_id) {
    Ok(workflow_dag.WorkflowStep(kind: workflow_dag.AgentStep(_, _), ..)) ->
      True
    _ -> False
  }
}

fn interrupted_step_is_safe_to_retry(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> Result(Nil, UnsafeRecoveryReason) {
  case workflow_dag.step_by_id(dag, step_id) {
    Error(Nil) -> Error(UnknownInterruptedStep(step_id))
    Ok(step) ->
      case step.kind {
        workflow_dag.CommandStep(_, _) ->
          Error(UnsafeInterruptedCommandStep(step_id))
        workflow_dag.AgentStep(_, _) -> Ok(Nil)
      }
  }
}

fn validate_recovered_workflow_filesystem(
  candidate: WorkflowRecoveryCandidate,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  artifacts: Dict(String, step_artifact.StepArtifact),
  workspaces: Dict(String, RecoveredWorkspaceSummary),
) -> Result(Nil, RecoveryError) {
  use roots <- result.try(validate_recovered_run_root(candidate, workspace_root))
  let #(root_abs, run_root_abs) = roots
  use _ <- result.try(validate_completed_workspace_summaries(
    dict.values(workspaces),
    candidate,
    dag,
    root_abs,
    run_root_abs,
  ))
  validate_pending_source_workspaces(dag.steps, dag, artifacts, workspaces)
}

fn validate_recovered_run_root(
  candidate: WorkflowRecoveryCandidate,
  workspace_root: String,
) -> Result(#(String, String), RecoveryError) {
  let root_abs = path.absolute_or_original(workspace_root)
  let run_root_abs = path.absolute_or_original(candidate.run_root)
  case
    string.trim(run_root_abs) == ""
    || run_root_abs == root_abs
    || !path.contains(root_abs, run_root_abs)
  {
    True ->
      Error(WorkspaceRecoveryFailed("invalid_run_root:" <> candidate.run_id))
    False -> Ok(#(root_abs, run_root_abs))
  }
}

fn validate_completed_workspace_summaries(
  workspaces: List(RecoveredWorkspaceSummary),
  candidate: WorkflowRecoveryCandidate,
  dag: workflow_dag.WorkflowDag,
  root_abs: String,
  run_root_abs: String,
) -> Result(Nil, RecoveryError) {
  case workspaces {
    [] -> Ok(Nil)
    [workspace, ..rest] -> {
      use _ <- result.try(validate_completed_workspace_summary(
        workspace,
        candidate,
        dag,
        root_abs,
        run_root_abs,
      ))
      validate_completed_workspace_summaries(
        rest,
        candidate,
        dag,
        root_abs,
        run_root_abs,
      )
    }
  }
}

fn validate_completed_workspace_summary(
  workspace: RecoveredWorkspaceSummary,
  candidate: WorkflowRecoveryCandidate,
  dag: workflow_dag.WorkflowDag,
  root_abs: String,
  run_root_abs: String,
) -> Result(Nil, RecoveryError) {
  let workspace_run_root_abs = path.absolute_or_original(workspace.run_root)
  let workspace_path_abs = path.absolute_or_original(workspace.path)
  case
    workspace.workflow_id == candidate.workflow_id
    && workspace.run_id == candidate.run_id
    && workspace_run_root_abs == run_root_abs
    && workspace_path_abs != run_root_abs
    && path.contains(run_root_abs, workspace_path_abs)
    && path.contains(root_abs, workspace_path_abs)
    && dag_has_workspace_name(dag.steps, workspace.workspace_name)
  {
    True ->
      validate_existing_recovered_directory(
        workspace_path_abs,
        "missing_workspace:" <> workspace.workspace_name,
      )
    False ->
      Error(WorkspaceRecoveryFailed(
        "invalid_workspace:" <> workspace.workspace_name,
      ))
  }
}

fn validate_pending_source_workspaces(
  steps: List(workflow_dag.WorkflowStep),
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, step_artifact.StepArtifact),
  workspaces: Dict(String, RecoveredWorkspaceSummary),
) -> Result(Nil, RecoveryError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(validate_pending_step_source(
        step,
        dag,
        artifacts,
        workspaces,
      ))
      validate_pending_source_workspaces(rest, dag, artifacts, workspaces)
    }
  }
}

fn validate_pending_step_source(
  step: workflow_dag.WorkflowStep,
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, step_artifact.StepArtifact),
  workspaces: Dict(String, RecoveredWorkspaceSummary),
) -> Result(Nil, RecoveryError) {
  case dict.has_key(artifacts, step.id), step.workspace.from {
    True, _ -> Ok(Nil)
    False, None -> Ok(Nil)
    False, Some(source) ->
      case dict.get(workspaces, source) {
        Ok(workspace) ->
          validate_existing_recovered_directory(
            path.absolute_or_original(workspace.path),
            "missing_source_workspace:" <> source <> ":for_step:" <> step.id,
          )
        Error(Nil) ->
          case
            source_workspace_can_be_produced_later(
              source,
              step.depends_on,
              dag,
              artifacts,
              [],
            )
          {
            True -> Ok(Nil)
            False ->
              Error(WorkspaceRecoveryFailed(
                "missing_source_workspace:" <> source <> ":for_step:" <> step.id,
              ))
          }
      }
  }
}

fn source_workspace_can_be_produced_later(
  source: String,
  dependency_ids: List(String),
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, step_artifact.StepArtifact),
  seen: List(String),
) -> Bool {
  case dependency_ids {
    [] -> False
    [dependency_id, ..rest] -> {
      case list.contains(seen, dependency_id) {
        True ->
          source_workspace_can_be_produced_later(
            source,
            rest,
            dag,
            artifacts,
            seen,
          )
        False ->
          case workflow_dag.step_by_id(dag, dependency_id) {
            Error(Nil) ->
              source_workspace_can_be_produced_later(
                source,
                rest,
                dag,
                artifacts,
                [dependency_id, ..seen],
              )
            Ok(step) -> {
              let step_is_pending = !dict.has_key(artifacts, step.id)
              case step_is_pending && step.workspace.name == source {
                True -> True
                False ->
                  source_workspace_can_be_produced_later(
                    source,
                    list.append(step.depends_on, rest),
                    dag,
                    artifacts,
                    [dependency_id, ..seen],
                  )
              }
            }
          }
      }
    }
  }
}

fn validate_existing_recovered_directory(
  path_abs: String,
  reason: String,
) -> Result(Nil, RecoveryError) {
  case simplifile.is_directory(path_abs) {
    Ok(True) -> Ok(Nil)
    _ -> Error(WorkspaceRecoveryFailed(reason))
  }
}

fn dag_has_workspace_name(
  steps: List(workflow_dag.WorkflowStep),
  workspace_name: String,
) -> Bool {
  case steps {
    [] -> False
    [step, ..rest] ->
      step.workspace.name == workspace_name
      || dag_has_workspace_name(rest, workspace_name)
  }
}

fn artifact_recovery_park_candidate_bodies(
  candidate: WorkflowRecoveryCandidate,
  issue_identifier: String,
  detail: ArtifactRecoveryFailure,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
) -> List(record.RecordBody) {
  [
    record.IssueParkedV2(
      candidate.issue_id,
      issue_identifier,
      "artifact_recovery_failed",
      "explicit_unpark_only",
      issue_fingerprint,
      observed_updated_at_ms,
    ),
    ..interrupt_candidate_bodies(
      candidate,
      artifact_recovery_failure_message(detail),
    )
  ]
}

fn park_candidate_bodies(
  candidate: WorkflowRecoveryCandidate,
  issue_identifier: String,
  reason: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
) -> List(record.RecordBody) {
  [
    record.IssueParkedV2(
      candidate.issue_id,
      issue_identifier,
      reason,
      "explicit_unpark_only",
      issue_fingerprint,
      observed_updated_at_ms,
    ),
    ..interrupt_candidate_bodies(candidate, reason)
  ]
}

fn interrupt_candidate_bodies(
  candidate: WorkflowRecoveryCandidate,
  reason: String,
) -> List(record.RecordBody) {
  [
    record.WorkflowRunInterrupted(
      candidate.run_id,
      candidate.workflow_id,
      candidate.issue_id,
      reason,
    ),
    ..unfinished_attempt_bodies(candidate, reason, "interrupt")
  ]
}

fn unfinished_attempt_bodies(
  candidate: WorkflowRecoveryCandidate,
  reason: String,
  mode: String,
) -> List(record.RecordBody) {
  candidate.attempts
  |> list.filter(is_unfinished_attempt)
  |> list.map(fn(status) {
    let #(run_id, step_id, attempt_index) = attempt_identity(status)
    let workflow_id = attempt_workflow_id(status)
    case mode {
      "supersede" ->
        record.StepAttemptSuperseded(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          attempt_index + 1,
          reason,
        )
      _ ->
        record.StepAttemptInterrupted(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          reason,
        )
    }
  })
}

fn is_unfinished_attempt(status: projection.StepAttemptStatus) -> Bool {
  case status {
    projection.StepAttemptPending(..) | projection.StepAttemptRunning(..) ->
      True
    _ -> False
  }
}

fn append_optional_resumption(
  resumptions: List(RecoveredWorkflowRun),
  resumption: Option(RecoveredWorkflowRun),
) -> List(RecoveredWorkflowRun) {
  case resumption {
    Some(resumption) -> [resumption, ..resumptions]
    None -> resumptions
  }
}

fn update_next_index(
  indexes: Dict(String, Int),
  step_id: String,
  value: Int,
) -> Dict(String, Int) {
  let current = next_attempt_index_or_first(indexes, step_id)
  case value > current {
    True -> dict.insert(indexes, step_id, value)
    False -> indexes
  }
}

fn next_attempt_index_or_first(
  indexes: Dict(String, Int),
  step_id: String,
) -> Int {
  case dict.get(indexes, step_id) {
    Ok(index) -> index
    Error(Nil) -> 1
  }
}

fn compare_attempts_for_replay(
  a: projection.StepAttemptStatus,
  b: projection.StepAttemptStatus,
) -> Order {
  case int.compare(attempt_status_time(a), attempt_status_time(b)) {
    Eq -> compare_attempt_identities(attempt_identity(a), attempt_identity(b))
    order -> order
  }
}

fn compare_attempt_identities(
  a: #(String, String, Int),
  b: #(String, String, Int),
) -> Order {
  let #(a_run_id, a_step_id, a_attempt_index) = a
  let #(b_run_id, b_step_id, b_attempt_index) = b
  case string.compare(a_run_id, b_run_id) {
    Eq ->
      case string.compare(a_step_id, b_step_id) {
        Eq -> int.compare(a_attempt_index, b_attempt_index)
        order -> order
      }
    order -> order
  }
}

fn attempt_status_time(status: projection.StepAttemptStatus) -> Int {
  case status {
    projection.StepAttemptPending(prepared_at_ms: at_ms, ..) -> at_ms
    projection.StepAttemptRunning(started_at_ms: at_ms, ..) -> at_ms
    projection.StepAttemptFinishedStatus(finished_at_ms: at_ms, ..) -> at_ms
    projection.StepAttemptInterruptedStatus(interrupted_at_ms: at_ms, ..) ->
      at_ms
    projection.StepAttemptSupersededStatus(superseded_at_ms: at_ms, ..) -> at_ms
  }
}

fn attempt_identity(
  status: projection.StepAttemptStatus,
) -> #(String, String, Int) {
  case status {
    projection.StepAttemptPending(
      run_id,
      _,
      step_id,
      attempt_index,
      _,
      _,
      _,
      _,
      _,
      _,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptRunning(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptFinishedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptInterruptedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptSupersededStatus(
      run_id,
      _,
      step_id,
      attempt_index,
      _,
      _,
      _,
    ) -> #(run_id, step_id, attempt_index)
  }
}

fn attempt_workflow_id(status: projection.StepAttemptStatus) -> String {
  case status {
    projection.StepAttemptPending(workflow_id: workflow_id, ..) -> workflow_id
    projection.StepAttemptRunning(workflow_id: workflow_id, ..) -> workflow_id
    projection.StepAttemptFinishedStatus(workflow_id: workflow_id, ..) ->
      workflow_id
    projection.StepAttemptInterruptedStatus(workflow_id: workflow_id, ..) ->
      workflow_id
    projection.StepAttemptSupersededStatus(workflow_id: workflow_id, ..) ->
      workflow_id
  }
}

fn replayable_outbox(projection: projection.Projection) -> OutboxRecovery {
  let recovered =
    projection.outbox
    |> dict.to_list
    |> list.sort(by: compare_outbox_entries_by_time)
    |> list.fold(
      OutboxRecovery(outbox_to_replay: [], record_bodies: [], warnings: []),
      fn(recovery, entry) { recover_outbox_entry(recovery, projection, entry) },
    )

  OutboxRecovery(
    outbox_to_replay: list.reverse(recovered.outbox_to_replay),
    record_bodies: list.reverse(recovered.record_bodies),
    warnings: list.reverse(recovered.warnings),
  )
}

fn recover_outbox_entry(
  recovery: OutboxRecovery,
  projection: projection.Projection,
  entry: #(String, projection.OutboxStatus),
) -> OutboxRecovery {
  let #(outbox_id, status) = entry
  case status {
    projection.OutboxPending(issue_id, outbox_kind, _, _) ->
      fail_outbox_recovery(
        recovery,
        outbox_id,
        legacy_linear_task_ref(issue_id),
        outbox_kind,
        outbox.OutboxPayloadMissing,
      )
    projection.OutboxPendingV2(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      _,
    ) ->
      recover_pending_outbox(
        recovery,
        projection,
        outbox_id,
        legacy_linear_task_ref(issue_id),
        outbox_kind,
        dedupe_key,
        payload_json,
      )
    projection.OutboxPendingV2WithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      _,
    ) ->
      recover_pending_outbox(
        recovery,
        projection,
        outbox_id,
        task_ref,
        outbox_kind,
        dedupe_key,
        payload_json,
      )
    projection.OutboxCompleted(_, _, _)
    | projection.OutboxCompletedWithTask(_, _, _)
    | projection.OutboxFailed(_, _, _, _)
    | projection.OutboxFailedWithTask(_, _, _, _) -> recovery
  }
}

fn recover_pending_outbox(
  recovery: OutboxRecovery,
  projection: projection.Projection,
  outbox_id: String,
  task_ref: record.TaskRefFields,
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
) -> OutboxRecovery {
  case outbox.decode_payload(payload_json) {
    Error(error) ->
      fail_outbox_recovery(recovery, outbox_id, task_ref, outbox_kind, error)
    Ok(payload) ->
      case command_ack_already_recorded(projection, outbox_id, payload) {
        True -> recovery
        False ->
          case outbox.recovery_replay_error(outbox_kind, payload.kind) {
            Error(error) ->
              fail_outbox_recovery(
                recovery,
                outbox_id,
                task_ref,
                outbox_kind,
                error,
              )
            Ok(Nil) ->
              OutboxRecovery(..recovery, outbox_to_replay: [
                OutboxReplay(
                  outbox_id,
                  task_ref,
                  outbox_kind,
                  dedupe_key,
                  payload_json,
                ),
                ..recovery.outbox_to_replay
              ])
          }
      }
  }
}

fn command_ack_already_recorded(
  projection: projection.Projection,
  outbox_id: String,
  payload: outbox.Payload,
) -> Bool {
  case command_ack_receipt_key(outbox_id, payload) {
    Some(receipt_key) ->
      command_receipt_is_acked(projection.command_receipt(
        projection,
        receipt_key,
      ))
    None -> False
  }
}

fn command_ack_receipt_key(
  outbox_id: String,
  payload: outbox.Payload,
) -> Option(String) {
  case payload.kind {
    "linear_command_ack" ->
      case payload.source_comment_id {
        Some(source_comment_id) -> Some(source_comment_id)
        None -> Some(outbox_id)
      }
    "remote_command_ack" ->
      case payload.backend_kind, payload.task_remote_id, payload.event_id {
        Some(backend_kind), Some(task_remote_id), Some(event_id) ->
          Some(projection.remote_command_receipt_key(
            backend_kind,
            task_remote_id,
            event_id,
          ))
        _, _, _ -> None
      }
    _ -> None
  }
}

fn command_receipt_is_acked(receipt: projection.CommandReceiptState) -> Bool {
  case receipt {
    projection.CommandReceiptCompleted(acked_at_ms: Some(_), ..) -> True
    projection.CommandReceiptAcked(..) -> True
    _ -> False
  }
}

fn fail_outbox_recovery(
  recovery: OutboxRecovery,
  outbox_id: String,
  task_ref: record.TaskRefFields,
  outbox_kind: String,
  error: outbox.ReplayError,
) -> OutboxRecovery {
  let error_code = outbox.replay_error_code(error)
  let body =
    record.OutboxFailedWithTask(outbox_id, task_ref, outbox_kind, error_code)
  OutboxRecovery(
    ..recovery,
    record_bodies: [body, ..recovery.record_bodies],
    warnings: [outbox_recovery_warning(outbox_id, error), ..recovery.warnings],
  )
}

fn legacy_linear_task_ref(issue_id: String) -> record.TaskRefFields {
  record.linear_task_ref_fields(issue_id, None, None)
}

fn outbox_recovery_warning(
  outbox_id: String,
  error: outbox.ReplayError,
) -> String {
  "outbox_replay_failed:" <> outbox_id <> ":" <> outbox.replay_error_code(error)
}

fn compare_outbox_entries_by_time(
  a: #(String, projection.OutboxStatus),
  b: #(String, projection.OutboxStatus),
) -> Order {
  let #(a_id, a_status) = a
  let #(b_id, b_status) = b
  case int.compare(outbox_status_time(a_status), outbox_status_time(b_status)) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn outbox_status_time(status: projection.OutboxStatus) -> Int {
  case status {
    projection.OutboxPending(_, _, _, pending_at_ms) -> pending_at_ms
    projection.OutboxPendingV2(_, _, _, _, pending_at_ms) -> pending_at_ms
    projection.OutboxPendingV2WithTask(_, _, _, _, pending_at_ms) ->
      pending_at_ms
    projection.OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    projection.OutboxCompletedWithTask(_, _, completed_at_ms) -> completed_at_ms
    projection.OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
    projection.OutboxFailedWithTask(_, _, _, failed_at_ms) -> failed_at_ms
  }
}

fn restore_counters(
  runtime: orchestrator_state.RuntimeState,
  projection: projection.Projection,
) -> orchestrator_state.RuntimeState {
  let counters =
    projection.issue_counters
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(issue_id, counter) = entry
      #(
        orchestrator_state.linear_issue_id_identity(issue_id),
        orchestrator_state.IssueCounter(
          counter.failure_attempts,
          counter.worker_sessions,
        ),
      )
    })
    |> dict.from_list
  orchestrator_state.RuntimeState(..runtime, issue_counters: counters)
}

fn restore_parked(
  build: Build,
  projection: projection.Projection,
  issue_by_id: Dict(String, tracker_issue.Issue),
) -> Build {
  projection.parked_issues
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(issue_id, parked) = entry
    case
      workflow_attempt.parked_issue_should_survive(
        parked,
        issue_id,
        issue_by_id,
      )
    {
      True -> {
        let task_ref = orchestrator_state.linear_issue_id_ref(issue_id)
        let identity = orchestrator_state.task_ref_identity(task_ref)
        let parked_entry =
          orchestrator_state.ParkedEntry(
            task_ref: task_ref,
            issue_id: issue_id,
            identifier: parked.issue_identifier,
            reason: park_reason_from_string(parked.reason),
            release_policy: orchestrator_state.park_release_policy_from_string(
              parked.release_policy,
              parked.issue_fingerprint,
            ),
            parked_at_ms: parked.parked_at_ms,
          )
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.insert(build.runtime.parked, identity, parked_entry),
            claimed: dict.delete(build.runtime.claimed, identity),
          ),
        )
      }
      False -> {
        let identity = orchestrator_state.linear_issue_id_identity(issue_id)
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.delete(build.runtime.parked, identity),
            retry_attempts: dict.delete(build.runtime.retry_attempts, identity),
            issue_counters: dict.delete(build.runtime.issue_counters, identity),
            claimed: dict.delete(build.runtime.claimed, identity),
          ),
          record_bodies: [
            record.IssueUnparked(
              issue_id,
              parked.issue_identifier,
              "issue_changed",
            ),
            record.IssueCounterUpdated(
              issue_id,
              parked.issue_identifier,
              0,
              0,
              parked.observed_updated_at_ms,
              None,
            ),
            ..build.record_bodies
          ],
          auto_unparked_issue_ids: [issue_id, ..build.auto_unparked_issue_ids],
        )
      }
    }
  })
}

fn restore_retries(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  now_ms: Int,
) -> Build {
  let backend_kinds = recovered_backend_kinds(projection)
  let active_workflow_issue_ids =
    projection.active_workflow_runs(projection)
    |> list.map(fn(entry) {
      let #(_, status) = entry
      case status {
        projection.WorkflowRunActive(issue_id: issue_id, ..) -> issue_id
        _ -> ""
      }
    })
  projection.retries
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(issue_id, status) = entry
    case status {
      projection.RetryScheduled(issue_identifier, _, generation, reason_text, _) ->
        restore_scheduled_retry(
          build,
          config,
          backend_kinds,
          active_workflow_issue_ids,
          issue_by_id,
          issue_id,
          issue_identifier,
          generation,
          reason_text,
          status,
          now_ms,
        )
      projection.RetryCancelled(_, _, _) -> build
    }
  })
}

fn restore_scheduled_retry(
  build: Build,
  config: config_types.EffectiveConfig,
  backend_kinds: Dict(String, String),
  active_workflow_issue_ids: List(String),
  issue_by_id: Dict(String, tracker_issue.Issue),
  issue_id: String,
  issue_identifier: String,
  generation: Int,
  reason_text: String,
  status: projection.RetryStatus,
  now_ms: Int,
) -> Build {
  let backend_kind = recovered_backend_kind_for_issue(backend_kinds, issue_id)
  let recovered_identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind)
  case list.contains(build.auto_unparked_issue_ids, issue_id) {
    True ->
      cancel_recovered_retry(
        build,
        issue_id,
        recovered_identity,
        generation,
        "recovery_auto_unparked",
      )
    False ->
      case list.contains(active_workflow_issue_ids, issue_id) {
        True ->
          cancel_recovered_retry(
            build,
            issue_id,
            recovered_identity,
            generation,
            "recovery_active_workflow_run",
          )
        False ->
          case dict.has_key(build.runtime.parked, recovered_identity) {
            True ->
              cancel_recovered_retry(
                build,
                issue_id,
                recovered_identity,
                generation,
                "recovery_parked",
              )
            False ->
              case dict.get(issue_by_id, issue_id) {
                Error(Nil) ->
                  cancel_recovered_retry(
                    build,
                    issue_id,
                    recovered_identity,
                    generation,
                    "recovery_missing_issue",
                  )
                Ok(issue) ->
                  case recovery_policy.is_terminal(config, issue.state) {
                    True ->
                      cancel_recovered_retry(
                        build,
                        issue_id,
                        recovered_identity,
                        generation,
                        "recovery_terminal_issue",
                      )
                    False ->
                      case
                        config_types.retry_state_allowed(config, issue.state)
                      {
                        False ->
                          cancel_recovered_retry(
                            build,
                            issue_id,
                            recovered_identity,
                            generation,
                            config_types.recovery_non_retryable_reason(
                              issue.state,
                            ),
                          )
                        True -> {
                          let remaining =
                            workflow_attempt.remaining_retry_delay(
                              status,
                              now_ms,
                            )
                          let task_ref =
                            orchestrator_state.issue_ref_for_backend(
                              issue,
                              backend_kind,
                            )
                          let identity =
                            orchestrator_state.task_ref_identity(task_ref)
                          let retry =
                            orchestrator_state.RetryEntry(
                              task_ref: task_ref,
                              issue_id: issue_id,
                              delay_ms: remaining,
                              timer_generation: generation,
                            )
                          Build(
                            ..build,
                            runtime: orchestrator_state.RuntimeState(
                              ..build.runtime,
                              retry_attempts: dict.insert(
                                build.runtime.retry_attempts,
                                identity,
                                retry,
                              ),
                              claimed: dict.insert(
                                build.runtime.claimed,
                                identity,
                                issue_identifier,
                              ),
                            ),
                            retry_timers: [
                              RecoveredRetry(
                                issue_id,
                                issue_identifier,
                                remaining,
                                generation,
                                reason_text,
                              ),
                              ..build.retry_timers
                            ],
                          )
                        }
                      }
                  }
              }
          }
      }
  }
}

fn cancel_recovered_retry(
  build: Build,
  issue_id: String,
  identity: identity.TaskIdentity,
  generation: Int,
  reason: String,
) -> Build {
  Build(
    ..build,
    runtime: orchestrator_state.RuntimeState(
      ..build.runtime,
      retry_attempts: dict.delete(build.runtime.retry_attempts, identity),
      claimed: dict.delete(build.runtime.claimed, identity),
    ),
    record_bodies: [
      record.RetryCancelled(issue_id, generation, reason),
      ..build.record_bodies
    ],
  )
}

fn recovered_backend_kinds(
  projection: projection.Projection,
) -> Dict(String, String) {
  projection.known_task_refs(projection)
  |> list.fold(dict.new(), fn(kinds, ref) {
    case ref.task_backend_kind == "linear" {
      True -> kinds
      False -> dict.insert(kinds, ref.task_remote_id, ref.task_backend_kind)
    }
  })
}

fn recovered_backend_kind_for_issue(
  backend_kinds: Dict(String, String),
  issue_id: String,
) -> String {
  case dict.get(backend_kinds, issue_id) {
    Ok(kind) -> kind
    Error(Nil) -> "linear"
  }
}

fn recover_interrupted_runs(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  now_ms: Int,
) -> Build {
  projection.runs
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(run_id, status) = entry
    case projection.has_workflow_run(projection, run_id) {
      True -> build
      False ->
        case status {
          projection.RunRunning(issue_id, issue_identifier, workspace_path, _) ->
            recover_one_interrupted_run(
              build,
              projection,
              config,
              issue_by_id,
              run_id,
              issue_id,
              issue_identifier,
              workspace_path,
              True,
              now_ms,
            )
          projection.RunInterrupted(issue_id, _, _) ->
            recover_one_interrupted_run(
              build,
              projection,
              config,
              issue_by_id,
              run_id,
              issue_id,
              identifier_for_issue(projection, issue_by_id, issue_id),
              workspace_for_issue(projection, issue_id),
              False,
              now_ms,
            )
          projection.RunFinished(_, _, _, _, _) -> build
        }
    }
  })
}

fn recover_one_interrupted_run(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
  append_interrupted: Bool,
  now_ms: Int,
) -> Build {
  let build = case append_interrupted {
    True ->
      Build(..build, record_bodies: [
        record.RunInterrupted(run_id, issue_id, "daemon_restart"),
        ..build.record_bodies
      ])
    False -> build
  }
  case dict.get(issue_by_id, issue_id) {
    Error(Nil) -> warn(build, "missing_issue_for_interrupted_run:" <> issue_id)
    Ok(issue) ->
      case recovery_policy.is_terminal(config, issue.state) {
        True ->
          recover_terminal_interrupted(
            build,
            projection,
            issue,
            issue_id,
            issue_identifier,
            workspace_path,
          )
        False ->
          case recovery_policy.is_active(config, issue.state) {
            True ->
              recover_active_interrupted(
                build,
                projection,
                config,
                issue,
                run_id,
                issue_identifier,
                now_ms,
              )
            False -> warn(build, "non_active_interrupted_run:" <> issue_id)
          }
      }
  }
}

fn recover_terminal_interrupted(
  build: Build,
  projection: projection.Projection,
  issue: tracker_issue.Issue,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
) -> Build {
  let workspace_path = case string.trim(workspace_path) == "" {
    True -> workspace_for_issue(projection, issue_id)
    False -> workspace_path
  }
  let cleanup_workspaces = case string.trim(workspace_path) == "" {
    True -> build.cleanup_workspaces
    False -> [
      CleanupRequest(issue_id, issue_identifier, workspace_path),
      ..build.cleanup_workspaces
    ]
  }
  let identity = orchestrator_state.issue_identity(issue)
  Build(
    ..build,
    runtime: orchestrator_state.RuntimeState(
      ..build.runtime,
      completed: dict.insert(build.runtime.completed, identity, issue),
      claimed: dict.delete(build.runtime.claimed, identity),
      retry_attempts: dict.delete(build.runtime.retry_attempts, identity),
    ),
    cleanup_workspaces: cleanup_workspaces,
  )
}

fn recover_active_interrupted(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  run_id: String,
  issue_identifier: String,
  now_ms: Int,
) -> Build {
  let issue_id = issue.id
  case projection.counter_has_source_run(projection, issue_id, run_id) {
    True ->
      ensure_retry_or_park_for_counter(
        build,
        config,
        issue,
        issue_identifier,
        now_ms,
      )
    False -> {
      let counter = counter_for_runtime(build.runtime, issue_id)
      let failures = counter.failure_attempts + 1
      let counter =
        orchestrator_state.IssueCounter(..counter, failure_attempts: failures)
      let build =
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            issue_counters: dict.insert(
              build.runtime.issue_counters,
              orchestrator_state.issue_identity(issue),
              counter,
            ),
          ),
          record_bodies: [
            record.IssueCounterUpdated(
              issue_id,
              issue_identifier,
              counter.failure_attempts,
              counter.worker_sessions,
              now_ms,
              Some(run_id),
            ),
            ..build.record_bodies
          ],
        )
      ensure_retry_or_park_for_counter(
        build,
        config,
        issue,
        issue_identifier,
        now_ms,
      )
    }
  }
}

fn ensure_retry_or_park_for_counter(
  build: Build,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  issue_identifier: String,
  now_ms: Int,
) -> Build {
  let issue_id = issue.id
  let counter = counter_for_runtime(build.runtime, issue_id)
  case counter.failure_attempts >= config.agent.max_retry_attempts {
    True ->
      case
        dict.has_key(
          build.runtime.parked,
          orchestrator_state.issue_identity(issue),
        )
      {
        True -> build
        False -> {
          let fingerprint = recovery_policy.issue_fingerprint(issue)
          let task_ref = orchestrator_state.issue_ref(issue)
          let identity = orchestrator_state.task_ref_identity(task_ref)
          let parked =
            orchestrator_state.ParkedEntry(
              task_ref: task_ref,
              issue_id: issue_id,
              identifier: issue_identifier,
              reason: reason.ParkMaxRetryAttempts,
              release_policy: orchestrator_state.AutoUnparkOnIssueChange(
                fingerprint,
              ),
              parked_at_ms: now_ms,
            )
          Build(
            ..build,
            runtime: orchestrator_state.RuntimeState(
              ..build.runtime,
              parked: dict.insert(build.runtime.parked, identity, parked),
              retry_attempts: dict.delete(
                build.runtime.retry_attempts,
                identity,
              ),
              claimed: dict.delete(build.runtime.claimed, identity),
            ),
            record_bodies: [
              record.IssueParkedV2(
                issue_id,
                issue_identifier,
                reason.park_to_string(reason.ParkMaxRetryAttempts),
                "auto_unpark_on_issue_change",
                fingerprint,
                now_ms,
              ),
              ..build.record_bodies
            ],
          )
        }
      }
    False ->
      case
        dict.has_key(
          build.runtime.retry_attempts,
          orchestrator_state.issue_identity(issue),
        )
      {
        True -> build
        False -> {
          let delay_ms =
            recovery_policy.backoff_delay(
              counter.failure_attempts,
              config.agent.max_retry_backoff_ms,
            )
          let generation = 1
          let task_ref = orchestrator_state.issue_ref(issue)
          let identity = orchestrator_state.task_ref_identity(task_ref)
          let retry =
            orchestrator_state.RetryEntry(
              task_ref: task_ref,
              issue_id: issue_id,
              delay_ms: delay_ms,
              timer_generation: generation,
            )
          Build(
            ..build,
            runtime: orchestrator_state.RuntimeState(
              ..build.runtime,
              retry_attempts: dict.insert(
                build.runtime.retry_attempts,
                identity,
                retry,
              ),
              claimed: dict.insert(
                build.runtime.claimed,
                identity,
                issue_identifier,
              ),
            ),
            retry_timers: [
              RecoveredRetry(
                issue_id,
                issue_identifier,
                delay_ms,
                generation,
                reason.retry_to_string(reason.RetryAfterFailure),
              ),
              ..build.retry_timers
            ],
            record_bodies: [
              record.RetryScheduled(
                issue_id,
                issue_identifier,
                delay_ms,
                generation,
                reason.retry_to_string(reason.RetryAfterFailure),
              ),
              ..build.record_bodies
            ],
          )
        }
      }
  }
}

fn ledger_records(
  now_ms: Int,
  bodies: List(record.RecordBody),
) -> List(record.LedgerRecord) {
  ledger_records_loop(bodies, now_ms, 1, [])
}

fn ledger_records_loop(
  bodies: List(record.RecordBody),
  now_ms: Int,
  sequence: Int,
  acc: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> list.reverse(acc)
    [body, ..rest] ->
      ledger_records_loop(rest, now_ms, sequence + 1, [
        record.new(now_ms, sequence, body),
        ..acc
      ])
  }
}

fn counter_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  case
    dict.get(
      runtime.issue_counters,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(counter) -> counter
    Error(Nil) -> orchestrator_state.new_issue_counter()
  }
}

fn park_reason_from_string(text: String) -> reason.ParkReason {
  case text {
    "max_retry_attempts" -> reason.ParkMaxRetryAttempts
    "max_sessions_per_issue" -> reason.ParkMaxSessionsPerIssue
    other -> reason.ParkOperator(other)
  }
}

fn identifier_for_issue(
  projection: projection.Projection,
  issue_by_id: Dict(String, tracker_issue.Issue),
  issue_id: String,
) -> String {
  case dict.get(issue_by_id, issue_id) {
    Ok(issue) -> issue.identifier
    Error(Nil) ->
      case dict.get(projection.known_workspaces, issue_id) {
        Ok(workspace) -> workspace.issue_identifier
        Error(Nil) ->
          case dict.get(projection.issue_counters, issue_id) {
            Ok(counter) -> counter.issue_identifier
            Error(Nil) -> issue_id
          }
      }
  }
}

fn workspace_for_issue(
  projection: projection.Projection,
  issue_id: String,
) -> String {
  case projection.known_workspace_for_issue(projection, issue_id) {
    Ok(workspace_path) -> workspace_path
    Error(Nil) -> ""
  }
}

fn warn(build: Build, warning: String) -> Build {
  Build(..build, warnings: [warning, ..build.warnings])
}
