import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt
import scherzo/workflow_dag
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
    issue_id: String,
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
}

type SessionRecoveryConfig {
  SessionRecoveryConfig(enabled: Bool, recovery_prompt: String)
}

pub type WorkflowFinalization {
  WorkflowFinalization(
    records_to_append: List(record.LedgerRecord),
    resumptions: List(RecoveredWorkflowRun),
    warnings: List(String),
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
  StepArtifactRecoveryFailed(reason: String)
  UnsafeWorkflowRecovery(reason: String)
  WorkspaceRecoveryFailed(reason: String)
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

fn default_session_recovery_config() -> SessionRecoveryConfig {
  SessionRecoveryConfig(enabled: False, recovery_prompt: "")
}

pub fn workflow_candidates(
  projection: projection.Projection,
) -> List(WorkflowRecoveryCandidate) {
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
    SessionRecoveryConfig(
      enabled: config.pi.session_persistence.enabled,
      recovery_prompt: config.pi.session_persistence.recovery_prompt,
    ),
    ResumeRecoveredWorkflows,
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
    default_session_recovery_config(),
    mode,
  )
}

fn finalize_workflow_candidates_with_config_and_mode(
  projection: projection.Projection,
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  artifact_store: artifact_store.Store,
  now_ms: Int,
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
) -> Result(WorkflowFinalization, RecoveryError) {
  let _ = projection
  finalize_workflow_candidates_loop(
    candidates,
    observations,
    artifact_store,
    now_ms,
    session_recovery,
    mode,
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
  let issue_by_id = issues_by_id(refreshed_issues)
  let base = core.new_state(config)
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
    StepArtifactRecoveryFailed(reason) -> reason
    UnsafeWorkflowRecovery(reason) -> "unsafe_workflow_recovery:" <> reason
    WorkspaceRecoveryFailed(reason) -> "workspace_recovery_failed:" <> reason
  }
}

fn workflow_task_ref_or_legacy(
  projection: projection.Projection,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
) -> record.TaskRefFields {
  projection.workflow_task_ref(projection, run_id)
  |> result.unwrap(record.legacy_linear_task_ref_fields(
    issue_id,
    issue_identifier,
  ))
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

fn finalize_workflow_candidates_loop(
  candidates: List(WorkflowRecoveryCandidate),
  observations: Dict(String, CurrentWorkflowObservation),
  store: artifact_store.Store,
  now_ms: Int,
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
  record_bodies: List(record.RecordBody),
  resumptions: List(RecoveredWorkflowRun),
  warnings: List(String),
) -> Result(WorkflowFinalization, RecoveryError) {
  case candidates {
    [] ->
      Ok(WorkflowFinalization(
        records_to_append: ledger_records(now_ms, list.reverse(record_bodies)),
        resumptions: list.reverse(resumptions),
        warnings: list.reverse(warnings),
      ))
    [candidate, ..rest] -> {
      let observation =
        dict.get(observations, candidate.run_id)
        |> result.unwrap(IssueUnavailable)
      use finalized <- result.try(finalize_one_workflow_candidate(
        candidate,
        observation,
        store,
        session_recovery,
        mode,
      ))
      let #(bodies, resumption, candidate_warnings) = finalized
      finalize_workflow_candidates_loop(
        rest,
        observations,
        store,
        now_ms,
        session_recovery,
        mode,
        list.append(list.reverse(bodies), record_bodies),
        append_optional_resumption(resumptions, resumption),
        list.append(list.reverse(candidate_warnings), warnings),
      )
    }
  }
}

fn finalize_one_workflow_candidate(
  candidate: WorkflowRecoveryCandidate,
  observation: CurrentWorkflowObservation,
  store: artifact_store.Store,
  session_recovery: SessionRecoveryConfig,
  mode: WorkflowRecoveryMode,
) -> Result(
  #(List(record.RecordBody), Option(RecoveredWorkflowRun), List(String)),
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
        workflow_id == candidate.workflow_id
        && workflow_fingerprint == candidate.workflow_fingerprint
        && issue_fingerprint == candidate.issue_fingerprint
      {
        False ->
          Ok(
            #(
              park_candidate_bodies(
                candidate,
                issue.identifier,
                "workflow_drift",
                issue_fingerprint,
                candidate.observed_updated_at_ms,
              ),
              None,
              [
                "workflow_recovery_parked_workflow_drift:" <> candidate.run_id,
              ],
            ),
          )
        True ->
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
                ),
              )
            ResumeRecoveredWorkflows ->
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

fn finalize_resumable_workflow_candidate(
  candidate: WorkflowRecoveryCandidate,
  issue: tracker_issue.Issue,
  issue_fingerprint: String,
  store: artifact_store.Store,
  dag: workflow_dag.WorkflowDag,
  workspace_root: String,
  session_recovery: SessionRecoveryConfig,
) -> Result(
  #(List(record.RecordBody), Option(RecoveredWorkflowRun), List(String)),
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
      Ok(#(bodies, Some(resumption), []))
    }
    Error(StepArtifactRecoveryFailed(reason)) ->
      Ok(
        #(
          park_candidate_bodies(
            candidate,
            issue.identifier,
            "artifact_recovery_failed",
            issue_fingerprint,
            candidate.observed_updated_at_ms,
          ),
          None,
          [
            "workflow_recovery_parked_artifact_recovery_failed:"
            <> candidate.run_id
            <> ":"
            <> reason,
          ],
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
          use artifact <- result.try(
            artifact_store.read_step_artifact(
              store,
              artifact_ref,
              artifact_sha256,
            )
            |> result.map_error(fn(error) {
              StepArtifactRecoveryFailed(describe_artifact_error(error))
            }),
          )
          let artifacts = dict.insert(artifacts, step_id, artifact)
          let run_root = case string.trim(run_root) == "" {
            True -> candidate.run_root
            False -> run_root
          }
          let workspaces = case dependency_satisfying_outcome(outcome) {
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
            Error(reason) -> Error(UnsafeWorkflowRecovery(reason))
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
        Error(reason) -> Error(UnsafeWorkflowRecovery(reason))
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
        Error(reason) -> Error(UnsafeWorkflowRecovery(reason))
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
) -> Result(#(String, Int, String, String, String, String, Int), String) {
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
    _ -> Error("recovery_session_fact_missing")
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
) -> Result(#(String, Int, String, String, String, String, Int), String) {
  case fact_count {
    0 -> Error("recovery_session_fact_missing")
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
        _, _ -> Error("recovery_session_fact_missing")
      }
    _ -> Error("recovery_session_fact_ambiguous")
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
  let root_abs = path.absolute(workspace_root) |> result.unwrap(workspace_root)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  let workspace_abs =
    path.absolute(workspace_path) |> result.unwrap(workspace_path)
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
) -> Result(Nil, String) {
  case workflow_dag.step_by_id(dag, step_id) {
    Error(_) -> Error("unknown_interrupted_step:" <> step_id)
    Ok(step) ->
      case step.kind {
        workflow_dag.CommandStep(_, _) ->
          Error("unsafe_interrupted_command_step:" <> step_id)
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
  let root_abs = path.absolute(workspace_root) |> result.unwrap(workspace_root)
  let run_root_abs =
    path.absolute(candidate.run_root) |> result.unwrap(candidate.run_root)
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
  let workspace_run_root_abs =
    path.absolute(workspace.run_root) |> result.unwrap(workspace.run_root)
  let workspace_path_abs =
    path.absolute(workspace.path) |> result.unwrap(workspace.path)
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
            path.absolute(workspace.path) |> result.unwrap(workspace.path),
            "missing_source_workspace:" <> source <> ":for_step:" <> step.id,
          )
        Error(_) ->
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
            Error(_) ->
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

fn dependency_satisfying_outcome(outcome: String) -> Bool {
  outcome == "completed" || outcome == "failed_continued"
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
  let current = dict.get(indexes, step_id) |> result.unwrap(1)
  case value > current {
    True -> dict.insert(indexes, step_id, value)
    False -> indexes
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

fn describe_artifact_error(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(message) -> message
    artifact_store.ArtifactWriteFailed(error) ->
      artifact_store.artifact_write_error_to_string(error)
    artifact_store.MissingStepArtifact(ref) -> "missing_step_artifact:" <> ref
    artifact_store.CorruptStepArtifact(ref) -> "corrupt_step_artifact:" <> ref
    artifact_store.InvalidArtifactRef(ref) -> "invalid_artifact_ref:" <> ref
    artifact_store.DecodeArtifactFailed(reason) ->
      "decode_artifact_failed:" <> reason
    artifact_store.DirectorySyncUnsupported(reason) ->
      "directory_sync_unsupported:" <> reason
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
        issue_id,
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
      case outbox.decode_payload(payload_json) {
        Error(error) ->
          fail_outbox_recovery(
            recovery,
            outbox_id,
            issue_id,
            outbox_kind,
            error,
          )
        Ok(payload) ->
          case outbox.recovery_replay_error(outbox_kind, payload.kind) {
            Error(error) ->
              fail_outbox_recovery(
                recovery,
                outbox_id,
                issue_id,
                outbox_kind,
                error,
              )
            Ok(Nil) ->
              case
                command_ack_already_recorded(projection, outbox_id, payload)
              {
                True -> recovery
                False ->
                  OutboxRecovery(..recovery, outbox_to_replay: [
                    OutboxReplay(
                      outbox_id,
                      issue_id,
                      outbox_kind,
                      dedupe_key,
                      payload_json,
                    ),
                    ..recovery.outbox_to_replay
                  ])
              }
          }
      }
    projection.OutboxCompleted(_, _, _) | projection.OutboxFailed(_, _, _, _) ->
      recovery
  }
}

fn command_ack_already_recorded(
  projection: projection.Projection,
  outbox_id: String,
  payload: outbox.Payload,
) -> Bool {
  case command_ack_event_id(outbox_id, payload) {
    Some(event_id) ->
      command_receipt_is_acked(projection.command_receipt(projection, event_id))
    None -> False
  }
}

fn command_ack_event_id(
  outbox_id: String,
  payload: outbox.Payload,
) -> Option(String) {
  case payload.kind {
    "linear_command_ack" ->
      case payload.source_comment_id {
        Some(source_comment_id) -> Some(source_comment_id)
        None -> Some(outbox_id)
      }
    "remote_command_ack" -> payload.event_id
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
  issue_id: String,
  outbox_kind: String,
  error: outbox.ReplayError,
) -> OutboxRecovery {
  let error_code = outbox.replay_error_code(error)
  let body = record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code)
  OutboxRecovery(
    ..recovery,
    record_bodies: [body, ..recovery.record_bodies],
    warnings: [outbox_recovery_warning(outbox_id, error), ..recovery.warnings],
  )
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
    projection.OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    projection.OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
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
        issue_id,
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
    case parked_should_survive(parked, issue_id, issue_by_id) {
      True -> {
        let parked_entry =
          orchestrator_state.ParkedEntry(
            issue_id: issue_id,
            identifier: parked.issue_identifier,
            reason: park_reason_from_string(parked.reason),
            release_policy: release_policy_from_projection(parked),
            parked_at_ms: parked.parked_at_ms,
          )
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.insert(build.runtime.parked, issue_id, parked_entry),
            claimed: dict.delete(build.runtime.claimed, issue_id),
          ),
        )
      }
      False ->
        Build(
          ..build,
          runtime: orchestrator_state.RuntimeState(
            ..build.runtime,
            parked: dict.delete(build.runtime.parked, issue_id),
            retry_attempts: dict.delete(build.runtime.retry_attempts, issue_id),
            issue_counters: dict.delete(build.runtime.issue_counters, issue_id),
            claimed: dict.delete(build.runtime.claimed, issue_id),
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
  })
}

fn restore_retries(
  build: Build,
  projection: projection.Projection,
  config: config_types.EffectiveConfig,
  issue_by_id: Dict(String, tracker_issue.Issue),
  now_ms: Int,
) -> Build {
  projection.retries
  |> dict.to_list
  |> list.fold(build, fn(build, entry) {
    let #(issue_id, status) = entry
    case status {
      projection.RetryScheduled(issue_identifier, _, generation, reason_text, _) ->
        restore_scheduled_retry(
          build,
          config,
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
  issue_by_id: Dict(String, tracker_issue.Issue),
  issue_id: String,
  issue_identifier: String,
  generation: Int,
  reason_text: String,
  status: projection.RetryStatus,
  now_ms: Int,
) -> Build {
  case list.contains(build.auto_unparked_issue_ids, issue_id) {
    True ->
      cancel_recovered_retry(
        build,
        issue_id,
        generation,
        "recovery_auto_unparked",
      )
    False ->
      case dict.has_key(build.runtime.parked, issue_id) {
        True ->
          cancel_recovered_retry(build, issue_id, generation, "recovery_parked")
        False ->
          case dict.get(issue_by_id, issue_id) {
            Error(Nil) ->
              cancel_recovered_retry(
                build,
                issue_id,
                generation,
                "recovery_missing_issue",
              )
            Ok(issue) ->
              case core.is_terminal(config, issue.state) {
                True ->
                  cancel_recovered_retry(
                    build,
                    issue_id,
                    generation,
                    "recovery_terminal_issue",
                  )
                False ->
                  case core.is_active(config, issue.state) {
                    False ->
                      cancel_recovered_retry(
                        build,
                        issue_id,
                        generation,
                        "recovery_non_active_issue",
                      )
                    True -> {
                      let remaining = remaining_retry_delay(status, now_ms)
                      let retry =
                        orchestrator_state.RetryEntry(
                          issue_id,
                          remaining,
                          generation,
                        )
                      Build(
                        ..build,
                        runtime: orchestrator_state.RuntimeState(
                          ..build.runtime,
                          retry_attempts: dict.insert(
                            build.runtime.retry_attempts,
                            issue_id,
                            retry,
                          ),
                          claimed: dict.insert(
                            build.runtime.claimed,
                            issue_id,
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

fn cancel_recovered_retry(
  build: Build,
  issue_id: String,
  generation: Int,
  reason: String,
) -> Build {
  Build(
    ..build,
    runtime: orchestrator_state.RuntimeState(
      ..build.runtime,
      retry_attempts: dict.delete(build.runtime.retry_attempts, issue_id),
      claimed: dict.delete(build.runtime.claimed, issue_id),
    ),
    record_bodies: [
      record.RetryCancelled(issue_id, generation, reason),
      ..build.record_bodies
    ],
  )
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
    Error(_) -> warn(build, "missing_issue_for_interrupted_run:" <> issue_id)
    Ok(issue) ->
      case core.is_terminal(config, issue.state) {
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
          case core.is_active(config, issue.state) {
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
  Build(
    ..build,
    runtime: orchestrator_state.RuntimeState(
      ..build.runtime,
      completed: dict.insert(build.runtime.completed, issue_id, issue),
      claimed: dict.delete(build.runtime.claimed, issue_id),
      retry_attempts: dict.delete(build.runtime.retry_attempts, issue_id),
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
              issue_id,
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
      case dict.has_key(build.runtime.parked, issue_id) {
        True -> build
        False -> {
          let fingerprint = core.issue_fingerprint(issue)
          let parked =
            orchestrator_state.ParkedEntry(
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
              parked: dict.insert(build.runtime.parked, issue_id, parked),
              retry_attempts: dict.delete(
                build.runtime.retry_attempts,
                issue_id,
              ),
              claimed: dict.delete(build.runtime.claimed, issue_id),
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
      case dict.has_key(build.runtime.retry_attempts, issue_id) {
        True -> build
        False -> {
          let delay_ms =
            core.backoff_delay(
              counter.failure_attempts,
              config.agent.max_retry_backoff_ms,
            )
          let generation = 1
          let retry =
            orchestrator_state.RetryEntry(issue_id, delay_ms, generation)
          Build(
            ..build,
            runtime: orchestrator_state.RuntimeState(
              ..build.runtime,
              retry_attempts: dict.insert(
                build.runtime.retry_attempts,
                issue_id,
                retry,
              ),
              claimed: dict.insert(
                build.runtime.claimed,
                issue_id,
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

fn parked_should_survive(
  parked: projection.ParkedIssue,
  issue_id: String,
  issue_by_id: Dict(String, tracker_issue.Issue),
) -> Bool {
  case parked.release_policy {
    "auto_unpark_on_issue_change" ->
      case dict.get(issue_by_id, issue_id) {
        Ok(issue) -> core.issue_fingerprint(issue) == parked.issue_fingerprint
        Error(_) -> True
      }
    _ -> True
  }
}

fn release_policy_from_projection(
  parked: projection.ParkedIssue,
) -> orchestrator_state.ParkReleasePolicy {
  case parked.release_policy {
    "auto_unpark_on_issue_change" ->
      orchestrator_state.AutoUnparkOnIssueChange(parked.issue_fingerprint)
    _ -> orchestrator_state.ExplicitUnparkOnly
  }
}

fn remaining_retry_delay(status: projection.RetryStatus, now_ms: Int) -> Int {
  case projection.retry_due_at_ms(status) {
    Ok(due_at_ms) -> max_int(0, due_at_ms - now_ms)
    Error(_) -> 0
  }
}

fn max_int(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn issues_by_id(
  issues: List(tracker_issue.Issue),
) -> Dict(String, tracker_issue.Issue) {
  issues
  |> list.map(fn(issue) { #(issue.id, issue) })
  |> dict.from_list
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
  dict.get(runtime.issue_counters, issue_id)
  |> result.unwrap(orchestrator_state.new_issue_counter())
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
    Error(_) ->
      case dict.get(projection.known_workspaces, issue_id) {
        Ok(workspace) -> workspace.issue_identifier
        Error(_) ->
          case dict.get(projection.issue_counters, issue_id) {
            Ok(counter) -> counter.issue_identifier
            Error(_) -> issue_id
          }
      }
  }
}

fn workspace_for_issue(
  projection: projection.Projection,
  issue_id: String,
) -> String {
  projection.known_workspace_for_issue(projection, issue_id)
  |> result.unwrap("")
}

fn warn(build: Build, warning: String) -> Build {
  Build(..build, warnings: [warning, ..build.warnings])
}
