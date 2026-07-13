import gleam/bit_array
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/state/projection
import scherzo/state/record

/// The result of inspecting the workspace-retention marker for a run.
/// Inspection uncertainty is represented explicitly so retention can fail closed.
pub type MarkerState {
  MarkerAbsent
  MarkerPresent
  MarkerUnreadable
}

pub type FamilyCounts {
  FamilyCounts(
    workflow_runs: Int,
    provenances: Int,
    task_refs: Int,
    input_manifests: Int,
    interface_snapshots: Int,
    output_manifests: Int,
    repairs: Int,
    step_attempts: Int,
    step_recoveries: Int,
    publication_attempts: Int,
    control_operations: Int,
    outbox_entries: Int,
  )
}

pub type BlockerCounts {
  BlockerCounts(
    active: Int,
    within_grace: Int,
    parked: Int,
    retained_workspace: Int,
    marker_unavailable: Int,
    recovery_started: Int,
    control_in_flight: Int,
    publication_unsettled: Int,
    outbox_unsettled: Int,
    malformed_association: Int,
  )
}

pub type PruneReport {
  PruneReport(
    candidate_run_ids: List(String),
    pruned_run_ids: List(String),
    families_removed: FamilyCounts,
    blockers: BlockerCounts,
    before_bytes: Int,
    after_bytes: Int,
  )
}

pub type PruneResult {
  PruneResult(projection: projection.Projection, report: PruneReport)
}

type TerminalRun {
  TerminalRun(
    run_id: String,
    issue_id: String,
    run_root: String,
    terminal_at_ms: Int,
    scheduled_job_id: Option(String),
  )
}

type Selection {
  Selection(candidates: List(TerminalRun), blockers: BlockerCounts)
}

pub type AppendRecordWorkflowRequirement {
  AddWorkflowRun(run_id: String)
  RequireKnownWorkflowRun(reason: String, run_id: String)
  RejectPrunedWorkflowRunOnly(run_id: String)
  NoWorkflowRunRequirement
}

pub fn append_record_workflow_requirement(
  body: record.RecordBody,
) -> AppendRecordWorkflowRequirement {
  case body {
    record.WorkflowRunStarted(run_id, _, _, _, _, _, _, _)
    | record.WorkflowRunStartedWithTask(run_id, _, _, _, _, _, _, _, _) ->
      AddWorkflowRun(run_id)
    record.WorkflowRunFinished(run_id, _, _, _, _, _)
    | record.WorkflowRunFinishedWithTask(run_id, _, _, _, _, _, _)
    | record.WorkflowRunInterrupted(run_id, _, _, _)
    | record.WorkflowRunSuperseded(run_id, _, _, _, _) ->
      RequireKnownWorkflowRun("unknown_workflow_run", run_id)
    record.WorkflowRunProvenanceRepaired(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.WorkflowRunInputsRecorded(run_id, _, _, _, _, _)
    | record.WorkflowInterfaceSnapshotRecorded(run_id, _, _, _, _, _)
    | record.WorkflowRunOutputsRecorded(run_id, _, _, _, _, _)
    | record.PublicationAttemptRecorded(
        run_id,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
      )
    | record.WorkflowRepairRequested(run_id, _, _, _, _, _, _, _, _, _) ->
      RejectPrunedWorkflowRunOnly(run_id)
    record.StepAttemptPrepared(run_id, _, _, _, _, _, _, _, _)
    | record.StepAttemptStarted(run_id, _, _, _, _, _, _)
    | record.StepAttemptContinuationStarted(run_id, _, _, _, _)
    | record.StepAttemptPiSessionRecorded(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptPiSessionRecordedWithTask(
        run_id,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
      )
    | record.StepAttemptFinished(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptInterrupted(run_id, _, _, _, _)
    | record.StepAttemptSuperseded(run_id, _, _, _, _, _) ->
      RequireKnownWorkflowRun(
        "orphan_step_attempt_without_workflow_run",
        run_id,
      )
    record.WorkflowStepRecoveryStarted(run_id, _, _, _, _, _, _, _)
    | record.WorkflowStepRecoveryFinished(run_id, _, _, _, _, _, _, _, _, _)
    | record.ScheduledJobDue(_, _, _, run_id, _)
    | record.ScheduledJobSkipped(_, _, _, run_id, _, _)
    | record.ScheduledRunPending(_, _, _, run_id, _, _)
    | record.ScheduledRunPendingBlocked(_, _, _, run_id, _, _)
    | record.ScheduledRunPendingCancelled(_, _, _, run_id, _, _)
    | record.ScheduledRunStarted(_, _, _, _, run_id, _, _, _)
    | record.ScheduledRunSucceeded(_, _, _, run_id, _, _, _, _)
    | record.ScheduledRunFailed(_, _, _, run_id, _, _, _, _, _)
    | record.ScheduledRunRetryScheduled(_, _, _, run_id, _, _, _, _)
    | record.ScheduledRunRetryCancelled(_, run_id, _, _)
    | record.ScheduledFailureReported(_, _, _, run_id, _, _, _, _)
    | record.ScheduledFailureReportFailed(_, _, _, run_id, _, _, _, _, _, _) ->
      RejectPrunedWorkflowRunOnly(run_id)
    record.ControlOperationQueued(_, _, _, _, run_id, _, _, _, _)
    | record.IssueCounterUpdated(_, _, _, _, _, run_id) ->
      optional_pruned_workflow_run_rejection(run_id)
    _ -> NoWorkflowRunRequirement
  }
}

fn optional_pruned_workflow_run_rejection(
  run_id: Option(String),
) -> AppendRecordWorkflowRequirement {
  case run_id {
    Some(run_id) -> RejectPrunedWorkflowRunOnly(run_id)
    _ -> NoWorkflowRunRequirement
  }
}

/// Preview deterministic retention without mutating the supplied projection.
pub fn preview(
  source: projection.Projection,
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
  marker_state: fn(String) -> MarkerState,
) -> PruneReport {
  select(source, policy, now_ms, marker_state).report
}

/// Select and remove all eligible runs as one projection operation.
pub fn prune(
  source: projection.Projection,
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
  marker_state: fn(String) -> MarkerState,
) -> PruneResult {
  case policy.enabled {
    False -> unchanged(source)
    True -> {
      let selection = select_candidates(source, policy, now_ms, marker_state)
      let run_ids = selection.candidates |> list.map(fn(run) { run.run_id })
      let pruned = projection.remove_run_ids(source, run_ids)
      PruneResult(
        projection: pruned,
        report: PruneReport(
          candidate_run_ids: run_ids,
          pruned_run_ids: run_ids,
          families_removed: family_difference(source, pruned),
          blockers: selection.blockers,
          before_bytes: projection_bytes(source),
          after_bytes: projection_bytes(pruned),
        ),
      )
    }
  }
}

fn select(
  source: projection.Projection,
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
  marker_state: fn(String) -> MarkerState,
) -> PruneResult {
  case policy.enabled {
    False -> unchanged(source)
    True -> prune(source, policy, now_ms, marker_state)
  }
}

fn unchanged(source: projection.Projection) -> PruneResult {
  let bytes = projection_bytes(source)
  PruneResult(
    projection: source,
    report: PruneReport(
      candidate_run_ids: [],
      pruned_run_ids: [],
      families_removed: zero_family_counts(),
      blockers: zero_blocker_counts(),
      before_bytes: bytes,
      after_bytes: bytes,
    ),
  )
}

fn select_candidates(
  source: projection.Projection,
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
  marker_state: fn(String) -> MarkerState,
) -> Selection {
  let initial = Selection(candidates: [], blockers: zero_blocker_counts())
  let classified =
    source.workflow_runs
    |> dict.to_list
    |> list.sort(by: fn(a, b) { string.compare(a.0, b.0) })
    |> list.fold(initial, fn(state, entry) {
      classify_run(source, policy, now_ms, marker_state, state, entry)
    })
  let ordinary = list.reverse(classified.candidates)
  let issue_candidates =
    ordinary
    |> list.filter(fn(run) { run.scheduled_job_id == None })
  let scheduled_candidates =
    ordinary
    |> list.filter(fn(run) { run.scheduled_job_id != None })
  let scheduled_to_prune =
    scheduled_candidates
    |> list.filter(fn(run) {
      scheduled_run_is_excess(run, scheduled_candidates, policy, now_ms)
    })
  Selection(
    candidates: issue_candidates |> list.append(scheduled_to_prune),
    blockers: classified.blockers,
  )
}

fn classify_run(
  source: projection.Projection,
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
  marker_state: fn(String) -> MarkerState,
  state: Selection,
  entry: #(String, projection.WorkflowRunStatus),
) -> Selection {
  let #(run_id, status) = entry
  case terminal_run(source, run_id, status) {
    Error(ActiveRun) ->
      Selection(..state, blockers: increment_active(state.blockers))
    Error(MalformedRun) ->
      Selection(..state, blockers: increment_malformed(state.blockers))
    Ok(run) -> {
      let age_ms = now_ms - run.terminal_at_ms
      case age_ms <= policy.terminal_grace_ms {
        True -> Selection(..state, blockers: increment_grace(state.blockers))
        False -> classify_safety_holds(source, marker_state, state, run)
      }
    }
  }
}

type TerminalRunError {
  ActiveRun
  MalformedRun
}

fn terminal_run(
  source: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> Result(TerminalRun, TerminalRunError) {
  let details = case status {
    projection.WorkflowRunActive(..) -> Error(ActiveRun)
    projection.WorkflowRunFinished(
      issue_id: issue_id,
      finished_at_ms: at,
      run_root: root,
      ..,
    )
    | projection.WorkflowRunInterrupted(
        issue_id: issue_id,
        interrupted_at_ms: at,
        run_root: root,
        ..,
      )
    | projection.WorkflowRunSuperseded(
        issue_id: issue_id,
        superseded_at_ms: at,
        run_root: root,
        ..,
      ) -> Ok(#(issue_id, root, at))
  }
  use details <- result.try(details)
  let #(issue_id, run_root, terminal_at_ms) = details
  case dict.get(source.workflow_run_provenances, run_id) {
    Error(Nil) -> Error(MalformedRun)
    Ok(provenance) -> {
      let scheduled_job_id = case provenance.issue_id == "" {
        True -> Some(provenance.issue_identifier)
        False -> None
      }
      case
        terminal_at_ms < 0
        || run_root == ""
        || provenance.issue_id != issue_id
        || provenance.run_root != run_root
      {
        True -> Error(MalformedRun)
        False ->
          Ok(TerminalRun(
            run_id,
            issue_id,
            run_root,
            terminal_at_ms,
            scheduled_job_id,
          ))
      }
    }
  }
}

fn classify_safety_holds(
  source: projection.Projection,
  marker_state: fn(String) -> MarkerState,
  state: Selection,
  run: TerminalRun,
) -> Selection {
  case dict.has_key(source.parked_issues, run.issue_id) {
    True -> Selection(..state, blockers: increment_parked(state.blockers))
    False ->
      case marker_state(run.run_root) {
        MarkerPresent ->
          Selection(..state, blockers: increment_marker(state.blockers))
        MarkerUnreadable ->
          Selection(
            ..state,
            blockers: increment_marker_unavailable(state.blockers),
          )
        MarkerAbsent ->
          case has_started_recovery(source, run.run_id) {
            True ->
              Selection(..state, blockers: increment_recovery(state.blockers))
            False ->
              case has_in_flight_control(source, run.run_id) {
                True ->
                  Selection(
                    ..state,
                    blockers: increment_control(state.blockers),
                  )
                False ->
                  case has_unsettled_publication(source, run.run_id) {
                    True ->
                      Selection(
                        ..state,
                        blockers: increment_publication(state.blockers),
                      )
                    False ->
                      case has_unsettled_outbox(source, run) {
                        True ->
                          Selection(
                            ..state,
                            blockers: increment_outbox(state.blockers),
                          )
                        False ->
                          Selection(..state, candidates: [
                            run,
                            ..state.candidates
                          ])
                      }
                  }
              }
          }
      }
  }
}

fn scheduled_run_is_excess(
  run: TerminalRun,
  scheduled: List(TerminalRun),
  policy: config_types.ProjectionRetentionConfig,
  now_ms: Int,
) -> Bool {
  let same_job =
    scheduled
    |> list.filter(fn(other) { other.scheduled_job_id == run.scheduled_job_id })
    |> list.sort(by: fn(a, b) {
      int.compare(b.terminal_at_ms, a.terminal_at_ms)
    })
  let newest = list.take(same_job, policy.scheduled_last_per_job)
  now_ms - run.terminal_at_ms > policy.scheduled_max_age_ms
  || !list.any(newest, fn(other) { other.run_id == run.run_id })
}

fn has_started_recovery(source: projection.Projection, run_id: String) -> Bool {
  source.step_recoveries
  |> dict.values
  |> list.any(fn(status) {
    case status {
      projection.StepRecoveryStartedStatus(run_id: candidate, ..) ->
        candidate == run_id
      projection.StepRecoveryFinishedStatus(..) -> False
    }
  })
}

fn has_in_flight_control(
  source: projection.Projection,
  run_id: String,
) -> Bool {
  source.control_operations
  |> dict.values
  |> list.any(fn(status) {
    status.run_id == Some(run_id)
    && { status.status == "queued" || status.status == "running" }
  })
}

fn has_unsettled_publication(
  source: projection.Projection,
  run_id: String,
) -> Bool {
  source.publication_latest_by_series
  |> dict.values
  |> list.any(fn(attempt) {
    attempt.run_id == run_id
    && { attempt.status == "planned" || attempt.status == "failed" }
  })
}

fn has_unsettled_outbox(
  source: projection.Projection,
  run: TerminalRun,
) -> Bool {
  source.outbox
  |> dict.to_list
  |> list.any(fn(entry) {
    let #(outbox_id, status) = entry
    outbox_unsettled(status) && outbox_may_reference_run(outbox_id, status, run)
  })
}

fn outbox_unsettled(status: projection.OutboxStatus) -> Bool {
  case status {
    projection.OutboxPending(..)
    | projection.OutboxPendingV2(..)
    | projection.OutboxPendingV2WithTask(..)
    | projection.OutboxAttempted(..)
    | projection.OutboxAttemptedWithTask(..)
    | projection.OutboxRetryScheduled(..)
    | projection.OutboxRetryScheduledWithTask(..) -> True
    _ -> False
  }
}

fn outbox_may_reference_run(
  outbox_id: String,
  status: projection.OutboxStatus,
  run: TerminalRun,
) -> Bool {
  let payload = outbox_payload(status)
  let issue_id = outbox_issue_id(status)
  let issue_matches = run.issue_id != "" && issue_id == Some(run.issue_id)
  exact_run_key(outbox_id, run.run_id)
  || issue_matches
  || case payload {
    None -> False
    Some(payload_json) ->
      case decode_payload_run_id(payload_json) {
        Ok(Some(payload_run_id)) -> payload_run_id == run.run_id
        Ok(None) -> False
        Error(Nil) -> issue_matches
      }
  }
}

fn exact_run_key(value: String, run_id: String) -> Bool {
  value == run_id
  || string.ends_with(value, ":" <> run_id)
  || string.ends_with(value, "/" <> run_id)
}

fn outbox_payload(status: projection.OutboxStatus) -> Option(String) {
  case status {
    projection.OutboxPendingV2(payload_json: payload, ..)
    | projection.OutboxPendingV2WithTask(payload_json: payload, ..)
    | projection.OutboxAttempted(payload_json: payload, ..)
    | projection.OutboxAttemptedWithTask(payload_json: payload, ..)
    | projection.OutboxRetryScheduled(payload_json: payload, ..)
    | projection.OutboxRetryScheduledWithTask(payload_json: payload, ..) ->
      Some(payload)
    _ -> None
  }
}

fn outbox_issue_id(status: projection.OutboxStatus) -> Option(String) {
  case status {
    projection.OutboxPending(issue_id: value, ..)
    | projection.OutboxPendingV2(issue_id: value, ..)
    | projection.OutboxAttempted(issue_id: value, ..)
    | projection.OutboxRetryScheduled(issue_id: value, ..)
    | projection.OutboxCompleted(issue_id: value, ..)
    | projection.OutboxFailed(issue_id: value, ..)
    | projection.OutboxPermanentlyFailed(issue_id: value, ..) -> Some(value)
    projection.OutboxPendingV2WithTask(task_ref: task, ..)
    | projection.OutboxAttemptedWithTask(task_ref: task, ..)
    | projection.OutboxRetryScheduledWithTask(task_ref: task, ..)
    | projection.OutboxCompletedWithTask(task_ref: task, ..)
    | projection.OutboxFailedWithTask(task_ref: task, ..)
    | projection.OutboxPermanentlyFailedWithTask(task_ref: task, ..) ->
      Some(task.task_remote_id)
  }
}

fn decode_payload_run_id(payload_json: String) -> Result(Option(String), Nil) {
  case json.parse(payload_json, payload_run_id_decoder()) {
    Ok(run_id) -> Ok(run_id)
    Error(_) -> Error(Nil)
  }
}

fn payload_run_id_decoder() -> decode.Decoder(Option(String)) {
  use run_id <- decode.optional_field(
    "run_id",
    None,
    decode.optional(decode.string),
  )
  decode.success(run_id)
}

fn projection_bytes(source: projection.Projection) -> Int {
  projection.to_string(source)
  |> bit_array.from_string
  |> bit_array.byte_size
}

fn family_difference(
  before: projection.Projection,
  after: projection.Projection,
) -> FamilyCounts {
  FamilyCounts(
    workflow_runs: dict.size(before.workflow_runs)
      - dict.size(after.workflow_runs),
    provenances: dict.size(before.workflow_run_provenances)
      - dict.size(after.workflow_run_provenances),
    task_refs: dict.size(before.workflow_task_refs)
      - dict.size(after.workflow_task_refs),
    input_manifests: dict.size(before.workflow_input_manifests)
      - dict.size(after.workflow_input_manifests),
    interface_snapshots: dict.size(before.workflow_interface_snapshots)
      - dict.size(after.workflow_interface_snapshots),
    output_manifests: dict.size(before.workflow_output_manifests)
      - dict.size(after.workflow_output_manifests),
    repairs: dict.size(before.workflow_repairs)
      - dict.size(after.workflow_repairs),
    step_attempts: dict.size(before.step_attempts)
      - dict.size(after.step_attempts),
    step_recoveries: dict.size(before.step_recoveries)
      - dict.size(after.step_recoveries),
    publication_attempts: dict.size(before.publication_attempts)
      - dict.size(after.publication_attempts),
    control_operations: dict.size(before.control_operations)
      - dict.size(after.control_operations),
    outbox_entries: dict.size(before.outbox) - dict.size(after.outbox),
  )
}

fn zero_family_counts() -> FamilyCounts {
  FamilyCounts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
}

fn zero_blocker_counts() -> BlockerCounts {
  BlockerCounts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
}

fn increment_active(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, active: value.active + 1)
}

fn increment_grace(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, within_grace: value.within_grace + 1)
}

fn increment_parked(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, parked: value.parked + 1)
}

fn increment_marker(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, retained_workspace: value.retained_workspace + 1)
}

fn increment_marker_unavailable(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, marker_unavailable: value.marker_unavailable + 1)
}

fn increment_recovery(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, recovery_started: value.recovery_started + 1)
}

fn increment_control(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, control_in_flight: value.control_in_flight + 1)
}

fn increment_publication(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, publication_unsettled: value.publication_unsettled + 1)
}

fn increment_outbox(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, outbox_unsettled: value.outbox_unsettled + 1)
}

fn increment_malformed(value: BlockerCounts) -> BlockerCounts {
  BlockerCounts(..value, malformed_association: value.malformed_association + 1)
}
