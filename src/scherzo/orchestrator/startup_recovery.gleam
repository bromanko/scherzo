import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/claim_abandonment
import scherzo/config/types as config_types
import scherzo/log
import scherzo/orchestrator/core
import scherzo/orchestrator/runtime_recovery
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/scheduled_runtime
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/recovery as session_recovery
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_fingerprint

const scheduled_failure_publication_kind = "scheduled_failure_publication"

pub type DaemonState {
  DaemonState(
    phase: Phase,
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    next_waiter_id: Int,
    pending_waiters: Dict(Int, process.Subject(Result(Nil, Nil))),
  )
}

pub type Phase {
  Pending(StartupRecovery)
  Running(Step, StartupRecovery)
  Ready
  Failed(String)
}

pub type Step {
  StageApplyRecovery
  StageApplyScheduledRecovery
  StageResumeWorkflows
  StageCheckInvariants
  StageFinish
}

pub fn daemon_state(plan: StartupRecovery) -> DaemonState {
  DaemonState(
    phase: Pending(plan),
    recovery_by_issue: plan.recovery_by_issue,
    next_waiter_id: 1,
    pending_waiters: dict.new(),
  )
}

pub fn recovery_by_issue(
  state: DaemonState,
) -> Dict(String, session_event.RecoveryInfo) {
  state.recovery_by_issue
}

pub fn clear_recovery(state: DaemonState, issue_id: String) -> DaemonState {
  DaemonState(
    ..state,
    recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
  )
}

pub fn phase(state: DaemonState) -> Phase {
  state.phase
}

pub fn is_ready(state: DaemonState) -> Bool {
  case state.phase {
    Ready -> True
    _ -> False
  }
}

pub fn set_phase(state: DaemonState, phase: Phase) -> DaemonState {
  DaemonState(..state, phase: phase)
}

pub fn register_waiter(
  state: DaemonState,
  reply: process.Subject(Result(Nil, Nil)),
) -> #(DaemonState, Int) {
  let waiter_id = state.next_waiter_id
  #(
    DaemonState(
      ..state,
      next_waiter_id: waiter_id + 1,
      pending_waiters: dict.insert(state.pending_waiters, waiter_id, reply),
    ),
    waiter_id,
  )
}

pub fn remove_waiter(
  state: DaemonState,
  waiter_id: Int,
) -> #(DaemonState, Option(process.Subject(Result(Nil, Nil)))) {
  #(
    DaemonState(
      ..state,
      pending_waiters: dict.delete(state.pending_waiters, waiter_id),
    ),
    case dict.get(state.pending_waiters, waiter_id) {
      Ok(reply) -> Some(reply)
      Error(Nil) -> None
    },
  )
}

pub fn complete_waiters(
  state: DaemonState,
  result: Result(Nil, Nil),
) -> #(
  DaemonState,
  List(#(process.Subject(Result(Nil, Nil)), Result(Nil, Nil))),
) {
  #(
    DaemonState(..state, pending_waiters: dict.new()),
    dict.values(state.pending_waiters),
  )
  |> map_waiter_result(result)
}

fn map_waiter_result(
  state_and_waiters: #(DaemonState, List(process.Subject(Result(Nil, Nil)))),
  result: Result(Nil, Nil),
) -> #(
  DaemonState,
  List(#(process.Subject(Result(Nil, Nil)), Result(Nil, Nil))),
) {
  let #(state, waiters) = state_and_waiters
  #(state, list.map(waiters, fn(reply) { #(reply, result) }))
}

pub type StartupError {
  StartupError(code: String, message: String)
}

pub type Dependencies {
  Dependencies(
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
    now_ms: fn() -> Int,
    sleep_ms: fn(Int) -> Nil,
  )
}

pub type ScheduledRecoveryEffect {
  AppendLedger(record_bodies: List(record.RecordBody), failure_event: String)
  ApplyScheduledRuntimeActions(
    actions: List(scheduled_runtime.Action),
    append_retry_record: Bool,
  )
  ScheduleReportRetryTimer(run_id: String, generation: Int, delay_ms: Int)
  BeginFailureReport(request: scheduled_runtime.FailureReportRequest)
}

pub type ScheduledRecovery {
  ScheduledRecovery(
    runtime: scheduled_runtime.Runtime,
    effects: List(ScheduledRecoveryEffect),
  )
}

pub type StartupRecovery {
  StartupRecovery(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(recovery.RecoveredRetry),
    cleanup_workspaces: List(recovery.CleanupRequest),
    outbox_to_replay: List(recovery.OutboxReplay),
    park_reports: List(adapter.ParkReport),
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    warnings: List(String),
    workflow_resumptions: List(recovery.RecoveredWorkflowRun),
    scheduled: ScheduledRecovery,
    projection: projection.Projection,
  )
}

type RecoveryTaskRefresh {
  RecoveryTaskRefresh(
    issues: List(tracker_issue.Issue),
    omitted_issue_ids: List(String),
    unrefreshed_issue_ids: List(String),
    warnings: List(String),
  )
}

pub fn load(
  bundle: runtime_bundle.RuntimeBundle,
  tracker_adapter: adapter.TrackerAdapter,
  dependencies: Dependencies,
  secrets: List(String),
) -> Result(StartupRecovery, StartupError) {
  let effective = bundle.effective
  use ledger_path <- try_startup(
    ledger.path_for_workspace_root(effective.workspace.root)
    |> map_ledger_error("ledger_path_failed"),
  )
  use replayed <- try_startup(
    ledger.replay(ledger_path)
    |> map_ledger_error("ledger_replay_failed"),
  )
  case replayed.truncated_tail {
    True ->
      emit_log(
        dependencies,
        "warn",
        "ledger_truncated_tail_ignored",
        [],
        secrets,
      )
    False -> Nil
  }
  let refresh =
    fetch_recovery_task_states(
      tracker_adapter,
      recovery.recovery_task_refs(replayed.projection),
      dependencies,
    )
  let issue_observations = recovery_issue_observations(refresh)
  use recovery_plan <- try_startup(
    recovery.plan_with_issue_observations(
      replayed.projection,
      effective,
      issue_observations,
      dependencies.now_ms(),
    )
    |> map_recovery_error,
  )
  let workflow_candidates = recovery.workflow_candidates(replayed.projection)
  let observations =
    workflow_recovery_observations(
      bundle,
      workflow_candidates,
      issue_observations,
    )
  use workflow_finalization <- try_startup(
    recovery.finalize_workflow_candidates_with_config(
      replayed.projection,
      workflow_candidates,
      observations,
      artifact_store.new(effective.workspace.root),
      dependencies.now_ms(),
      effective,
    )
    |> map_recovery_error,
  )
  let records_to_append =
    list.append(
      recovery_plan.records_to_append,
      workflow_finalization.records_to_append,
    )
  use Nil <- try_startup(
    ledger.append_many(ledger_path, records_to_append, True)
    |> map_ledger_error("ledger_recovery_append_failed"),
  )
  let runtime =
    runtime_recovery.runtime_with_appended_park_records(
      recovery_plan.runtime,
      records_to_append,
    )
  let projected = projection.fold_from(replayed.projection, records_to_append)
  let scheduled =
    recover_scheduled_runtime_with_outbox_replay(
      bundle,
      dependencies.now_ms(),
      projection.scheduled_statuses(projected),
      recovery_plan.outbox_to_replay,
    )
  Ok(StartupRecovery(
    runtime: runtime,
    retry_timers: recovery_plan.retry_timers,
    cleanup_workspaces: recovery_plan.cleanup_workspaces,
    outbox_to_replay: recovery_plan.outbox_to_replay,
    park_reports: startup_park_reports(records_to_append),
    recovery_by_issue: startup_recovery_by_issue(
      replayed.projection,
      recovery_plan,
    ),
    warnings: list.append(
      refresh.warnings,
      list.append(recovery_plan.warnings, workflow_finalization.warnings),
    ),
    workflow_resumptions: workflow_finalization.resumptions,
    scheduled: scheduled,
    projection: projected,
  ))
}

pub fn recover_scheduled_runtime(
  bundle: runtime_bundle.RuntimeBundle,
  now_ms: Int,
  scheduled_statuses: List(projection.ScheduledJobStatus),
) -> ScheduledRecovery {
  recover_scheduled_runtime_with_outbox_replay(
    bundle,
    now_ms,
    scheduled_statuses,
    [],
  )
}

pub fn recover_scheduled_runtime_with_outbox_replay(
  bundle: runtime_bundle.RuntimeBundle,
  now_ms: Int,
  scheduled_statuses: List(projection.ScheduledJobStatus),
  outbox_to_replay: List(recovery.OutboxReplay),
) -> ScheduledRecovery {
  let runtime =
    scheduled_runtime.from_next_due(
      dict.to_list(initial_scheduled_next_due(
        bundle.orchestrator.scheduled_jobs,
        now_ms,
        scheduled_statuses,
      )),
    )
  let #(runtime, effects) =
    list.fold(scheduled_statuses, #(runtime, []), fn(acc, status) {
      let #(runtime, effects) = acc
      recover_scheduled_status(
        runtime,
        effects,
        bundle.orchestrator.scheduled_jobs,
        now_ms,
        status,
        outbox_to_replay,
      )
    })
  ScheduledRecovery(runtime: runtime, effects: list.reverse(effects))
}

fn initial_scheduled_next_due(
  jobs: List(config_types.ScheduledJobConfig),
  now_ms: Int,
  scheduled_statuses: List(projection.ScheduledJobStatus),
) -> Dict(String, Int) {
  jobs
  |> list.filter(fn(job) { job.enabled })
  |> list.fold(dict.new(), fn(acc, job) {
    let next_due = case scheduled_status_for(scheduled_statuses, job.id) {
      Some(status) ->
        case status.last_due_at_ms {
          Some(due_at_ms) ->
            schedule_core.next_due_after_persisted_due(
              due_at_ms,
              now_ms,
              job.every_ms,
            )
          None -> schedule_core.initial_next_due(now_ms, job.every_ms)
        }
      None -> schedule_core.initial_next_due(now_ms, job.every_ms)
    }
    dict.insert(acc, job.id, next_due)
  })
}

fn scheduled_status_for(
  statuses: List(projection.ScheduledJobStatus),
  job_id: String,
) -> Option(projection.ScheduledJobStatus) {
  case list.find(statuses, fn(status) { status.job_id == job_id }) {
    Ok(status) -> Some(status)
    Error(Nil) -> None
  }
}

fn scheduled_job_by_id(
  jobs: List(config_types.ScheduledJobConfig),
  job_id: String,
) -> Result(config_types.ScheduledJobConfig, Nil) {
  jobs |> list.find(fn(job) { job.id == job_id })
}

fn recover_scheduled_status(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  jobs: List(config_types.ScheduledJobConfig),
  now_ms: Int,
  status: projection.ScheduledJobStatus,
  outbox_to_replay: List(recovery.OutboxReplay),
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  case scheduled_job_by_id(jobs, status.job_id), status.current_run {
    Ok(job), Some(run) ->
      case job.enabled {
        False ->
          recover_disabled_scheduled_run(runtime, effects, now_ms, status, run)
        True ->
          recover_enabled_scheduled_run(
            runtime,
            effects,
            now_ms,
            job,
            status,
            run,
            outbox_to_replay,
          )
      }
    Error(Nil), Some(run) ->
      recover_disabled_scheduled_run(runtime, effects, now_ms, status, run)
    _, None -> #(runtime, effects)
  }
}

fn recover_enabled_scheduled_run(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  now_ms: Int,
  job: config_types.ScheduledJobConfig,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
  outbox_to_replay: List(recovery.OutboxReplay),
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  case status.state {
    projection.ScheduledDuePending
    | projection.ScheduledPaused
    | projection.ScheduledWaitingForGlobalSlot -> #(
      scheduled_runtime.insert_pending_start(
        runtime,
        scheduled_runtime.PendingStart(
          job_id: job.id,
          workflow_id: job.workflow,
          due_at_ms: run.due_at_ms,
          run_id: run.run_id,
          trigger: run.trigger,
          requested_at_ms: now_ms,
          attempt: normalized_scheduled_attempt(run.attempt),
          blocking_reason: optional_string_or_default(run.reason, ""),
        ),
      ),
      effects,
    )
    projection.ScheduledActive ->
      recover_interrupted_scheduled_run(runtime, effects, now_ms, job, run)
    projection.ScheduledRetryWaiting ->
      recover_scheduled_retry_waiting(runtime, effects, now_ms, job, run)
    projection.ScheduledReportRetryWaiting ->
      recover_scheduled_report_retry_waiting(
        runtime,
        effects,
        now_ms,
        job,
        status,
        outbox_to_replay,
      )
    projection.ScheduledIdle
    | projection.ScheduledTerminalSuccess
    | projection.ScheduledTerminalFailure
    | projection.ScheduledQuarantined -> #(runtime, effects)
  }
}

fn normalized_scheduled_attempt(attempt: Int) -> Int {
  case attempt <= 0 {
    True -> 1
    False -> attempt
  }
}

fn optional_string_or_default(
  value: Option(String),
  default: String,
) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn recover_scheduled_retry_waiting(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  now_ms: Int,
  job: config_types.ScheduledJobConfig,
  run: projection.ScheduledRunSummary,
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  let attempt = normalized_scheduled_attempt(run.attempt)
  let reason = optional_string_or_default(run.reason, "whole_run_retry_removed")
  let request =
    scheduled_runtime.FailureReportRequest(
      job_id: job.id,
      workflow_id: job.workflow,
      due_at_ms: run.due_at_ms,
      run_id: run.run_id,
      attempt: attempt,
      reason: reason,
      run_root: run.run_root,
      session_id: run.session_id,
    )
  #(runtime, [
    BeginFailureReport(request: request),
    AppendLedger(
      record_bodies: [
        record.ScheduledRunFailed(
          job.id,
          job.workflow,
          run.due_at_ms,
          run.run_id,
          attempt,
          now_ms,
          reason,
          True,
          run.run_root,
        ),
      ],
      failure_event: "scheduled_recovery_append_failed",
    ),
    ..effects
  ])
}

fn recover_scheduled_report_retry_waiting(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  now_ms: Int,
  job: config_types.ScheduledJobConfig,
  status: projection.ScheduledJobStatus,
  outbox_to_replay: List(recovery.OutboxReplay),
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  case status.report_retry {
    None -> #(runtime, effects)
    Some(report_retry) -> {
      let runtime =
        scheduled_runtime.insert_report_retry(
          runtime,
          scheduled_runtime.ReportRetryStart(
            job_id: job.id,
            run_id: report_retry.run_id,
            generation: report_retry.generation,
          ),
        )
      case
        scheduled_failure_outbox_replay_exists(outbox_to_replay, report_retry)
      {
        True -> #(runtime, effects)
        False -> {
          let delay_ms = case report_retry.next_retry_at_ms <= now_ms {
            True -> 0
            False -> report_retry.next_retry_at_ms - now_ms
          }
          #(runtime, [
            ScheduleReportRetryTimer(
              run_id: report_retry.run_id,
              generation: report_retry.generation,
              delay_ms: delay_ms,
            ),
            ..effects
          ])
        }
      }
    }
  }
}

fn scheduled_failure_outbox_replay_exists(
  outbox_to_replay: List(recovery.OutboxReplay),
  report_retry: projection.ScheduledReportRetry,
) -> Bool {
  list.any(outbox_to_replay, fn(replay) {
    let recovery.OutboxReplay(outbox_id, _, outbox_kind, dedupe_key, _) = replay
    outbox_kind == scheduled_failure_publication_kind
    && {
      outbox_id == report_retry.dedupe_key
      || dedupe_key == report_retry.dedupe_key
    }
  })
}

fn recover_disabled_scheduled_run(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  now_ms: Int,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  case status.state {
    projection.ScheduledDuePending
    | projection.ScheduledPaused
    | projection.ScheduledWaitingForGlobalSlot -> #(runtime, [
      AppendLedger(
        record_bodies: [
          record.ScheduledRunPendingCancelled(
            status.job_id,
            status.workflow_id,
            run.due_at_ms,
            run.run_id,
            "job_disabled",
            now_ms,
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      ..effects
    ])
    projection.ScheduledActive -> #(runtime, [
      AppendLedger(
        record_bodies: [
          record.ScheduledRunFailed(
            status.job_id,
            status.workflow_id,
            run.due_at_ms,
            run.run_id,
            normalized_scheduled_attempt(run.attempt),
            now_ms,
            "daemon_restart",
            True,
            run.run_root,
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      ..effects
    ])
    projection.ScheduledRetryWaiting -> #(runtime, [
      AppendLedger(
        record_bodies: [
          record.ScheduledRunRetryCancelled(
            status.job_id,
            run.run_id,
            0,
            "job_disabled",
          ),
        ],
        failure_event: "scheduled_recovery_append_failed",
      ),
      ..effects
    ])
    _ -> #(runtime, effects)
  }
}

fn recover_interrupted_scheduled_run(
  runtime: scheduled_runtime.Runtime,
  effects: List(ScheduledRecoveryEffect),
  now_ms: Int,
  job: config_types.ScheduledJobConfig,
  run: projection.ScheduledRunSummary,
) -> #(scheduled_runtime.Runtime, List(ScheduledRecoveryEffect)) {
  let attempt = normalized_scheduled_attempt(run.attempt)
  let #(runtime, follow_up) =
    scheduled_runtime.worker_failure_follow_up(
      runtime,
      job.id,
      job.workflow,
      run.due_at_ms,
      run.run_id,
      attempt,
      "daemon_restart",
      run.run_root,
      run.session_id,
    )
  let effects = [
    AppendLedger(
      record_bodies: [
        record.ScheduledRunFailed(
          job.id,
          job.workflow,
          run.due_at_ms,
          run.run_id,
          attempt,
          now_ms,
          "daemon_restart",
          True,
          run.run_root,
        ),
      ],
      failure_event: "scheduled_recovery_append_failed",
    ),
    ..effects
  ]
  case follow_up {
    scheduled_runtime.WorkerFailureReport(request) -> #(runtime, [
      BeginFailureReport(request: request),
      ..effects
    ])
  }
}

fn emit_log(
  dependencies: Dependencies,
  level: String,
  event: String,
  fields: List(log.Field),
  secrets: List(String),
) -> Nil {
  case dependencies.logger(level, event, fields, secrets) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

fn startup_park_reports(
  records: List(record.LedgerRecord),
) -> List(adapter.ParkReport) {
  let run_ids = startup_park_report_run_ids(records)
  startup_park_reports_loop(records, run_ids, [], [])
}

fn startup_park_reports_loop(
  records: List(record.LedgerRecord),
  run_ids: Dict(String, String),
  seen_issue_ids: List(String),
  reports: List(adapter.ParkReport),
) -> List(adapter.ParkReport) {
  case records {
    [] -> list.reverse(reports)
    [ledger_record, ..rest] ->
      case ledger_record.body {
        record.IssueParked(issue_id, issue_identifier, reason_text, _) ->
          add_startup_park_report(
            rest,
            run_ids,
            seen_issue_ids,
            reports,
            issue_id,
            issue_identifier,
            reason_text,
            None,
          )
        record.IssueParkedV2(
          issue_id,
          issue_identifier,
          reason_text,
          release_policy,
          _,
          _,
        ) ->
          add_startup_park_report(
            rest,
            run_ids,
            seen_issue_ids,
            reports,
            issue_id,
            issue_identifier,
            reason_text,
            Some(release_policy),
          )
        _ -> startup_park_reports_loop(rest, run_ids, seen_issue_ids, reports)
      }
  }
}

fn add_startup_park_report(
  rest: List(record.LedgerRecord),
  run_ids: Dict(String, String),
  seen_issue_ids: List(String),
  reports: List(adapter.ParkReport),
  issue_id: String,
  issue_identifier: String,
  reason_text: String,
  release_policy: Option(String),
) -> List(adapter.ParkReport) {
  case
    string.trim(issue_id) == ""
    || list.contains(seen_issue_ids, issue_id)
    || claim_abandonment.is_reason_text(reason_text)
  {
    True -> startup_park_reports_loop(rest, run_ids, seen_issue_ids, reports)
    False ->
      startup_park_reports_loop(rest, run_ids, [issue_id, ..seen_issue_ids], [
        adapter.ParkReport(
          task: task.TaskRef(
            backend_kind: "linear",
            remote_id: issue_id,
            key: Some(issue_identifier),
            url: None,
          ),
          issue_identifier: issue_identifier,
          reason: reason_text,
          release_policy: release_policy,
          run_id: optional_run_id(run_ids, issue_id),
        ),
        ..reports
      ])
  }
}

fn startup_park_report_run_ids(
  records: List(record.LedgerRecord),
) -> Dict(String, String) {
  list.fold(records, dict.new(), fn(run_ids, ledger_record) {
    case ledger_record.body {
      record.RunInterrupted(run_id, issue_id, _) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      record.WorkflowRunInterrupted(run_id, _, issue_id, _) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      record.IssueCounterUpdated(issue_id, _, _, _, _, Some(run_id)) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      _ -> run_ids
    }
  })
}

fn insert_run_id_if_missing(
  run_ids: Dict(String, String),
  issue_id: String,
  run_id: String,
) -> Dict(String, String) {
  case string.trim(run_id) == "" || dict.has_key(run_ids, issue_id) {
    True -> run_ids
    False -> dict.insert(run_ids, issue_id, run_id)
  }
}

fn optional_run_id(
  run_ids: Dict(String, String),
  issue_id: String,
) -> Option(String) {
  case dict.get(run_ids, issue_id) {
    Ok(run_id) -> Some(run_id)
    Error(Nil) -> None
  }
}

fn workflow_recovery_observations(
  bundle: runtime_bundle.RuntimeBundle,
  candidates: List(recovery.WorkflowRecoveryCandidate),
  issue_observations: Dict(String, recovery.IssueRefreshObservation),
) -> Dict(String, recovery.CurrentWorkflowObservation) {
  candidates
  |> list.map(fn(candidate) {
    let observation = case dict.get(issue_observations, candidate.issue_id) {
      Ok(recovery.RefreshedIssue(issue)) ->
        current_workflow_observation(bundle, issue)
      Ok(recovery.RefreshUnavailable) -> recovery.TrackerRefreshUnavailable
      Ok(recovery.ConfirmedUnavailable) | Error(Nil) ->
        recovery.IssueUnavailable
    }
    #(candidate.run_id, observation)
  })
  |> dict.from_list
}

pub fn current_workflow_observation(
  bundle: runtime_bundle.RuntimeBundle,
  issue: tracker_issue.Issue,
) -> recovery.CurrentWorkflowObservation {
  case runtime_bundle.select_workflow(bundle, issue) {
    Error(runtime_bundle.BundleError(code, message)) ->
      recovery.WorkflowUnavailable(code <> ":" <> message)
    Ok(#(workflow_id, dag)) ->
      case
        workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
      {
        Error(err) ->
          recovery.WorkflowUnavailable(
            "workflow_fingerprint_failed:" <> fingerprint_error_message(err),
          )
        Ok(fingerprint) ->
          recovery.CurrentWorkflow(
            issue,
            workflow_id,
            fingerprint,
            core.issue_fingerprint(issue),
            dag,
            bundle.effective.workspace.root,
          )
      }
  }
}

pub fn fingerprint_error_message(
  err: workflow_fingerprint.FingerprintError,
) -> String {
  case err {
    workflow_fingerprint.PromptFileReadFailed(path) ->
      "prompt_file_read_failed:" <> path
    workflow_fingerprint.UnsupportedWorkflowShape(reason) ->
      "unsupported_workflow_shape:" <> reason
    workflow_fingerprint.WorkspaceProfileUnavailable(profile_name) ->
      "workspace_profile_unavailable:" <> profile_name
  }
}

fn fetch_recovery_task_states(
  tracker_adapter: adapter.TrackerAdapter,
  task_refs: List(record.TaskRefFields),
  dependencies: Dependencies,
) -> RecoveryTaskRefresh {
  let refs =
    task_refs
    |> list.filter(fn(ref) { ref.task_backend_kind == tracker_adapter.kind })
    |> list.map(record_task_ref_to_task_ref)
  fetch_recovery_task_chunks(
    tracker_adapter,
    chunk_task_refs(refs, 50),
    dependencies,
    [],
    [],
    [],
    [],
  )
}

fn fetch_recovery_task_chunks(
  tracker_adapter: adapter.TrackerAdapter,
  chunks: List(List(task.TaskRef)),
  dependencies: Dependencies,
  issue_acc: List(tracker_issue.Issue),
  omitted_acc: List(String),
  unrefreshed_acc: List(String),
  warning_acc: List(String),
) -> RecoveryTaskRefresh {
  case chunks {
    [] ->
      RecoveryTaskRefresh(
        issues: list.reverse(issue_acc),
        omitted_issue_ids: list.reverse(omitted_acc),
        unrefreshed_issue_ids: list.reverse(unrefreshed_acc),
        warnings: list.reverse(warning_acc),
      )
    [chunk, ..rest] -> {
      let requested_issue_ids = list.map(chunk, fn(ref) { ref.remote_id })
      case
        fetch_recovery_task_chunk(tracker_adapter, chunk, dependencies, [
          50,
          200,
        ])
      {
        Ok(issues) -> {
          let refreshed_issue_ids = list.map(issues, fn(issue) { issue.id })
          let omitted_issue_ids =
            list.filter(requested_issue_ids, fn(issue_id) {
              !list.contains(refreshed_issue_ids, issue_id)
            })
          fetch_recovery_task_chunks(
            tracker_adapter,
            rest,
            dependencies,
            list.append(list.reverse(issues), issue_acc),
            list.append(list.reverse(omitted_issue_ids), omitted_acc),
            unrefreshed_acc,
            warning_acc,
          )
        }
        Error(err) -> {
          let stop_remaining = tracker_refresh_error_stops_remaining(err)
          let remaining_issue_ids = case stop_remaining {
            True -> issue_ids_for_chunks(rest)
            False -> []
          }
          let remaining_chunks = case stop_remaining {
            True -> []
            False -> rest
          }
          fetch_recovery_task_chunks(
            tracker_adapter,
            remaining_chunks,
            dependencies,
            issue_acc,
            omitted_acc,
            list.append(
              list.reverse(list.append(requested_issue_ids, remaining_issue_ids)),
              unrefreshed_acc,
            ),
            [
              "tracker_refresh_unavailable:"
                <> tracker_adapter.kind
                <> ":"
                <> tracker_error_message(err),
              ..warning_acc
            ],
          )
        }
      }
    }
  }
}

fn fetch_recovery_task_chunk(
  tracker_adapter: adapter.TrackerAdapter,
  refs: List(task.TaskRef),
  dependencies: Dependencies,
  retry_delays_ms: List(Int),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  case refresh_runtime_issues_by_refs(tracker_adapter, refs) {
    Ok(issues) -> Ok(issues)
    Error(err) ->
      case tracker_refresh_error_retryable(err), retry_delays_ms {
        True, [delay_ms, ..rest] -> {
          dependencies.sleep_ms(delay_ms)
          fetch_recovery_task_chunk(tracker_adapter, refs, dependencies, rest)
        }
        _, _ -> Error(err)
      }
  }
}

fn tracker_refresh_error_retryable(err: adapter.TrackerError) -> Bool {
  case err {
    adapter.Transient(_) -> True
    adapter.Unauthorized(_)
    | adapter.NotFound(_)
    | adapter.Permanent(_)
    | adapter.UnsupportedCapability(_)
    | adapter.DecodeFailed(_) -> False
  }
}

fn tracker_refresh_error_stops_remaining(err: adapter.TrackerError) -> Bool {
  case err {
    adapter.Unauthorized(_) | adapter.UnsupportedCapability(_) -> True
    adapter.NotFound(_)
    | adapter.Transient(_)
    | adapter.Permanent(_)
    | adapter.DecodeFailed(_) -> False
  }
}

fn issue_ids_for_chunks(chunks: List(List(task.TaskRef))) -> List(String) {
  chunks
  |> list.fold([], fn(issue_ids, chunk) {
    list.append(issue_ids, list.map(chunk, fn(ref) { ref.remote_id }))
  })
}

fn recovery_issue_observations(
  refresh: RecoveryTaskRefresh,
) -> Dict(String, recovery.IssueRefreshObservation) {
  let observations =
    refresh.issues
    |> list.map(fn(issue) { #(issue.id, recovery.RefreshedIssue(issue)) })
    |> dict.from_list
  let observations =
    list.fold(
      refresh.omitted_issue_ids,
      observations,
      fn(observations, issue_id) {
        dict.insert(observations, issue_id, recovery.ConfirmedUnavailable)
      },
    )
  list.fold(
    refresh.unrefreshed_issue_ids,
    observations,
    fn(observations, issue_id) {
      dict.insert(observations, issue_id, recovery.RefreshUnavailable)
    },
  )
}

fn refresh_runtime_issues_by_refs(
  tracker_adapter: adapter.TrackerAdapter,
  refs: List(task.TaskRef),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  case tracker_adapter.task_source.refresh_by_refs(refs) {
    Ok(tasks) -> tasks_to_runtime_issues(tracker_adapter.kind, tasks)
    Error(err) -> Error(err)
  }
}

fn tasks_to_runtime_issues(
  backend_kind: String,
  tasks: List(task.Task),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  case tasks {
    [] -> Ok([])
    [item, ..rest] -> {
      use issue <- result.try(task_to_runtime_issue(backend_kind, item))
      use rest_issues <- result.try(tasks_to_runtime_issues(backend_kind, rest))
      Ok([issue, ..rest_issues])
    }
  }
}

fn task_to_runtime_issue(
  backend_kind: String,
  item: task.Task,
) -> Result(tracker_issue.Issue, adapter.TrackerError) {
  case item.ref.backend_kind == backend_kind {
    True -> Ok(task.to_runtime_issue(item))
    False ->
      Error(adapter.Permanent(
        "tracker adapter returned task for backend "
        <> item.ref.backend_kind
        <> " while "
        <> backend_kind
        <> " was expected",
      ))
  }
}

fn tracker_error_message(err: adapter.TrackerError) -> String {
  case err {
    adapter.Unauthorized(message) -> message
    adapter.NotFound(ref) -> "task not found: " <> ref.remote_id
    adapter.Transient(message) -> message
    adapter.Permanent(message) -> message
    adapter.UnsupportedCapability(capability) ->
      "unsupported tracker capability: " <> capability
    adapter.DecodeFailed(message) -> message
  }
}

fn record_task_ref_to_task_ref(ref: record.TaskRefFields) -> task.TaskRef {
  task.TaskRef(
    backend_kind: ref.task_backend_kind,
    remote_id: ref.task_remote_id,
    key: ref.task_key,
    url: ref.task_url,
  )
}

fn chunk_task_refs(
  values: List(task.TaskRef),
  size: Int,
) -> List(List(task.TaskRef)) {
  case values {
    [] -> []
    values -> {
      let chunk = list.take(values, size)
      let rest = list.drop(values, size)
      [chunk, ..chunk_task_refs(rest, size)]
    }
  }
}

fn startup_recovery_by_issue(
  projection: projection.Projection,
  recovery_plan: recovery.RecoveryPlan,
) -> Dict(String, session_event.RecoveryInfo) {
  dict.new()
  |> insert_interrupted_recovery(projection)
  |> insert_recovered_retry_recovery(recovery_plan.retry_timers)
  |> insert_parked_recovery(projection)
  |> insert_cleanup_recovery(recovery_plan.cleanup_workspaces)
}

fn insert_interrupted_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  projection: projection.Projection,
) -> Dict(String, session_event.RecoveryInfo) {
  projection.runs
  |> dict.to_list
  |> list.fold(acc, fn(acc, entry) {
    let #(run_id, status) = entry
    case session_recovery.interrupted_run(run_id, status, None) {
      Some(info) -> dict.insert(acc, issue_id_for_run_status(status), info)
      None -> acc
    }
  })
}

fn insert_recovered_retry_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  retries: List(recovery.RecoveredRetry),
) -> Dict(String, session_event.RecoveryInfo) {
  list.fold(retries, acc, fn(acc, retry) {
    let recovery.RecoveredRetry(issue_id, _, _, _, reason) = retry
    insert_if_missing(
      acc,
      issue_id,
      session_recovery.recovered("recovery.recovered_retry", Some(reason)),
    )
  })
}

fn insert_parked_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  projection: projection.Projection,
) -> Dict(String, session_event.RecoveryInfo) {
  projection.parked_issues
  |> dict.to_list
  |> list.fold(acc, fn(acc, entry) {
    let #(issue_id, parked) = entry
    dict.insert(acc, issue_id, session_recovery.parked_issue(parked))
  })
}

fn insert_cleanup_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  cleanups: List(recovery.CleanupRequest),
) -> Dict(String, session_event.RecoveryInfo) {
  list.fold(cleanups, acc, fn(acc, cleanup) {
    let recovery.CleanupRequest(issue_id, _, _) = cleanup
    dict.insert(acc, issue_id, session_recovery.cleanup_request(cleanup))
  })
}

fn insert_if_missing(
  acc: Dict(String, session_event.RecoveryInfo),
  issue_id: String,
  info: session_event.RecoveryInfo,
) -> Dict(String, session_event.RecoveryInfo) {
  case dict.has_key(acc, issue_id) {
    True -> acc
    False -> dict.insert(acc, issue_id, info)
  }
}

fn issue_id_for_run_status(status: projection.RunStatus) -> String {
  case status {
    projection.RunRunning(issue_id, ..)
    | projection.RunInterrupted(issue_id, ..)
    | projection.RunFinished(issue_id, ..) -> issue_id
  }
}

fn map_ledger_error(
  result: Result(a, ledger.LedgerError),
  code: String,
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(StartupError(code, ledger_error_message(err)))
  }
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn map_recovery_error(
  result: Result(a, recovery.RecoveryError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(StartupError(
        "startup_recovery_failed",
        recovery.describe_error(err),
      ))
  }
}

fn try_startup(
  result: Result(a, StartupError),
  next: fn(a) -> Result(b, StartupError),
) -> Result(b, StartupError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
