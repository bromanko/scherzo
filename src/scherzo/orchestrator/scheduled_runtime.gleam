import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, Some}
import scherzo/orchestrator/schedule_core
import scherzo/retry_policy
import scherzo/state/record
import scherzo/tracker/adapter

const default_report_max_attempts_value = 5

pub type Runtime {
  Runtime(
    next_due: Dict(String, Int),
    pending_starts: Dict(String, PendingStart),
    scheduled_retries: Dict(String, RetryStart),
    next_scheduled_retry_generation: Int,
    scheduled_report_retries: Dict(String, ReportRetryStart),
  )
}

pub type PendingStart {
  PendingStart(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
    requested_at_ms: Int,
    attempt: Int,
    blocking_reason: String,
  )
}

pub type RetryStart {
  RetryStart(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    next_attempt: Int,
    generation: Int,
  )
}

pub type ReportRetryStart {
  ReportRetryStart(job_id: String, run_id: String, generation: Int)
}

pub type FailureReportRequest {
  FailureReportRequest(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    reason: String,
    run_root: Option(String),
    session_id: Option(String),
  )
}

pub type WorkerFailureFollowUp {
  WorkerFailureReport(request: FailureReportRequest)
}

pub type ReportFailureDecision {
  ReportFailureRetry(
    runtime: Runtime,
    next_retry_at_ms: Int,
    report_attempt_index: Int,
    error_code: String,
    error_message: String,
    actions: List(Action),
  )
  ReportFailureTerminal(
    runtime: Runtime,
    report_attempt_index: Int,
    error_code: String,
    error_message: String,
  )
}

pub type Action {
  RecordScheduledDue(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
  )
  RecordScheduledPending(pending: PendingStart)
  RecordScheduledSkipped(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    skipped_count: Int,
  )
  RecordScheduledPendingBlocked(pending: PendingStart, blocked_at_ms: Int)
  UpdateNextDue(job_id: String, next_due_at_ms: Int)
  ScheduleRetryTimer(run_id: String, generation: Int, delay_ms: Int)
  ScheduleReportRetryTimer(run_id: String, generation: Int, delay_ms: Int)
  RecordScheduledRetry(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    next_attempt: Int,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
  PromoteRetryToPending(pending: PendingStart)
  RetryReport(job_id: String, run_id: String, report_attempt_index: Int)
}

pub fn default_max_backoff_ms() -> Int {
  retry_policy.default_max_backoff_ms()
}

pub fn default_report_max_attempts() -> Int {
  default_report_max_attempts_value
}

pub fn report_attempts_exhausted(report_attempt_index: Int) -> Bool {
  retry_policy.completed_attempts_exhausted(
    normalize_attempt_index(report_attempt_index),
    default_report_max_attempts(),
  )
}

pub fn initial_report_attempt_index() -> Int {
  retry_policy.first_attempt_index()
}

pub fn normalize_report_attempt_index(attempt: Int) -> Int {
  normalize_attempt_index(attempt)
}

pub fn new() -> Runtime {
  Runtime(
    next_due: dict.new(),
    pending_starts: dict.new(),
    scheduled_retries: dict.new(),
    next_scheduled_retry_generation: 1,
    scheduled_report_retries: dict.new(),
  )
}

pub fn from_next_due(entries: List(#(String, Int))) -> Runtime {
  Runtime(..new(), next_due: dict.from_list(entries))
}

pub fn due_count(runtime: Runtime, now_ms: Int) -> Int {
  runtime.next_due
  |> dict.values
  |> list.filter(fn(due_at_ms) { due_at_ms <= now_ms })
  |> list.length
}

pub fn next_due_count(runtime: Runtime) -> Int {
  dict.size(runtime.next_due)
}

pub fn pending_starts(runtime: Runtime) -> List(PendingStart) {
  dict.values(runtime.pending_starts)
}

pub fn retry_run_ids(runtime: Runtime) -> List(String) {
  runtime.scheduled_retries
  |> dict.values
  |> list.map(fn(entry) { entry.run_id })
}

pub fn report_retry_run_ids(runtime: Runtime) -> List(String) {
  runtime.scheduled_report_retries
  |> dict.values
  |> list.map(fn(entry) { entry.run_id })
}

pub fn retry_tick_matches(
  runtime: Runtime,
  run_id: String,
  generation: Int,
) -> Bool {
  case dict.get(runtime.scheduled_retries, run_id) {
    Ok(entry) -> entry.generation == generation
    Error(Nil) -> False
  }
}

pub fn report_retry_tick_matches(
  runtime: Runtime,
  run_id: String,
  generation: Int,
) -> Bool {
  case dict.get(runtime.scheduled_report_retries, run_id) {
    Ok(entry) -> entry.generation == generation
    Error(Nil) -> False
  }
}

pub fn ensure_next_due(
  runtime: Runtime,
  job_id: String,
  now_ms: Int,
  every_ms: Int,
) -> #(Runtime, Int) {
  case dict.get(runtime.next_due, job_id) {
    Ok(value) -> #(runtime, value)
    Error(Nil) -> {
      let next_due_at_ms = schedule_core.initial_next_due(now_ms, every_ms)
      #(
        Runtime(
          ..runtime,
          next_due: dict.insert(runtime.next_due, job_id, next_due_at_ms),
        ),
        next_due_at_ms,
      )
    }
  }
}

pub fn admit_due(
  runtime: Runtime,
  job_id: String,
  workflow_id: String,
  every_ms: Int,
  now_ms: Int,
  has_active_worker: Bool,
) -> #(Runtime, List(Action)) {
  let #(runtime, next_due_at_ms) =
    ensure_next_due(runtime, job_id, now_ms, every_ms)
  let schedule_state =
    schedule_core.ScheduleState(
      job_id: job_id,
      workflow_id: workflow_id,
      every_ms: every_ms,
      next_due_at_ms: next_due_at_ms,
      mode: schedule_mode(runtime, job_id, has_active_worker),
    )
  schedule_core.admit_due_boundaries(schedule_state, now_ms)
  |> apply_schedule_decisions(runtime, job_id, workflow_id)
}

pub fn insert_pending_start(
  runtime: Runtime,
  pending: PendingStart,
) -> Runtime {
  Runtime(
    ..runtime,
    pending_starts: dict.insert(runtime.pending_starts, pending.job_id, pending),
  )
}

pub fn remove_pending_start(runtime: Runtime, job_id: String) -> Runtime {
  Runtime(
    ..runtime,
    pending_starts: dict.delete(runtime.pending_starts, job_id),
  )
}

pub fn block_pending_start(
  runtime: Runtime,
  job_id: String,
  reason: String,
  blocked_at_ms: Int,
) -> #(Runtime, List(Action)) {
  case dict.get(runtime.pending_starts, job_id) {
    Error(Nil) -> #(runtime, [])
    Ok(pending) ->
      case pending.blocking_reason == reason {
        True -> #(runtime, [])
        False -> {
          let pending = PendingStart(..pending, blocking_reason: reason)
          #(insert_pending_start(runtime, pending), [
            RecordScheduledPendingBlocked(pending, blocked_at_ms),
          ])
        }
      }
  }
}

pub fn insert_retry(runtime: Runtime, retry: RetryStart) -> Runtime {
  let next_generation =
    retry_policy.next_generation_after_reserved(
      runtime.next_scheduled_retry_generation,
      retry.generation,
    )
  Runtime(
    ..runtime,
    scheduled_retries: dict.insert(
      runtime.scheduled_retries,
      retry.run_id,
      retry,
    ),
    next_scheduled_retry_generation: next_generation,
  )
}

pub fn schedule_retry(
  runtime: Runtime,
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  current_attempt: Int,
  next_attempt: Int,
  reason: String,
  max_backoff_ms: Int,
) -> #(Runtime, List(Action)) {
  let generation = runtime.next_scheduled_retry_generation
  let delay_ms = retry_policy.backoff_delay(current_attempt, max_backoff_ms)
  let runtime =
    insert_retry(
      runtime,
      RetryStart(
        job_id: job_id,
        workflow_id: workflow_id,
        due_at_ms: due_at_ms,
        run_id: run_id,
        next_attempt: next_attempt,
        generation: generation,
      ),
    )
  #(runtime, [
    RecordScheduledRetry(
      job_id: job_id,
      workflow_id: workflow_id,
      due_at_ms: due_at_ms,
      run_id: run_id,
      next_attempt: next_attempt,
      delay_ms: delay_ms,
      generation: generation,
      reason: reason,
    ),
    ScheduleRetryTimer(run_id, generation, delay_ms),
  ])
}

pub fn worker_failure_follow_up(
  runtime: Runtime,
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  reason: String,
  run_root: Option(String),
  session_id: Option(String),
) -> #(Runtime, WorkerFailureFollowUp) {
  #(
    runtime,
    WorkerFailureReport(FailureReportRequest(
      job_id: job_id,
      workflow_id: workflow_id,
      due_at_ms: due_at_ms,
      run_id: run_id,
      attempt: attempt,
      reason: reason,
      run_root: run_root,
      session_id: session_id,
    )),
  )
}

pub fn needs_human_follow_up(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  run_root: Option(String),
  session_id: Option(String),
) -> FailureReportRequest {
  FailureReportRequest(
    job_id: job_id,
    workflow_id: workflow_id,
    due_at_ms: due_at_ms,
    run_id: run_id,
    attempt: attempt,
    reason: "needs_human",
    run_root: run_root,
    session_id: session_id,
  )
}

pub fn handle_retry_tick(
  runtime: Runtime,
  run_id: String,
  generation: Int,
  now_ms: Int,
  slot_available: Bool,
  operator_paused: Bool,
) -> #(Runtime, List(Action)) {
  case dict.get(runtime.scheduled_retries, run_id) {
    Error(Nil) -> #(runtime, [])
    Ok(entry) ->
      case
        retry_policy.classify_timer_tick(Some(entry.generation), generation)
      {
        retry_policy.TimerMissing
        | retry_policy.TimerGenerationMismatch(_, _) -> #(runtime, [])
        retry_policy.TimerAccepted(_) ->
          case operator_paused || !slot_available {
            True -> #(runtime, [
              ScheduleRetryTimer(
                run_id,
                generation,
                retry_policy.defer_delay_ms(),
              ),
            ])
            False -> {
              let pending =
                PendingStart(
                  job_id: entry.job_id,
                  workflow_id: entry.workflow_id,
                  due_at_ms: entry.due_at_ms,
                  run_id: entry.run_id,
                  trigger: "automatic",
                  requested_at_ms: now_ms,
                  attempt: entry.next_attempt,
                  blocking_reason: "",
                )
              let runtime =
                Runtime(
                  ..runtime,
                  scheduled_retries: dict.delete(
                    runtime.scheduled_retries,
                    entry.run_id,
                  ),
                  pending_starts: dict.insert(
                    runtime.pending_starts,
                    entry.job_id,
                    pending,
                  ),
                )
              #(runtime, [PromoteRetryToPending(pending)])
            }
          }
      }
  }
}

pub fn insert_report_retry(
  runtime: Runtime,
  report_retry: ReportRetryStart,
) -> Runtime {
  Runtime(
    ..runtime,
    scheduled_report_retries: dict.insert(
      runtime.scheduled_report_retries,
      report_retry.run_id,
      report_retry,
    ),
  )
}

pub fn register_report_retry(
  runtime: Runtime,
  job_id: String,
  run_id: String,
) -> Runtime {
  insert_report_retry(
    runtime,
    ReportRetryStart(
      job_id: job_id,
      run_id: run_id,
      generation: retry_policy.first_attempt_index(),
    ),
  )
}

pub fn clear_report_retry(runtime: Runtime, run_id: String) -> Runtime {
  Runtime(
    ..runtime,
    scheduled_report_retries: dict.delete(
      runtime.scheduled_report_retries,
      run_id,
    ),
  )
}

pub fn schedule_report_retry_after_failure(
  runtime: Runtime,
  job_id: String,
  run_id: String,
  generation: Int,
  max_backoff_ms: Int,
) -> #(Runtime, Int, List(Action)) {
  let generation = normalize_attempt_index(generation)
  let delay_ms = retry_policy.backoff_delay(generation, max_backoff_ms)
  let runtime =
    insert_report_retry(
      runtime,
      ReportRetryStart(job_id: job_id, run_id: run_id, generation: generation),
    )
  #(runtime, delay_ms, [
    ScheduleReportRetryTimer(run_id, generation, delay_ms),
  ])
}

pub fn decide_report_failure(
  runtime: Runtime,
  job_id: String,
  run_id: String,
  generation: Int,
  err: adapter.TrackerError,
  now_ms: Int,
  max_backoff_ms: Int,
) -> ReportFailureDecision {
  let report_attempt_index = normalize_report_attempt_index(generation)
  let error_code = adapter_tracker_error_code(err)
  let error_message = adapter_tracker_error_message(err)
  case
    adapter_tracker_error_retryable(err),
    report_attempts_exhausted(report_attempt_index)
  {
    True, False -> {
      let #(runtime, delay_ms, actions) =
        schedule_report_retry_after_failure(
          runtime,
          job_id,
          run_id,
          report_attempt_index,
          max_backoff_ms,
        )
      ReportFailureRetry(
        runtime: runtime,
        next_retry_at_ms: now_ms + delay_ms,
        report_attempt_index: report_attempt_index,
        error_code: error_code,
        error_message: error_message,
        actions: actions,
      )
    }
    _, _ ->
      ReportFailureTerminal(
        runtime: clear_report_retry(runtime, run_id),
        report_attempt_index: report_attempt_index,
        error_code: error_code,
        error_message: error_message,
      )
  }
}

pub fn report_failure_decision_runtime(
  decision: ReportFailureDecision,
) -> Runtime {
  case decision {
    ReportFailureRetry(runtime:, ..) -> runtime
    ReportFailureTerminal(runtime:, ..) -> runtime
  }
}

pub fn report_failure_failed_record(
  publication: adapter.ScheduledFailurePublication,
  decision: ReportFailureDecision,
) -> record.RecordBody {
  let #(next_retry_at_ms, report_attempt_index, error_code, error_message) = case
    decision
  {
    ReportFailureRetry(
      next_retry_at_ms:,
      report_attempt_index:,
      error_code:,
      error_message:,
      ..,
    ) -> #(next_retry_at_ms, report_attempt_index, error_code, error_message)
    ReportFailureTerminal(
      report_attempt_index:,
      error_code:,
      error_message:,
      ..,
    ) -> #(0, report_attempt_index, error_code, error_message)
  }
  record.ScheduledFailureReportFailed(
    publication.job_id,
    publication.workflow_id,
    publication.due_at_ms,
    publication.run_id,
    publication.attempt,
    publication.dedupe_key,
    error_code,
    error_message,
    next_retry_at_ms,
    report_attempt_index,
  )
}

pub fn report_failure_log_fields(
  publication: adapter.ScheduledFailurePublication,
  decision: ReportFailureDecision,
) -> List(#(String, String)) {
  let #(report_attempt_index, error_code, retrying) = case decision {
    ReportFailureRetry(report_attempt_index:, error_code:, ..) -> #(
      report_attempt_index,
      error_code,
      "true",
    )
    ReportFailureTerminal(report_attempt_index:, error_code:, ..) -> #(
      report_attempt_index,
      error_code,
      "false",
    )
  }
  [
    #("job_id", publication.job_id),
    #("run_id", publication.run_id),
    #("error", error_code),
    #("report_attempt", int.to_string(report_attempt_index)),
    #("retrying", retrying),
  ]
}

pub fn insert_timer_cancelling_existing(
  timers: Dict(String, timer),
  key: String,
  timer: timer,
  cancel_timer: fn(timer) -> Nil,
) -> Dict(String, timer) {
  case dict.get(timers, key) {
    Ok(existing) -> cancel_timer(existing)
    Error(Nil) -> Nil
  }
  dict.insert(timers, key, timer)
}

pub fn delete_timer_cancelling_existing(
  timers: Dict(String, timer),
  key: String,
  cancel_timer: fn(timer) -> Nil,
) -> Dict(String, timer) {
  case dict.get(timers, key) {
    Ok(existing) -> cancel_timer(existing)
    Error(Nil) -> Nil
  }
  dict.delete(timers, key)
}

pub fn handle_report_retry_tick(
  runtime: Runtime,
  run_id: String,
  generation: Int,
) -> #(Runtime, List(Action)) {
  case dict.get(runtime.scheduled_report_retries, run_id) {
    Error(Nil) -> #(runtime, [])
    Ok(entry) ->
      case
        retry_policy.classify_timer_tick(Some(entry.generation), generation)
      {
        retry_policy.TimerMissing
        | retry_policy.TimerGenerationMismatch(_, _) -> #(runtime, [])
        retry_policy.TimerAccepted(_) -> {
          let runtime = clear_report_retry(runtime, run_id)
          #(runtime, [
            RetryReport(
              entry.job_id,
              entry.run_id,
              retry_policy.next_attempt_index(normalize_attempt_index(
                entry.generation,
              )),
            ),
          ])
        }
      }
  }
}

pub fn schedule_mode(
  runtime: Runtime,
  job_id: String,
  has_active_worker: Bool,
) -> schedule_core.ScheduleMode {
  case dict.get(runtime.pending_starts, job_id) {
    Ok(pending) ->
      schedule_core.Pending(normalize_blocking_reason(pending.blocking_reason))
    Error(Nil) ->
      case has_active_worker {
        True -> schedule_core.Active
        False ->
          case retry_waiting_for_job(runtime, job_id) {
            True -> schedule_core.RetryWaiting
            False -> schedule_core.Idle
          }
      }
  }
}

fn apply_schedule_decisions(
  decisions: List(schedule_core.ScheduleDecision),
  runtime: Runtime,
  job_id: String,
  workflow_id: String,
) -> #(Runtime, List(Action)) {
  list.fold(decisions, #(runtime, []), fn(state, decision) {
    let #(runtime, actions) = state
    let #(next_runtime, next_action) =
      apply_schedule_decision(decision, runtime, job_id, workflow_id)
    #(next_runtime, list.append(actions, [next_action]))
  })
}

fn apply_schedule_decision(
  decision: schedule_core.ScheduleDecision,
  runtime: Runtime,
  job_id: String,
  workflow_id: String,
) -> #(Runtime, Action) {
  case decision {
    schedule_core.ScheduledDue(due_at_ms, run_id, trigger) -> #(
      runtime,
      RecordScheduledDue(
        job_id: job_id,
        workflow_id: workflow_id,
        due_at_ms: due_at_ms,
        run_id: run_id,
        trigger: trigger,
      ),
    )
    schedule_core.ScheduledPending(due_at_ms, run_id, trigger, requested_at_ms) -> {
      let pending =
        PendingStart(
          job_id: job_id,
          workflow_id: workflow_id,
          due_at_ms: due_at_ms,
          run_id: run_id,
          trigger: trigger,
          requested_at_ms: requested_at_ms,
          attempt: 1,
          blocking_reason: "",
        )
      #(insert_pending_start(runtime, pending), RecordScheduledPending(pending))
    }
    schedule_core.ScheduledSkipped(due_at_ms, run_id, reason, skipped_count) -> #(
      runtime,
      RecordScheduledSkipped(
        job_id: job_id,
        workflow_id: workflow_id,
        due_at_ms: due_at_ms,
        run_id: run_id,
        reason: reason,
        skipped_count: skipped_count,
      ),
    )
    schedule_core.ScheduledNextDue(next_due_at_ms) -> #(
      Runtime(
        ..runtime,
        next_due: dict.insert(runtime.next_due, job_id, next_due_at_ms),
      ),
      UpdateNextDue(job_id, next_due_at_ms),
    )
  }
}

fn normalize_blocking_reason(reason: String) -> String {
  case reason {
    "paused" -> "schedule_paused"
    _ -> reason
  }
}

fn normalize_attempt_index(attempt_index: Int) -> Int {
  case attempt_index <= 0 {
    True -> retry_policy.first_attempt_index()
    False -> attempt_index
  }
}

fn adapter_tracker_error_retryable(err: adapter.TrackerError) -> Bool {
  case err {
    adapter.Transient(_) -> True
    adapter.Unauthorized(_)
    | adapter.NotFound(_)
    | adapter.Permanent(_)
    | adapter.UnsupportedCapability(_)
    | adapter.DecodeFailed(_) -> False
  }
}

fn adapter_tracker_error_code(err: adapter.TrackerError) -> String {
  case err {
    adapter.Unauthorized(_) -> "tracker_unauthorized"
    adapter.NotFound(_) -> "tracker_not_found"
    adapter.Transient(_) -> "tracker_transient"
    adapter.Permanent(_) -> "tracker_permanent"
    adapter.UnsupportedCapability(_) -> "tracker_unsupported_capability"
    adapter.DecodeFailed(_) -> "tracker_decode_failed"
  }
}

fn adapter_tracker_error_message(err: adapter.TrackerError) -> String {
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

fn retry_waiting_for_job(runtime: Runtime, job_id: String) -> Bool {
  list.any(dict.values(runtime.scheduled_retries), fn(entry) {
    entry.job_id == job_id
  })
  || list.any(dict.values(runtime.scheduled_report_retries), fn(entry) {
    entry.job_id == job_id
  })
}
