import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/state/record

pub const context_name = "scheduled"

const max_recent_scheduled_run_ids = 25

pub type ScheduledRunState {
  ScheduledIdle
  ScheduledDuePending
  ScheduledPaused
  ScheduledWaitingForGlobalSlot
  ScheduledActive
  ScheduledRetryWaiting
  ScheduledReportRetryWaiting
  ScheduledTerminalSuccess
  ScheduledTerminalFailure
}

pub type ScheduledRunSummary {
  ScheduledRunSummary(
    run_id: String,
    due_at_ms: Int,
    trigger: String,
    attempt: Int,
    status: String,
    reason: Option(String),
    session_id: Option(String),
    run_root: Option(String),
  )
}

pub type ScheduledReportRetry {
  ScheduledReportRetry(
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    error_code: String,
    error_message: String,
    next_retry_at_ms: Int,
    generation: Int,
  )
}

pub type ScheduledJobStatus {
  ScheduledJobStatus(
    job_id: String,
    workflow_id: String,
    state: ScheduledRunState,
    current_run: Option(ScheduledRunSummary),
    last_due_at_ms: Option(Int),
    last_success_at_ms: Option(Int),
    last_success_run_id: Option(String),
    last_failure_at_ms: Option(Int),
    last_failure_run_id: Option(String),
    last_failure_reason: Option(String),
    retry_count: Int,
    skipped_overlap_count: Int,
    skipped_catch_up_count: Int,
    skipped_paused_count: Int,
    skipped_capacity_count: Int,
    failure_issue_id: Option(String),
    failure_dedupe_key: Option(String),
    report_retry: Option(ScheduledReportRetry),
    recent_run_ids: List(String),
  )
}

type ScheduledJobSnapshot {
  ScheduledJobSnapshot(job_id: String, status: ScheduledJobStatus)
}

pub fn status_for(
  statuses: Dict(String, ScheduledJobStatus),
  job_id: String,
) -> Result(ScheduledJobStatus, Nil) {
  dict.get(statuses, job_id)
}

pub fn statuses(
  statuses: Dict(String, ScheduledJobStatus),
) -> List(ScheduledJobStatus) {
  dict.values(statuses)
}

pub fn apply_record(
  statuses: Dict(String, ScheduledJobStatus),
  ledger_record: record.LedgerRecord,
) -> Result(ScheduledJobStatus, Nil) {
  case ledger_record.body {
    record.ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger) ->
      ensure_status(statuses, job_id, workflow_id)
      |> due_status(due_at_ms, run_id, trigger)
      |> Ok

    record.ScheduledJobSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> skipped_status(due_at_ms, run_id, reason, skipped_count)
      |> Ok

    record.ScheduledRunPending(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      trigger,
      _,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> pending_status(due_at_ms, run_id, trigger)
      |> Ok

    record.ScheduledRunPendingBlocked(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      _,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> blocked_status(due_at_ms, run_id, reason)
      |> Ok

    record.ScheduledRunPendingCancelled(
      job_id,
      workflow_id,
      _,
      run_id,
      reason,
      _,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> cancelled_status(run_id, reason)
      |> Ok

    record.ScheduledRunStarted(
      job_id,
      workflow_id,
      due_at_ms,
      _,
      run_id,
      attempt,
      session_id,
      run_root,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> started_status(due_at_ms, run_id, attempt, session_id, run_root)
      |> Ok

    record.ScheduledRunSucceeded(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      finished_at_ms,
      _,
      _,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> succeeded_status(due_at_ms, run_id, attempt, finished_at_ms)
      |> Ok

    record.ScheduledRunFailed(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      finished_at_ms,
      reason,
      retry_exhausted,
      run_root,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> failed_status(
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        reason,
        retry_exhausted,
        run_root,
      )
      |> Ok

    record.ScheduledRunRetryScheduled(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      next_attempt,
      _,
      _,
      reason,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> retry_status(due_at_ms, run_id, next_attempt, reason)
      |> Ok

    record.ScheduledRunRetryCancelled(job_id, run_id, _, reason) ->
      ensure_status(statuses, job_id, "")
      |> cancelled_status(run_id, reason)
      |> Ok

    record.ScheduledFailureReported(
      job_id,
      workflow_id,
      _,
      _,
      _,
      dedupe_key,
      linear_issue_id,
      _,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> reported_status(dedupe_key, linear_issue_id)
      |> Ok

    record.ScheduledFailureReportFailed(
      job_id,
      workflow_id,
      _,
      run_id,
      attempt,
      dedupe_key,
      error_code,
      error_message,
      next_retry_at_ms,
      generation,
    ) ->
      ensure_status(statuses, job_id, workflow_id)
      |> report_failed_status(
        run_id,
        attempt,
        dedupe_key,
        error_code,
        error_message,
        next_retry_at_ms,
        generation,
      )
      |> Ok

    _ -> Error(Nil)
  }
}

pub fn ensure_status(
  statuses: Dict(String, ScheduledJobStatus),
  job_id: String,
  workflow_id: String,
) -> ScheduledJobStatus {
  case dict.get(statuses, job_id) {
    Ok(status) ->
      case status.workflow_id == "" && workflow_id != "" {
        True -> ScheduledJobStatus(..status, workflow_id: workflow_id)
        False -> status
      }
    Error(Nil) -> empty_status(job_id, workflow_id)
  }
}

pub fn empty_status(job_id: String, workflow_id: String) -> ScheduledJobStatus {
  ScheduledJobStatus(
    job_id: job_id,
    workflow_id: workflow_id,
    state: ScheduledIdle,
    current_run: None,
    last_due_at_ms: None,
    last_success_at_ms: None,
    last_success_run_id: None,
    last_failure_at_ms: None,
    last_failure_run_id: None,
    last_failure_reason: None,
    retry_count: 0,
    skipped_overlap_count: 0,
    skipped_catch_up_count: 0,
    skipped_paused_count: 0,
    skipped_capacity_count: 0,
    failure_issue_id: None,
    failure_dedupe_key: None,
    report_retry: None,
    recent_run_ids: [],
  )
}

pub fn due_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledDuePending,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: trigger,
      attempt: 0,
      status: "due",
      reason: None,
      session_id: None,
      run_root: None,
    )),
    last_due_at_ms: Some(due_at_ms),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn pending_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledDuePending,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: trigger,
      attempt: 0,
      status: "pending",
      reason: None,
      session_id: None,
      run_root: None,
    )),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn blocked_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  reason: String,
) -> ScheduledJobStatus {
  let next_state = case reason {
    "paused" -> ScheduledPaused
    "waiting_for_global_slot" -> ScheduledWaitingForGlobalSlot
    _ -> ScheduledDuePending
  }
  ScheduledJobStatus(
    ..status,
    state: next_state,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: current_attempt(status),
      status: "blocked",
      reason: Some(reason),
      session_id: None,
      run_root: current_run_root(status),
    )),
  )
}

pub fn cancelled_status(
  status: ScheduledJobStatus,
  run_id: String,
  reason: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledIdle,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: current_due_at(status),
      trigger: current_trigger(status),
      attempt: current_attempt(status),
      status: "cancelled",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
  )
}

pub fn started_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  session_id: String,
  run_root: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledActive,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "active",
      reason: None,
      session_id: Some(session_id),
      run_root: Some(run_root),
    )),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn succeeded_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledTerminalSuccess,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "succeeded",
      reason: None,
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
    last_success_at_ms: Some(finished_at_ms),
    last_success_run_id: Some(run_id),
    report_retry: None,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn failed_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
  reason: String,
  retry_exhausted: Bool,
  run_root: Option(String),
) -> ScheduledJobStatus {
  let next_state = case retry_exhausted {
    True -> ScheduledTerminalFailure
    False -> ScheduledRetryWaiting
  }
  ScheduledJobStatus(
    ..status,
    state: next_state,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "failed",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: first_some_string(run_root, current_run_root(status)),
    )),
    last_failure_at_ms: Some(finished_at_ms),
    last_failure_run_id: Some(run_id),
    last_failure_reason: Some(reason),
    retry_count: attempt,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn retry_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  next_attempt: Int,
  reason: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledRetryWaiting,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: next_attempt,
      status: "retry_waiting",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
    retry_count: next_attempt - 1,
  )
}

pub fn reported_status(
  status: ScheduledJobStatus,
  dedupe_key: String,
  linear_issue_id: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledTerminalFailure,
    failure_issue_id: Some(linear_issue_id),
    failure_dedupe_key: Some(dedupe_key),
    report_retry: None,
  )
}

pub fn report_failed_status(
  status: ScheduledJobStatus,
  run_id: String,
  attempt: Int,
  dedupe_key: String,
  error_code: String,
  error_message: String,
  next_retry_at_ms: Int,
  generation: Int,
) -> ScheduledJobStatus {
  case next_retry_at_ms <= 0 {
    True ->
      ScheduledJobStatus(
        ..status,
        state: ScheduledTerminalFailure,
        failure_dedupe_key: Some(dedupe_key),
        report_retry: None,
      )
    False ->
      ScheduledJobStatus(
        ..status,
        state: ScheduledReportRetryWaiting,
        failure_dedupe_key: Some(dedupe_key),
        report_retry: Some(ScheduledReportRetry(
          run_id: run_id,
          attempt: attempt,
          dedupe_key: dedupe_key,
          error_code: error_code,
          error_message: error_message,
          next_retry_at_ms: next_retry_at_ms,
          generation: generation,
        )),
      )
  }
}

pub fn skipped_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  reason: String,
  skipped_count: Int,
) -> ScheduledJobStatus {
  let #(overlap, catch_up, paused, capacity) = case reason {
    "overlap_running" -> #(skipped_count, 0, 0, 0)
    "catch_up_disabled" -> #(0, skipped_count, 0, 0)
    "schedule_paused" -> #(0, 0, skipped_count, 0)
    "waiting_for_global_slot" -> #(0, 0, 0, skipped_count)
    _ -> #(0, 0, 0, 0)
  }
  ScheduledJobStatus(
    ..status,
    last_due_at_ms: Some(due_at_ms),
    skipped_overlap_count: status.skipped_overlap_count + overlap,
    skipped_catch_up_count: status.skipped_catch_up_count + catch_up,
    skipped_paused_count: status.skipped_paused_count + paused,
    skipped_capacity_count: status.skipped_capacity_count + capacity,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

pub fn entry_to_json(entry: #(String, ScheduledJobStatus)) -> json.Json {
  let #(job_id, status) = entry
  json.object([
    #("job_id", json.string(job_id)),
    #("workflow_id", json.string(status.workflow_id)),
    #("state", json.string(state_to_string(status.state))),
    #("current_run", option_run_to_json(status.current_run)),
    #("last_due_at_ms", option_int_to_json(status.last_due_at_ms)),
    #("last_success_at_ms", option_int_to_json(status.last_success_at_ms)),
    #("last_success_run_id", option_string_to_json(status.last_success_run_id)),
    #("last_failure_at_ms", option_int_to_json(status.last_failure_at_ms)),
    #("last_failure_run_id", option_string_to_json(status.last_failure_run_id)),
    #("last_failure_reason", option_string_to_json(status.last_failure_reason)),
    #("retry_count", json.int(status.retry_count)),
    #("skipped_overlap_count", json.int(status.skipped_overlap_count)),
    #("skipped_catch_up_count", json.int(status.skipped_catch_up_count)),
    #("skipped_paused_count", json.int(status.skipped_paused_count)),
    #("skipped_capacity_count", json.int(status.skipped_capacity_count)),
    #("failure_issue_id", option_string_to_json(status.failure_issue_id)),
    #("failure_dedupe_key", option_string_to_json(status.failure_dedupe_key)),
    #("report_retry", option_report_retry_to_json(status.report_retry)),
    #("recent_run_ids", json.array(status.recent_run_ids, of: json.string)),
  ])
}

pub fn snapshot_decoder() -> decode.Decoder(#(String, ScheduledJobStatus)) {
  scheduled_job_snapshot_decoder()
  |> decode.map(fn(snapshot) {
    let ScheduledJobSnapshot(job_id, status) = snapshot
    #(job_id, status)
  })
}

fn current_trigger(status: ScheduledJobStatus) -> String {
  case status.current_run {
    Some(run) -> run.trigger
    None -> "automatic"
  }
}

fn current_due_at(status: ScheduledJobStatus) -> Int {
  case status.current_run {
    Some(run) -> run.due_at_ms
    None -> 0
  }
}

fn current_attempt(status: ScheduledJobStatus) -> Int {
  case status.current_run {
    Some(run) -> run.attempt
    None -> 0
  }
}

fn current_session_id(status: ScheduledJobStatus) -> Option(String) {
  case status.current_run {
    Some(run) -> run.session_id
    None -> None
  }
}

fn current_run_root(status: ScheduledJobStatus) -> Option(String) {
  case status.current_run {
    Some(run) -> run.run_root
    None -> None
  }
}

fn first_some_string(
  preferred: Option(String),
  fallback: Option(String),
) -> Option(String) {
  case preferred {
    Some(value) -> Some(value)
    None -> fallback
  }
}

fn insert_recent_run(ids: List(String), run_id: String) -> List(String) {
  case list.contains(ids, run_id) {
    True -> trim_recent_runs(ids)
    False -> trim_recent_runs([run_id, ..ids])
  }
}

fn trim_recent_runs(ids: List(String)) -> List(String) {
  list.take(ids, max_recent_scheduled_run_ids)
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(inner) -> json.int(inner)
    None -> json.null()
  }
}

fn option_run_to_json(value: Option(ScheduledRunSummary)) -> json.Json {
  case value {
    None -> json.null()
    Some(run) ->
      json.object([
        #("run_id", json.string(run.run_id)),
        #("due_at_ms", json.int(run.due_at_ms)),
        #("trigger", json.string(run.trigger)),
        #("attempt", json.int(run.attempt)),
        #("status", json.string(run.status)),
        #("reason", option_string_to_json(run.reason)),
        #("session_id", option_string_to_json(run.session_id)),
        #("run_root", option_string_to_json(run.run_root)),
      ])
  }
}

fn option_report_retry_to_json(
  value: Option(ScheduledReportRetry),
) -> json.Json {
  case value {
    None -> json.null()
    Some(retry) ->
      json.object([
        #("run_id", json.string(retry.run_id)),
        #("attempt", json.int(retry.attempt)),
        #("dedupe_key", json.string(retry.dedupe_key)),
        #("error_code", json.string(retry.error_code)),
        #("error_message", json.string(retry.error_message)),
        #("next_retry_at_ms", json.int(retry.next_retry_at_ms)),
        #("generation", json.int(retry.generation)),
      ])
  }
}

fn state_to_string(state: ScheduledRunState) -> String {
  case state {
    ScheduledIdle -> "idle"
    ScheduledDuePending -> "due_pending"
    ScheduledPaused -> "paused"
    ScheduledWaitingForGlobalSlot -> "waiting_for_global_slot"
    ScheduledActive -> "active"
    ScheduledRetryWaiting -> "retry_waiting"
    ScheduledReportRetryWaiting -> "report_retry_waiting"
    ScheduledTerminalSuccess -> "terminal_success"
    ScheduledTerminalFailure -> "terminal_failure"
  }
}

fn state_from_string(value: String) -> ScheduledRunState {
  case value {
    "due_pending" -> ScheduledDuePending
    "paused" -> ScheduledPaused
    "waiting_for_global_slot" -> ScheduledWaitingForGlobalSlot
    "active" -> ScheduledActive
    "retry_waiting" -> ScheduledRetryWaiting
    "report_retry_waiting" -> ScheduledReportRetryWaiting
    "terminal_success" -> ScheduledTerminalSuccess
    "terminal_failure" -> ScheduledTerminalFailure
    _ -> ScheduledIdle
  }
}

fn scheduled_run_summary_decoder() -> decode.Decoder(ScheduledRunSummary) {
  use run_id <- decode.field("run_id", decode.string)
  use due_at_ms <- decode.field("due_at_ms", decode.int)
  use trigger <- decode.field("trigger", decode.string)
  use attempt <- decode.field("attempt", decode.int)
  use status <- decode.field("status", decode.string)
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use run_root <- decode.optional_field(
    "run_root",
    None,
    decode.optional(decode.string),
  )
  decode.success(ScheduledRunSummary(
    run_id: run_id,
    due_at_ms: due_at_ms,
    trigger: trigger,
    attempt: attempt,
    status: status,
    reason: reason,
    session_id: session_id,
    run_root: run_root,
  ))
}

fn scheduled_report_retry_decoder() -> decode.Decoder(ScheduledReportRetry) {
  use run_id <- decode.field("run_id", decode.string)
  use attempt <- decode.field("attempt", decode.int)
  use dedupe_key <- decode.field("dedupe_key", decode.string)
  use error_code <- decode.field("error_code", decode.string)
  use error_message <- decode.field("error_message", decode.string)
  use next_retry_at_ms <- decode.field("next_retry_at_ms", decode.int)
  use generation <- decode.field("generation", decode.int)
  decode.success(ScheduledReportRetry(
    run_id: run_id,
    attempt: attempt,
    dedupe_key: dedupe_key,
    error_code: error_code,
    error_message: error_message,
    next_retry_at_ms: next_retry_at_ms,
    generation: generation,
  ))
}

fn scheduled_job_snapshot_decoder() -> decode.Decoder(ScheduledJobSnapshot) {
  use job_id <- decode.field("job_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use state <- decode.optional_field("state", "idle", decode.string)
  use current_run <- decode.optional_field(
    "current_run",
    None,
    decode.optional(scheduled_run_summary_decoder()),
  )
  use last_due_at_ms <- decode.optional_field(
    "last_due_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_success_at_ms <- decode.optional_field(
    "last_success_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_success_run_id <- decode.optional_field(
    "last_success_run_id",
    None,
    decode.optional(decode.string),
  )
  use last_failure_at_ms <- decode.optional_field(
    "last_failure_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_failure_run_id <- decode.optional_field(
    "last_failure_run_id",
    None,
    decode.optional(decode.string),
  )
  use last_failure_reason <- decode.optional_field(
    "last_failure_reason",
    None,
    decode.optional(decode.string),
  )
  use retry_count <- decode.optional_field("retry_count", 0, decode.int)
  use skipped_overlap_count <- decode.optional_field(
    "skipped_overlap_count",
    0,
    decode.int,
  )
  use skipped_catch_up_count <- decode.optional_field(
    "skipped_catch_up_count",
    0,
    decode.int,
  )
  use skipped_paused_count <- decode.optional_field(
    "skipped_paused_count",
    0,
    decode.int,
  )
  use skipped_capacity_count <- decode.optional_field(
    "skipped_capacity_count",
    0,
    decode.int,
  )
  use failure_issue_id <- decode.optional_field(
    "failure_issue_id",
    None,
    decode.optional(decode.string),
  )
  use failure_dedupe_key <- decode.optional_field(
    "failure_dedupe_key",
    None,
    decode.optional(decode.string),
  )
  use report_retry <- decode.optional_field(
    "report_retry",
    None,
    decode.optional(scheduled_report_retry_decoder()),
  )
  use recent_run_ids <- decode.optional_field(
    "recent_run_ids",
    [],
    decode.list(of: decode.string),
  )
  decode.success(ScheduledJobSnapshot(
    job_id,
    ScheduledJobStatus(
      job_id: job_id,
      workflow_id: workflow_id,
      state: state_from_string(state),
      current_run: current_run,
      last_due_at_ms: last_due_at_ms,
      last_success_at_ms: last_success_at_ms,
      last_success_run_id: last_success_run_id,
      last_failure_at_ms: last_failure_at_ms,
      last_failure_run_id: last_failure_run_id,
      last_failure_reason: last_failure_reason,
      retry_count: retry_count,
      skipped_overlap_count: skipped_overlap_count,
      skipped_catch_up_count: skipped_catch_up_count,
      skipped_paused_count: skipped_paused_count,
      skipped_capacity_count: skipped_capacity_count,
      failure_issue_id: failure_issue_id,
      failure_dedupe_key: failure_dedupe_key,
      report_retry: report_retry,
      recent_run_ids: trim_recent_runs(recent_run_ids),
    ),
  ))
}
