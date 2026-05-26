import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub const context_name = "scheduled"

pub type ScheduledFields {
  ScheduledFields(
    job_id: Option(String),
    workflow_id: Option(String),
    due_at_ms: Option(Int),
    run_id: Option(String),
    trigger: Option(String),
    reason: Option(String),
    skipped_count: Option(Int),
    requested_at_ms: Option(Int),
    observed_at_ms: Option(Int),
    cancelled_at_ms: Option(Int),
    started_at_ms: Option(Int),
    finished_at_ms: Option(Int),
    attempt: Option(Int),
    session_id: Option(String),
    retry_exhausted: Option(Bool),
    next_attempt: Option(Int),
    generation: Option(Int),
    delay_ms: Option(Int),
    dedupe_key: Option(String),
    linear_issue_id: Option(String),
    action: Option(String),
    error_code: Option(String),
    error_message: Option(String),
    next_retry_at_ms: Option(Int),
    run_root: Option(String),
    token_total: Option(Int),
    turns: Option(Int),
  )
}

pub type DecodedBody {
  ScheduledJobDueBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
  )
  ScheduledJobSkippedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    skipped_count: Int,
  )
  ScheduledRunPendingBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
    requested_at_ms: Int,
  )
  ScheduledRunPendingBlockedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    observed_at_ms: Int,
  )
  ScheduledRunPendingCancelledBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    cancelled_at_ms: Int,
  )
  ScheduledRunStartedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    started_at_ms: Int,
    run_id: String,
    attempt: Int,
    session_id: String,
    run_root: String,
  )
  ScheduledRunSucceededBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    finished_at_ms: Int,
    token_total: Int,
    turns: Int,
  )
  ScheduledRunFailedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    finished_at_ms: Int,
    reason: String,
    retry_exhausted: Bool,
    run_root: Option(String),
  )
  ScheduledRunRetryScheduledBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    next_attempt: Int,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
  ScheduledRunRetryCancelledBody(
    job_id: String,
    run_id: String,
    generation: Int,
    reason: String,
  )
  ScheduledFailureReportedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    linear_issue_id: String,
    action: String,
  )
  ScheduledFailureReportFailedBody(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    error_code: String,
    error_message: String,
    next_retry_at_ms: Int,
    generation: Int,
  )
}

pub fn base_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
) -> List(#(String, json.Json)) {
  [
    #("job_id", json.string(job_id)),
    #("workflow_id", json.string(workflow_id)),
    #("due_at_ms", json.int(due_at_ms)),
    #("run_id", json.string(run_id)),
  ]
}

pub fn append_entries(
  base: List(#(String, json.Json)),
  extra: List(#(String, json.Json)),
) -> List(#(String, json.Json)) {
  list.append(base, extra)
}

pub fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}

pub fn job_due_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([#("trigger", json.string(trigger))])
}

pub fn job_skipped_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  reason: String,
  skipped_count: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("reason", json.string(reason)),
    #("skipped_count", json.int(skipped_count)),
  ])
}

pub fn run_pending_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
  requested_at_ms: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("trigger", json.string(trigger)),
    #("requested_at_ms", json.int(requested_at_ms)),
  ])
}

pub fn run_pending_blocked_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  reason: String,
  observed_at_ms: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("reason", json.string(reason)),
    #("observed_at_ms", json.int(observed_at_ms)),
  ])
}

pub fn run_pending_cancelled_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  reason: String,
  cancelled_at_ms: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("reason", json.string(reason)),
    #("cancelled_at_ms", json.int(cancelled_at_ms)),
  ])
}

pub fn run_started_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  started_at_ms: Int,
  run_id: String,
  attempt: Int,
  session_id: String,
  run_root: String,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("started_at_ms", json.int(started_at_ms)),
    #("attempt", json.int(attempt)),
    #("session_id", json.string(session_id)),
    #("run_root", json.string(run_root)),
  ])
}

pub fn run_succeeded_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
  token_total: Int,
  turns: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("attempt", json.int(attempt)),
    #("finished_at_ms", json.int(finished_at_ms)),
    #("token_total", json.int(token_total)),
    #("turns", json.int(turns)),
  ])
}

pub fn run_failed_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
  reason: String,
  retry_exhausted: Bool,
  run_root: Option(String),
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("attempt", json.int(attempt)),
    #("finished_at_ms", json.int(finished_at_ms)),
    #("reason", json.string(reason)),
    #("retry_exhausted", json.bool(retry_exhausted)),
    #("run_root", option_string_to_json(run_root)),
  ])
}

pub fn run_retry_scheduled_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  next_attempt: Int,
  delay_ms: Int,
  generation: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("next_attempt", json.int(next_attempt)),
    #("delay_ms", json.int(delay_ms)),
    #("generation", json.int(generation)),
    #("reason", json.string(reason)),
  ])
}

pub fn run_retry_cancelled_entries(
  job_id: String,
  run_id: String,
  generation: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("job_id", json.string(job_id)),
    #("run_id", json.string(run_id)),
    #("generation", json.int(generation)),
    #("reason", json.string(reason)),
  ]
}

pub fn failure_reported_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  dedupe_key: String,
  linear_issue_id: String,
  action: String,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("attempt", json.int(attempt)),
    #("dedupe_key", json.string(dedupe_key)),
    #("linear_issue_id", json.string(linear_issue_id)),
    #("action", json.string(action)),
  ])
}

pub fn failure_report_failed_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  dedupe_key: String,
  error_code: String,
  error_message: String,
  next_retry_at_ms: Int,
  generation: Int,
) -> List(#(String, json.Json)) {
  base_entries(job_id, workflow_id, due_at_ms, run_id)
  |> append_entries([
    #("attempt", json.int(attempt)),
    #("dedupe_key", json.string(dedupe_key)),
    #("error_code", json.string(error_code)),
    #("error_message", json.string(error_message)),
    #("next_retry_at_ms", json.int(next_retry_at_ms)),
    #("generation", json.int(generation)),
  ])
}

pub fn decode(
  kind: String,
  fields: ScheduledFields,
  required_string: fn(Option(String), String) -> Result(String, String),
  required_int: fn(Option(Int), String) -> Result(Int, String),
  required_bool: fn(Option(Bool), String) -> Result(Bool, String),
) -> Result(DecodedBody, String) {
  case kind {
    "scheduled_job_due" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use trigger <- result.try(required_string(fields.trigger, "trigger"))
      Ok(ScheduledJobDueBody(job_id, workflow_id, due_at_ms, run_id, trigger))
    }
    "scheduled_job_skipped" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use skipped_count <- result.try(required_int(
        fields.skipped_count,
        "skipped_count",
      ))
      Ok(ScheduledJobSkippedBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        skipped_count,
      ))
    }
    "scheduled_run_pending" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use trigger <- result.try(required_string(fields.trigger, "trigger"))
      use requested_at_ms <- result.try(required_int(
        fields.requested_at_ms,
        "requested_at_ms",
      ))
      Ok(ScheduledRunPendingBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        trigger,
        requested_at_ms,
      ))
    }
    "scheduled_run_pending_blocked" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use observed_at_ms <- result.try(required_int(
        fields.observed_at_ms,
        "observed_at_ms",
      ))
      Ok(ScheduledRunPendingBlockedBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        observed_at_ms,
      ))
    }
    "scheduled_run_pending_cancelled" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use cancelled_at_ms <- result.try(required_int(
        fields.cancelled_at_ms,
        "cancelled_at_ms",
      ))
      Ok(ScheduledRunPendingCancelledBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        cancelled_at_ms,
      ))
    }
    "scheduled_run_started" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use started_at_ms <- result.try(required_int(
        fields.started_at_ms,
        "started_at_ms",
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use session_id <- result.try(required_string(
        fields.session_id,
        "session_id",
      ))
      use run_root <- result.try(required_string(fields.run_root, "run_root"))
      Ok(ScheduledRunStartedBody(
        job_id,
        workflow_id,
        due_at_ms,
        started_at_ms,
        run_id,
        attempt,
        session_id,
        run_root,
      ))
    }
    "scheduled_run_succeeded" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use finished_at_ms <- result.try(required_int(
        fields.finished_at_ms,
        "finished_at_ms",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      Ok(ScheduledRunSucceededBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        token_total,
        turns,
      ))
    }
    "scheduled_run_failed" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use finished_at_ms <- result.try(required_int(
        fields.finished_at_ms,
        "finished_at_ms",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use retry_exhausted <- result.try(required_bool(
        fields.retry_exhausted,
        "retry_exhausted",
      ))
      Ok(ScheduledRunFailedBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        reason,
        retry_exhausted,
        fields.run_root,
      ))
    }
    "scheduled_run_retry_scheduled" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use next_attempt <- result.try(required_int(
        fields.next_attempt,
        "next_attempt",
      ))
      use delay_ms <- result.try(required_int(fields.delay_ms, "delay_ms"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(ScheduledRunRetryScheduledBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        next_attempt,
        delay_ms,
        generation,
        reason,
      ))
    }
    "scheduled_run_retry_cancelled" -> {
      use job_id <- result.try(required_string(fields.job_id, "job_id"))
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(ScheduledRunRetryCancelledBody(job_id, run_id, generation, reason))
    }
    "scheduled_failure_reported" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use linear_issue_id <- result.try(required_string(
        fields.linear_issue_id,
        "linear_issue_id",
      ))
      use action <- result.try(required_string(fields.action, "action"))
      Ok(ScheduledFailureReportedBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        dedupe_key,
        linear_issue_id,
        action,
      ))
    }
    "scheduled_failure_report_failed" -> {
      use #(job_id, workflow_id, due_at_ms, run_id) <- result.try(required_base(
        fields,
        required_string,
        required_int,
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use error_code <- result.try(required_string(
        fields.error_code,
        "error_code",
      ))
      use error_message <- result.try(required_string(
        fields.error_message,
        "error_message",
      ))
      use next_retry_at_ms <- result.try(required_int(
        fields.next_retry_at_ms,
        "next_retry_at_ms",
      ))
      use generation <- result.try(required_int(fields.generation, "generation"))
      Ok(ScheduledFailureReportFailedBody(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        dedupe_key,
        error_code,
        error_message,
        next_retry_at_ms,
        generation,
      ))
    }
    _ -> Error("unsupported scheduled kind " <> kind)
  }
}

fn required_base(
  fields: ScheduledFields,
  required_string: fn(Option(String), String) -> Result(String, String),
  required_int: fn(Option(Int), String) -> Result(Int, String),
) -> Result(#(String, String, Int, String), String) {
  use job_id <- result.try(required_string(fields.job_id, "job_id"))
  use workflow_id <- result.try(required_string(
    fields.workflow_id,
    "workflow_id",
  ))
  use due_at_ms <- result.try(required_int(fields.due_at_ms, "due_at_ms"))
  use run_id <- result.try(required_string(fields.run_id, "run_id"))
  Ok(#(job_id, workflow_id, due_at_ms, run_id))
}
