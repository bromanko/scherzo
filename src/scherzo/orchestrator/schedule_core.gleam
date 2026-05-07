import birl
import gleam/int
import gleam/result
import gleam/string
import scherzo/workflow_identity

pub type ScheduledRunContext {
  ScheduledRunContext(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    started_at_ms: Int,
    run_id: String,
    attempt: Int,
    trigger: String,
  )
}

pub type ScheduleMode {
  Idle
  Pending(blocking_reason: String)
  Active
  RetryWaiting
}

pub type ScheduleState {
  ScheduleState(
    job_id: String,
    workflow_id: String,
    every_ms: Int,
    next_due_at_ms: Int,
    mode: ScheduleMode,
  )
}

pub type ScheduleDecision {
  ScheduledDue(due_at_ms: Int, run_id: String, trigger: String)
  ScheduledPending(
    due_at_ms: Int,
    run_id: String,
    trigger: String,
    requested_at_ms: Int,
  )
  ScheduledSkipped(
    due_at_ms: Int,
    run_id: String,
    reason: String,
    skipped_count: Int,
  )
  ScheduledNextDue(next_due_at_ms: Int)
}

pub fn parse_every(value: String) -> Result(Int, String) {
  let value = string.trim(value)
  case value == "" {
    True -> Error("scheduled job every must be non-empty")
    False -> parse_every_non_empty(value)
  }
}

fn parse_every_non_empty(value: String) -> Result(Int, String) {
  let #(number_text, multiplier, unit_ok) = case string.ends_with(value, "ms") {
    True -> #(string.drop_end(value, 2), 1, True)
    False ->
      case string.ends_with(value, "s") {
        True -> #(string.drop_end(value, 1), 1000, True)
        False ->
          case string.ends_with(value, "m") {
            True -> #(string.drop_end(value, 1), 60_000, True)
            False ->
              case string.ends_with(value, "h") {
                True -> #(string.drop_end(value, 1), 3_600_000, True)
                False -> #(value, 1, False)
              }
          }
      }
  }
  case unit_ok {
    False -> Error("scheduled job every must use unit ms, s, m, or h")
    True -> {
      use number <- result.try(
        number_text
        |> string.trim
        |> int.parse
        |> result.replace_error(
          "scheduled job every must start with an integer",
        ),
      )
      case number <= 0 {
        True -> Error("scheduled job every must be positive")
        False -> Ok(number * multiplier)
      }
    }
  }
}

pub fn next_due_after(now_ms: Int, every_ms: Int) -> Int {
  { now_ms / every_ms + 1 } * every_ms
}

pub fn due_at_or_before(now_ms: Int, every_ms: Int) -> Int {
  { now_ms / every_ms } * every_ms
}

pub fn initial_next_due(startup_ms: Int, every_ms: Int) -> Int {
  next_due_after(startup_ms, every_ms)
}

pub fn run_id(job_id: String, due_at_ms: Int) -> String {
  "schedule-"
  <> workflow_identity.safe_component(job_id, "job")
  <> "-"
  <> utc_basic(due_at_ms)
}

pub fn manual_run_id(job_id: String, started_at_ms: Int) -> String {
  "schedule-"
  <> workflow_identity.safe_component(job_id, "job")
  <> "-manual-"
  <> utc_basic(started_at_ms)
}

pub fn iso_utc(at_ms: Int) -> String {
  birl.from_unix_milli(at_ms)
  |> birl.to_iso8601
  |> string.replace(".000", "")
  |> string.replace("+00:00", "Z")
}

pub fn retry_delay(attempt: Int, max_backoff_ms: Int) -> Int {
  backoff_delay_loop(10_000, attempt - 1, max_backoff_ms)
}

pub fn retry_exhausted(next_attempt: Int, max_attempts: Int) -> Bool {
  next_attempt > max_attempts
}

pub fn admit_due_boundaries(
  state: ScheduleState,
  now_ms: Int,
) -> List(ScheduleDecision) {
  case now_ms < state.next_due_at_ms {
    True -> []
    False -> {
      let latest_due = due_at_or_before(now_ms, state.every_ms)
      let boundary_count =
        count_boundaries(state.next_due_at_ms, latest_due, state.every_ms)
      case state.mode {
        Idle -> {
          let skipped = boundary_count - 1
          let due_run_id = run_id(state.job_id, latest_due)
          let next_due = next_due_after(latest_due, state.every_ms)
          case skipped > 0 {
            True -> [
              ScheduledSkipped(
                state.next_due_at_ms,
                run_id(state.job_id, state.next_due_at_ms),
                "catch_up_disabled",
                skipped,
              ),
              ScheduledDue(latest_due, due_run_id, "automatic"),
              ScheduledPending(latest_due, due_run_id, "automatic", now_ms),
              ScheduledNextDue(next_due),
            ]
            False -> [
              ScheduledDue(latest_due, due_run_id, "automatic"),
              ScheduledPending(latest_due, due_run_id, "automatic", now_ms),
              ScheduledNextDue(next_due),
            ]
          }
        }
        Pending(blocking_reason) ->
          account_blocked_boundaries(state, now_ms, blocking_reason)
        Active -> account_blocked_boundaries(state, now_ms, "overlap_running")
        RetryWaiting ->
          account_blocked_boundaries(state, now_ms, "overlap_running")
      }
    }
  }
}

pub fn account_blocked_boundaries(
  state: ScheduleState,
  now_ms: Int,
  reason: String,
) -> List(ScheduleDecision) {
  case now_ms < state.next_due_at_ms {
    True -> []
    False -> {
      let latest_due = due_at_or_before(now_ms, state.every_ms)
      let skipped_count =
        count_boundaries(state.next_due_at_ms, latest_due, state.every_ms)
      [
        ScheduledSkipped(
          latest_due,
          run_id(state.job_id, latest_due),
          reason,
          skipped_count,
        ),
        ScheduledNextDue(next_due_after(now_ms, state.every_ms)),
      ]
    }
  }
}

fn count_boundaries(first_due: Int, latest_due: Int, every_ms: Int) -> Int {
  case latest_due < first_due {
    True -> 0
    False -> { { latest_due - first_due } / every_ms } + 1
  }
}

fn utc_basic(at_ms: Int) -> String {
  iso_utc(at_ms)
  |> string.replace("-", "")
  |> string.replace(":", "")
}

fn backoff_delay_loop(
  delay_ms: Int,
  remaining_doubles: Int,
  max_ms: Int,
) -> Int {
  case delay_ms >= max_ms {
    True -> max_ms
    False ->
      case remaining_doubles <= 0 {
        True -> delay_ms
        False -> backoff_delay_loop(delay_ms * 2, remaining_doubles - 1, max_ms)
      }
  }
}
