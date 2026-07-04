import birl
import gleam/result
import gleam/string
import scherzo/duration
import scherzo/workflow_identity

// Persisted due times older than this are treated as legacy monotonic-clock
// values rather than real wall-clock schedule boundaries.
const max_persisted_due_lag_ms = 2_592_000_000

pub const default_quarantine_after_failures = 3

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
  duration.parse_positive_ms(value, "scheduled job every")
  |> result.map_error(duration.error_message)
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

pub fn next_due_after_persisted_due(
  persisted_due_at_ms: Int,
  now_ms: Int,
  every_ms: Int,
) -> Int {
  let projected_next_due = next_due_after(persisted_due_at_ms, every_ms)
  let startup_next_due = initial_next_due(now_ms, every_ms)
  case projected_next_due > startup_next_due {
    True -> startup_next_due
    False -> {
      case now_ms - projected_next_due > max_persisted_due_lag_ms {
        True -> startup_next_due
        False -> projected_next_due
      }
    }
  }
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
