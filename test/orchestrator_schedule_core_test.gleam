import birl
import scherzo/orchestrator/schedule_core

fn ms(iso: String) -> Int {
  let assert Ok(time) = birl.parse(iso)
  birl.to_unix_milli(time)
}

pub fn interval_boundaries_and_run_ids_test() {
  let interval = 15 * 60 * 1000
  assert schedule_core.next_due_after(ms("2026-05-05T12:00:01Z"), interval)
    == ms("2026-05-05T12:15:00Z")
  assert schedule_core.due_at_or_before(ms("2026-05-05T12:14:59Z"), interval)
    == ms("2026-05-05T12:00:00Z")
  assert schedule_core.run_id("pr-conflict-repair", ms("2026-05-05T12:00:00Z"))
    == "schedule-pr-conflict-repair-20260505T120000Z"
  assert schedule_core.manual_run_id(
      "pr-conflict-repair",
      ms("2026-05-05T12:00:03Z"),
    )
    == "schedule-pr-conflict-repair-manual-20260505T120003Z"
}

pub fn parse_every_accepts_mvp_units_and_rejects_invalid_values_test() {
  assert schedule_core.parse_every("500ms") == Ok(500)
  assert schedule_core.parse_every("30s") == Ok(30_000)
  assert schedule_core.parse_every("15m") == Ok(900_000)
  assert schedule_core.parse_every("2h") == Ok(7_200_000)
  assert schedule_core.parse_every("abcms")
    == Error("scheduled job every must start with an integer")
  let assert Error(_) = schedule_core.parse_every("0s")
  let assert Error(_) = schedule_core.parse_every("-1m")
  let assert Error(_) = schedule_core.parse_every("15")
  let assert Error(_) = schedule_core.parse_every("15d")
}

pub fn delayed_tick_admits_latest_due_and_skips_older_boundaries_test() {
  let interval = 15 * 60 * 1000
  let state =
    schedule_core.ScheduleState(
      job_id: "repair",
      workflow_id: "repair",
      every_ms: interval,
      next_due_at_ms: ms("2026-05-05T12:15:00Z"),
      mode: schedule_core.Idle,
    )
  let decisions =
    schedule_core.admit_due_boundaries(state, ms("2026-05-05T12:46:00Z"))
  let assert [
    schedule_core.ScheduledSkipped(
      due_at_ms: skipped_due,
      reason: "catch_up_disabled",
      skipped_count: 2,
      ..,
    ),
    schedule_core.ScheduledDue(
      due_at_ms: due_at_ms,
      run_id: run_id,
      trigger: "automatic",
    ),
    schedule_core.ScheduledPending(
      due_at_ms: pending_due,
      run_id: pending_run_id,
      trigger: "automatic",
      ..,
    ),
    schedule_core.ScheduledNextDue(next_due_at_ms: next_due),
  ] = decisions
  assert skipped_due == ms("2026-05-05T12:15:00Z")
  assert due_at_ms == ms("2026-05-05T12:45:00Z")
  assert pending_due == due_at_ms
  assert pending_run_id == run_id
  assert next_due == ms("2026-05-05T13:00:00Z")
}

pub fn blocked_boundaries_are_summarized_by_reason_test() {
  let interval = 15 * 60 * 1000
  let state =
    schedule_core.ScheduleState(
      job_id: "repair",
      workflow_id: "repair",
      every_ms: interval,
      next_due_at_ms: ms("2026-05-05T12:15:00Z"),
      mode: schedule_core.Active,
    )
  let decisions =
    schedule_core.account_blocked_boundaries(
      state,
      ms("2026-05-05T12:46:00Z"),
      "overlap_running",
    )
  let assert [
    schedule_core.ScheduledSkipped(
      due_at_ms: skipped_due,
      reason: "overlap_running",
      skipped_count: 3,
      ..,
    ),
    schedule_core.ScheduledNextDue(next_due_at_ms: next_due),
  ] = decisions
  assert skipped_due == ms("2026-05-05T12:45:00Z")
  assert next_due == ms("2026-05-05T13:00:00Z")
}
