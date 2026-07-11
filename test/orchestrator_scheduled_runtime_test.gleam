import birl
import gleam/dict
import gleam/erlang/process
import gleam/option.{None}
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/scheduled_runtime
import test_async

fn ms(iso: String) -> Int {
  let assert Ok(time) = birl.parse(iso)
  birl.to_unix_milli(time)
}

pub fn runtime_initializes_from_next_due_data_test() {
  let runtime =
    scheduled_runtime.from_next_due([
      #("scheduled-job", ms("2026-05-05T12:15:00Z")),
    ])
  let #(runtime, next_due_at_ms) =
    scheduled_runtime.ensure_next_due(
      runtime,
      "scheduled-job",
      ms("2026-05-05T12:00:10Z"),
      15 * 60 * 1000,
    )

  assert next_due_at_ms == ms("2026-05-05T12:15:00Z")
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Idle
}

pub fn due_admission_emits_due_pending_and_next_due_actions_test() {
  let #(runtime, actions) =
    scheduled_runtime.admit_due(
      scheduled_runtime.from_next_due([
        #("scheduled-job", ms("2026-05-05T12:15:00Z")),
      ]),
      "scheduled-job",
      "scheduled-command",
      15 * 60 * 1000,
      ms("2026-05-05T12:15:00Z"),
      False,
    )

  let assert [
    scheduled_runtime.RecordScheduledDue(
      job_id: "scheduled-job",
      workflow_id: "scheduled-command",
      due_at_ms: due_at_ms,
      run_id: run_id,
      trigger: "automatic",
    ),
    scheduled_runtime.RecordScheduledPending(pending: scheduled_runtime.PendingStart(
      job_id: "scheduled-job",
      workflow_id: "scheduled-command",
      due_at_ms: pending_due_at_ms,
      run_id: pending_run_id,
      trigger: "automatic",
      requested_at_ms: requested_at_ms,
      attempt: 1,
      blocking_reason: "",
    )),
    scheduled_runtime.UpdateNextDue(
      job_id: "scheduled-job",
      next_due_at_ms: next_due_at_ms,
    ),
  ] = actions

  assert due_at_ms == ms("2026-05-05T12:15:00Z")
  assert pending_due_at_ms == due_at_ms
  assert pending_run_id == run_id
  assert requested_at_ms == ms("2026-05-05T12:15:00Z")
  assert next_due_at_ms == ms("2026-05-05T12:30:00Z")
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Pending("")
}

pub fn pending_start_insertion_and_deletion_test() {
  let pending =
    scheduled_runtime.PendingStart(
      job_id: "scheduled-job",
      workflow_id: "scheduled-command",
      due_at_ms: ms("2026-05-05T12:15:00Z"),
      run_id: "schedule-scheduled-job-20260505T121500Z",
      trigger: "automatic",
      requested_at_ms: ms("2026-05-05T12:15:00Z"),
      attempt: 1,
      blocking_reason: "",
    )
  let runtime =
    scheduled_runtime.insert_pending_start(scheduled_runtime.new(), pending)
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Pending("")

  let runtime = scheduled_runtime.remove_pending_start(runtime, "scheduled-job")
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Idle
}

pub fn blocked_pending_start_updates_reason_and_normalizes_paused_mode_test() {
  let pending =
    scheduled_runtime.PendingStart(
      job_id: "scheduled-job",
      workflow_id: "scheduled-command",
      due_at_ms: ms("2026-05-05T12:15:00Z"),
      run_id: "schedule-scheduled-job-20260505T121500Z",
      trigger: "automatic",
      requested_at_ms: ms("2026-05-05T12:15:00Z"),
      attempt: 1,
      blocking_reason: "",
    )
  let #(runtime, actions) =
    scheduled_runtime.block_pending_start(
      scheduled_runtime.insert_pending_start(scheduled_runtime.new(), pending),
      "scheduled-job",
      "paused",
      ms("2026-05-05T12:15:30Z"),
    )

  let assert [
    scheduled_runtime.RecordScheduledPendingBlocked(
      pending: scheduled_runtime.PendingStart(blocking_reason: "paused", ..),
      blocked_at_ms: blocked_at_ms,
    ),
  ] = actions
  assert blocked_at_ms == ms("2026-05-05T12:15:30Z")
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Pending("schedule_paused")
}

pub fn retry_generation_mismatch_is_ignored_and_matching_tick_promotes_pending_test() {
  let #(runtime, schedule_actions) =
    scheduled_runtime.schedule_retry(
      scheduled_runtime.new(),
      "scheduled-job",
      "scheduled-command",
      ms("2026-05-05T12:15:00Z"),
      "schedule-scheduled-job-20260505T121500Z",
      1,
      2,
      "boom",
      60_000,
    )

  let assert [
    scheduled_runtime.RecordScheduledRetry(generation: 1, delay_ms: 10_000, ..),
    scheduled_runtime.ScheduleRetryTimer(
      run_id: "schedule-scheduled-job-20260505T121500Z",
      generation: 1,
      delay_ms: 10_000,
    ),
  ] = schedule_actions

  let #(runtime, stale_actions) =
    scheduled_runtime.handle_retry_tick(
      runtime,
      "schedule-scheduled-job-20260505T121500Z",
      2,
      ms("2026-05-05T12:15:30Z"),
      True,
      False,
    )
  assert stale_actions == []

  let #(runtime, actions) =
    scheduled_runtime.handle_retry_tick(
      runtime,
      "schedule-scheduled-job-20260505T121500Z",
      1,
      ms("2026-05-05T12:15:30Z"),
      True,
      False,
    )
  let assert [
    scheduled_runtime.PromoteRetryToPending(pending: scheduled_runtime.PendingStart(
      attempt: 2,
      blocking_reason: "",
      requested_at_ms: requested_at_ms,
      ..,
    )),
  ] = actions
  assert requested_at_ms == ms("2026-05-05T12:15:30Z")
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Pending("")
}

pub fn retry_tick_defers_while_paused_or_out_of_slots_test() {
  let #(runtime, _) =
    scheduled_runtime.schedule_retry(
      scheduled_runtime.new(),
      "scheduled-job",
      "scheduled-command",
      ms("2026-05-05T12:15:00Z"),
      "schedule-scheduled-job-20260505T121500Z",
      1,
      2,
      "boom",
      60_000,
    )
  let #(_, actions) =
    scheduled_runtime.handle_retry_tick(
      runtime,
      "schedule-scheduled-job-20260505T121500Z",
      1,
      ms("2026-05-05T12:15:30Z"),
      False,
      False,
    )

  assert actions
    == [
      scheduled_runtime.ScheduleRetryTimer(
        "schedule-scheduled-job-20260505T121500Z",
        1,
        1000,
      ),
    ]
}

pub fn report_retry_generation_mismatch_is_ignored_and_matching_tick_retries_test() {
  let runtime =
    scheduled_runtime.register_report_retry(
      scheduled_runtime.new(),
      "scheduled-job",
      "schedule-scheduled-job-20260505T121500Z",
    )
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.RetryWaiting

  let #(runtime, stale_actions) =
    scheduled_runtime.handle_report_retry_tick(
      runtime,
      "schedule-scheduled-job-20260505T121500Z",
      2,
    )
  assert stale_actions == []

  let #(runtime, actions) =
    scheduled_runtime.handle_report_retry_tick(
      runtime,
      "schedule-scheduled-job-20260505T121500Z",
      1,
    )
  assert actions
    == [
      scheduled_runtime.RetryReport(
        "scheduled-job",
        "schedule-scheduled-job-20260505T121500Z",
        2,
      ),
    ]
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Idle
}

pub fn worker_failure_follow_up_reports_without_retry_test() {
  let #(runtime, follow_up) =
    scheduled_runtime.worker_failure_follow_up(
      scheduled_runtime.new(),
      "scheduled-job",
      "scheduled-command",
      ms("2026-05-05T12:15:00Z"),
      "schedule-scheduled-job-20260505T121500Z",
      1,
      "boom",
      None,
      None,
    )
  let assert scheduled_runtime.WorkerFailureReport(request: scheduled_runtime.FailureReportRequest(
    reason: "boom",
    attempt: 1,
    ..,
  )) = follow_up
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Idle
}

pub fn report_failure_retry_schedules_timer_test() {
  let #(runtime, delay_ms, actions) =
    scheduled_runtime.schedule_report_retry_after_failure(
      scheduled_runtime.new(),
      "scheduled-job",
      "schedule-scheduled-job-20260505T121500Z",
      2,
      60_000,
    )

  assert delay_ms == 20_000
  assert actions
    == [
      scheduled_runtime.ScheduleReportRetryTimer(
        "schedule-scheduled-job-20260505T121500Z",
        2,
        20_000,
      ),
    ]
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.RetryWaiting
}

pub fn report_failure_retry_uses_per_run_attempt_for_fresh_runs_test() {
  let #(runtime, _, _) =
    scheduled_runtime.schedule_report_retry_after_failure(
      scheduled_runtime.new(),
      "scheduled-job-a",
      "schedule-scheduled-job-a-20260505T121500Z",
      5,
      60_000,
    )
  let #(_, delay_ms, actions) =
    scheduled_runtime.schedule_report_retry_after_failure(
      runtime,
      "scheduled-job-b",
      "schedule-scheduled-job-b-20260505T121500Z",
      1,
      60_000,
    )

  assert delay_ms == 10_000
  assert actions
    == [
      scheduled_runtime.ScheduleReportRetryTimer(
        "schedule-scheduled-job-b-20260505T121500Z",
        1,
        10_000,
      ),
    ]
}

pub fn report_attempts_exhaust_at_default_bound_test() {
  assert !scheduled_runtime.report_attempts_exhausted(4)
  assert scheduled_runtime.report_attempts_exhausted(5)
}

pub fn timer_replacement_and_removal_cancel_existing_timer_test() {
  let cancelled = process.new_subject()
  let cancel_timer = fn(timer) { process.send(cancelled, timer) }

  let timers =
    scheduled_runtime.insert_timer_cancelling_existing(
      dict.new(),
      "run-1",
      "timer-1",
      cancel_timer,
    )
  test_async.assert_no_extra_message_within(cancelled, 20)

  let timers =
    scheduled_runtime.insert_timer_cancelling_existing(
      timers,
      "run-1",
      "timer-2",
      cancel_timer,
    )
  let assert Ok("timer-1") = process.receive(cancelled, within: 1000)

  let timers =
    scheduled_runtime.delete_timer_cancelling_existing(
      timers,
      "run-1",
      cancel_timer,
    )
  let assert Ok("timer-2") = process.receive(cancelled, within: 1000)

  let timers =
    scheduled_runtime.delete_timer_cancelling_existing(
      timers,
      "run-1",
      cancel_timer,
    )
  assert dict.size(timers) == 0
  test_async.assert_no_extra_message_within(cancelled, 20)
}

pub fn owner_state_keeps_runtime_and_timer_lifecycle_atomic_test() {
  let cancelled = process.new_subject()
  let cancel_timer = fn(timer) { process.send(cancelled, timer) }
  let runtime =
    scheduled_runtime.insert_retry(
      scheduled_runtime.new(),
      scheduled_runtime.RetryStart(
        job_id: "job-1",
        workflow_id: "implementation",
        due_at_ms: 100,
        run_id: "run-1",
        next_attempt: 2,
        generation: 1,
      ),
    )
  let owner =
    scheduled_runtime.owner(runtime)
    |> scheduled_runtime.insert_retry_timer_cancelling_existing(
      "run-1",
      "timer-1",
      cancel_timer,
    )
    |> scheduled_runtime.insert_retry_timer_cancelling_existing(
      "run-1",
      "timer-2",
      cancel_timer,
    )
  let assert Ok("timer-1") = process.receive(cancelled, within: 1000)
  assert scheduled_runtime.retry_timer_count(owner) == 1

  let #(owner, actions) =
    scheduled_runtime.handle_retry_tick_owner(
      owner,
      "run-1",
      1,
      200,
      True,
      False,
    )
  assert scheduled_runtime.retry_timer_count(owner) == 0
  assert actions
    == [
      scheduled_runtime.PromoteRetryToPending(scheduled_runtime.PendingStart(
        job_id: "job-1",
        workflow_id: "implementation",
        due_at_ms: 100,
        run_id: "run-1",
        trigger: "automatic",
        requested_at_ms: 200,
        attempt: 2,
        blocking_reason: "",
      )),
    ]

  let #(owner, duplicate_actions) =
    scheduled_runtime.handle_retry_tick_owner(
      owner,
      "run-1",
      1,
      201,
      True,
      False,
    )
  assert duplicate_actions == []
  assert scheduled_runtime.retry_timer_count(owner) == 0
  test_async.assert_no_extra_message_within(cancelled, 20)
}
