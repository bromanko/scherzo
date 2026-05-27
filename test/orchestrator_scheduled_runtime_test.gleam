import birl
import gleam/option.{None}
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/scheduled_runtime

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
      ),
    ]
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.Idle
}

pub fn worker_failure_follow_up_chooses_retry_then_report_test() {
  let #(runtime, retry_follow_up) =
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
      3,
      60_000,
    )
  let assert scheduled_runtime.WorkerFailureRetry(actions) = retry_follow_up
  let assert [
    scheduled_runtime.RecordScheduledRetry(generation: 1, ..),
    scheduled_runtime.ScheduleRetryTimer(generation: 1, ..),
  ] = actions
  assert scheduled_runtime.schedule_mode(runtime, "scheduled-job", False)
    == schedule_core.RetryWaiting

  let #(_, report_follow_up) =
    scheduled_runtime.worker_failure_follow_up(
      scheduled_runtime.new(),
      "scheduled-job",
      "scheduled-command",
      ms("2026-05-05T12:15:00Z"),
      "schedule-scheduled-job-20260505T121500Z",
      3,
      "boom",
      None,
      None,
      3,
      60_000,
    )
  let assert scheduled_runtime.WorkerFailureReport(request: scheduled_runtime.FailureReportRequest(
    reason: "boom",
    attempt: 3,
    ..,
  )) = report_follow_up
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
