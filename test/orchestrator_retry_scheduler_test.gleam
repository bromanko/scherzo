import gleam/erlang/process
import scherzo/orchestrator/retry_scheduler

pub fn retry_scheduler_schedules_and_cancels_timer_test() {
  let cancelled = process.new_subject()
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_timer("issue-1", 123, fn(_) { Nil })

  assert retry_scheduler.timer_for_issue(state, "issue-1") == Ok(123)

  let state =
    retry_scheduler.cancel_timer(state, "issue-1", fn(timer) {
      process.send(cancelled, timer)
    })

  assert process.receive(cancelled, within: 100) == Ok(123)
  assert retry_scheduler.timer_for_issue(state, "issue-1") == Error(Nil)
}

pub fn retry_scheduler_replaces_existing_timer_when_scheduling_test() {
  let cancelled = process.new_subject()
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_timer("issue-1", 123, fn(timer) {
      process.send(cancelled, timer)
    })
    |> retry_scheduler.schedule_timer("issue-1", 456, fn(timer) {
      process.send(cancelled, timer)
    })

  assert process.receive(cancelled, within: 100) == Ok(123)
  assert retry_scheduler.timer_for_issue(state, "issue-1") == Ok(456)
  assert process.receive(cancelled, within: 50) == Error(Nil)
}

pub fn retry_scheduler_tracks_one_refresh_per_issue_test() {
  let state = retry_scheduler.new()
  let assert Ok(state) = retry_scheduler.begin_refresh(state, "issue-1", 7)

  assert retry_scheduler.refresh_generation(state, "issue-1") == Ok(7)
  assert retry_scheduler.begin_refresh(state, "issue-1", 8) == Error(Nil)

  let state = retry_scheduler.finish_refresh(state, "issue-1")
  assert retry_scheduler.refresh_generation(state, "issue-1") == Error(Nil)

  let assert Ok(state) = retry_scheduler.begin_refresh(state, "issue-1", 8)
  assert retry_scheduler.refresh_generation(state, "issue-1") == Ok(8)
}

pub fn retry_scheduler_remove_timer_does_not_cancel_test() {
  let cancelled = process.new_subject()
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_timer("issue-1", 123, fn(timer) {
      process.send(cancelled, timer)
    })
    |> retry_scheduler.remove_timer("issue-1")

  assert retry_scheduler.timer_for_issue(state, "issue-1") == Error(Nil)
  assert process.receive(cancelled, within: 50) == Error(Nil)
}

pub fn retry_scheduler_cancel_all_clears_timers_and_refreshes_test() {
  let cancelled = process.new_subject()
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_timer("issue-1", 101, fn(timer) {
      process.send(cancelled, timer)
    })
    |> retry_scheduler.schedule_timer("issue-2", 202, fn(timer) {
      process.send(cancelled, timer)
    })
  let assert Ok(state) = retry_scheduler.begin_refresh(state, "issue-1", 1)

  let state =
    retry_scheduler.cancel_all(state, fn(timer) {
      process.send(cancelled, timer)
    })

  let first = process.receive(cancelled, within: 100)
  let second = process.receive(cancelled, within: 100)
  assert first == Ok(101) || first == Ok(202)
  assert second == Ok(101) || second == Ok(202)
  assert first != second
  assert retry_scheduler.timer_for_issue(state, "issue-1") == Error(Nil)
  assert retry_scheduler.timer_for_issue(state, "issue-2") == Error(Nil)
  assert retry_scheduler.refresh_generation(state, "issue-1") == Error(Nil)
}
