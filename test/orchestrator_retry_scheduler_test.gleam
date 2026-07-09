import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/orchestrator/retry_scheduler
import scherzo/task
import test_async

fn task_ref(
  backend_kind: String,
  remote_id: String,
  key: String,
) -> task.TaskRef {
  task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: Some(key),
    url: None,
  )
}

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
  test_async.assert_no_extra_message_within(cancelled, 50)
}

pub fn retry_scheduler_distinguishes_duplicate_remote_ids_by_backend_test() {
  let cancelled = process.new_subject()
  let linear = task_ref("linear", "shared", "ABC-1")
  let memory = task_ref("test-memory", "shared", "MEM-1")
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_task_timer(linear, 101, fn(timer) {
      process.send(cancelled, timer)
    })
    |> retry_scheduler.schedule_task_timer(memory, 202, fn(timer) {
      process.send(cancelled, timer)
    })

  assert retry_scheduler.timer_for_task_ref(state, linear) == Ok(101)
  assert retry_scheduler.timer_for_task_ref(state, memory) == Ok(202)

  let state =
    retry_scheduler.cancel_task_timer(state, linear, fn(timer) {
      process.send(cancelled, timer)
    })

  assert process.receive(cancelled, within: 100) == Ok(101)
  assert retry_scheduler.timer_for_task_ref(state, linear) == Error(Nil)
  assert retry_scheduler.timer_for_task_ref(state, memory) == Ok(202)
  test_async.assert_no_extra_message_within(cancelled, 50)
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
  test_async.assert_no_extra_message_within(cancelled, 50)
}

pub fn retry_scheduler_cancel_all_clears_timers_test() {
  let cancelled = process.new_subject()
  let state =
    retry_scheduler.new()
    |> retry_scheduler.schedule_timer("issue-1", 101, fn(timer) {
      process.send(cancelled, timer)
    })
    |> retry_scheduler.schedule_timer("issue-2", 202, fn(timer) {
      process.send(cancelled, timer)
    })

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
}
