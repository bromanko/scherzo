import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/orchestrator/poll_scheduler

pub fn poll_scheduler_starts_and_accepts_current_generation_test() {
  let state = poll_scheduler.start(fn(generation) { generation })

  assert poll_scheduler.generation(state) == 1
  assert poll_scheduler.timer(state) == Some(1)
  assert poll_scheduler.in_flight(state) == None

  let assert Ok(state) = poll_scheduler.accept_tick(state, 1)
  assert poll_scheduler.in_flight(state) == Some(1)
  assert !poll_scheduler.result_is_stale(state, 1)
  assert poll_scheduler.result_is_stale(state, 2)
}

pub fn poll_scheduler_rejects_stale_or_concurrent_ticks_test() {
  let state = poll_scheduler.start(fn(generation) { generation })
  assert poll_scheduler.accept_tick(state, 2) == Error(Nil)

  let assert Ok(state) = poll_scheduler.accept_tick(state, 1)
  assert poll_scheduler.accept_tick(state, 1) == Error(Nil)
}

pub fn poll_scheduler_schedules_next_and_cancels_old_timer_test() {
  let cancelled = process.new_subject()
  let state = poll_scheduler.start(fn(generation) { generation })
  let assert Ok(state) = poll_scheduler.accept_tick(state, 1)

  let state =
    poll_scheduler.schedule_next(
      state,
      fn(generation) { generation + 100 },
      fn(timer) { process.send(cancelled, timer) },
    )

  assert poll_scheduler.generation(state) == 2
  assert poll_scheduler.in_flight(state) == None
  assert poll_scheduler.timer(state) == Some(102)
  assert process.receive(cancelled, within: 100) == Ok(1)
}

pub fn poll_scheduler_cancel_all_clears_timer_and_in_flight_test() {
  let cancelled = process.new_subject()
  let state = poll_scheduler.start(fn(generation) { generation })
  let assert Ok(state) = poll_scheduler.accept_tick(state, 1)

  let state =
    poll_scheduler.cancel_all(state, fn(timer) {
      process.send(cancelled, timer)
    })

  assert poll_scheduler.in_flight(state) == None
  assert poll_scheduler.timer(state) == None
  assert process.receive(cancelled, within: 100) == Ok(1)
}
