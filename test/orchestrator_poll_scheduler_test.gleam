import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/orchestrator/poll_jitter
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

pub fn poll_scheduler_schedules_jittered_next_with_metadata_test() {
  let cancelled = process.new_subject()
  let scheduled = process.new_subject()
  let state = poll_scheduler.start(fn(generation) { #(0, generation) })
  let assert Ok(state) = poll_scheduler.accept_tick(state, 1)

  let #(state, delay) =
    poll_scheduler.schedule_next_jittered_message(
      state,
      1000,
      "seed-a",
      Nil,
      fn(generation) { generation },
      fn(_, delay_ms, message) {
        process.send(scheduled, #(delay_ms, message))
        #(delay_ms, message)
      },
      fn(timer) { process.send(cancelled, timer) },
    )

  let expected_delay_ms = poll_jitter.delay_ms(1000, "seed-a", 2)
  assert poll_scheduler.generation(state) == 2
  assert poll_scheduler.in_flight(state) == None
  assert poll_scheduler.timer(state) == Some(#(expected_delay_ms, 2))
  assert process.receive(cancelled, within: 100) == Ok(#(0, 1))
  assert process.receive(scheduled, within: 100) == Ok(#(expected_delay_ms, 2))
  assert poll_scheduler.jitter_log_fields(delay)
    == [
      #("generation", "2"),
      #("polling_interval_ms", "1000"),
      #("polling_jitter_bound_ms", "100"),
      #("next_poll_delay_ms", "1043"),
    ]
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
