import gleam/erlang/process
import gleam/list

/// Default timeout for positive receives that should happen after an explicit
/// test synchronization point.
pub const default_receive_timeout_ms = 1000

/// Short timeout used only to drain a subject once the test has already
/// synchronized with the actor or worker under test.
pub const drain_timeout_ms = 10

/// Short timeout for asserting that no further messages arrive after a
/// deterministic synchronization point.
pub const no_extra_message_timeout_ms = 20

pub type Barrier {
  Barrier(waiting: process.Subject(process.Subject(Nil)))
}

pub fn new_barrier() -> Barrier {
  Barrier(process.new_subject())
}

pub fn wait_at_barrier(barrier: Barrier) -> Nil {
  let Barrier(waiting) = barrier
  let release = process.new_subject()
  process.send(waiting, release)
  let _ = process.receive_forever(release)
  Nil
}

pub fn release_barrier(barrier: Barrier) -> Nil {
  let Barrier(waiting) = barrier
  let release = expect_message(waiting)
  process.send(release, Nil)
}

pub fn release_barrier_if_waiting(barrier: Barrier) -> Nil {
  release_barrier_if_waiting_within(barrier, no_extra_message_timeout_ms)
}

pub fn release_barrier_if_waiting_within(
  barrier: Barrier,
  timeout_ms: Int,
) -> Nil {
  let Barrier(waiting) = barrier
  case process.receive(waiting, within: timeout_ms) {
    Ok(release) -> process.send(release, Nil)
    Error(_) -> Nil
  }
}

/// Use in fake agents/workers that need to remain active until the test has
/// made assertions or sent an operator command. This replaces long sleeps such
/// as `process.sleep(5000)` in tests.
pub fn block_until_released(barrier: Barrier) -> Nil {
  wait_at_barrier(barrier)
}

pub fn expect_message(subject: process.Subject(a)) -> a {
  expect_message_within(subject, default_receive_timeout_ms)
}

pub fn expect_message_within(
  subject: process.Subject(a),
  timeout_ms: Int,
) -> a {
  let assert Ok(message) = process.receive(subject, within: timeout_ms)
  message
}

pub fn drain_subject(subject: process.Subject(a)) -> List(a) {
  drain_subject_loop(subject, [])
}

fn drain_subject_loop(subject: process.Subject(a), acc: List(a)) -> List(a) {
  case process.receive(subject, within: drain_timeout_ms) {
    Ok(message) -> drain_subject_loop(subject, [message, ..acc])
    Error(_) -> list.reverse(acc)
  }
}

pub fn assert_no_extra_message(subject: process.Subject(a)) -> Nil {
  assert_no_extra_message_within(subject, no_extra_message_timeout_ms)
}

pub fn assert_no_extra_message_within(
  subject: process.Subject(a),
  timeout_ms: Int,
) -> Nil {
  case process.receive(subject, within: timeout_ms) {
    Error(_) -> Nil
    Ok(_) -> panic as "expected no extra message after synchronization point"
  }
}
