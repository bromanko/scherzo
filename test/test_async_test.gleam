import gleam/erlang/process
import test_async

pub fn barrier_blocks_worker_until_released_test() {
  let barrier = test_async.new_barrier()
  let subject = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      process.send(subject, "before")
      test_async.wait_at_barrier(barrier)
      process.send(subject, "after")
    })

  assert test_async.expect_message(subject) == "before"
  test_async.assert_no_extra_message(subject)

  test_async.release_barrier(barrier)
  assert test_async.expect_message(subject) == "after"
}

pub fn drain_subject_returns_messages_in_receive_order_test() {
  let subject = process.new_subject()
  process.send(subject, "first")
  process.send(subject, "second")

  assert test_async.drain_subject(subject) == ["first", "second"]
  test_async.assert_no_extra_message(subject)
}
