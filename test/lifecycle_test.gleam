import gleam/erlang/process
import scherzo/lifecycle
import scherzo/log
import test_async

type LogEntry {
  LogEntry(level: String, event: String, fields: List(log.Field))
}

pub fn run_until_stop_calls_shutdown_cleanup_and_release_once_test() {
  let ready = process.new_subject()
  let shutdown_called = process.new_subject()
  let cleanup_called = process.new_subject()
  let release_called = process.new_subject()
  let result_subject = process.new_subject()
  let log_subject = process.new_subject()

  let _pid =
    process.spawn_unlinked(fn() {
      let stop_subject = process.new_subject()
      process.send(ready, stop_subject)
      let result =
        lifecycle.run_until_stop(
          stop_subject,
          fn(reason) {
            process.send(shutdown_called, reason)
            Ok(Nil)
          },
          fn() { process.send(cleanup_called, "cleanup") },
          fn() { process.send(release_called, "release") },
          fn(level, event, fields) {
            process.send(log_subject, LogEntry(level, event, fields))
          },
        )
      process.send(result_subject, result)
    })

  let assert Ok(stop_subject) = process.receive(ready, within: 1000)
  process.send(stop_subject, lifecycle.TestStop("test"))

  assert process.receive(shutdown_called, within: 1000)
    == Ok(lifecycle.TestStop("test"))
  assert process.receive(cleanup_called, within: 1000) == Ok("cleanup")
  assert process.receive(release_called, within: 1000) == Ok("release")
  assert process.receive(result_subject, within: 1000)
    == Ok(lifecycle.ShutdownComplete)
  assert has_log_event(log_subject, "daemon_stop_requested", 5)
  assert has_log_event(log_subject, "daemon_shutdown_complete", 5)
  test_async.assert_no_extra_message_within(shutdown_called, 20)
  test_async.assert_no_extra_message_within(cleanup_called, 20)
  test_async.assert_no_extra_message_within(release_called, 20)
}

pub fn run_until_stop_ignores_duplicate_stop_messages_test() {
  let ready = process.new_subject()
  let shutdown_called = process.new_subject()
  let cleanup_called = process.new_subject()
  let release_called = process.new_subject()
  let result_subject = process.new_subject()
  let log_subject = process.new_subject()

  let _pid =
    process.spawn_unlinked(fn() {
      let stop_subject = process.new_subject()
      let continue_subject = process.new_subject()
      process.send(ready, #(stop_subject, continue_subject))
      let result =
        lifecycle.run_until_stop(
          stop_subject,
          fn(reason) {
            process.send(shutdown_called, reason)
            let _ = process.receive(continue_subject, within: 1000)
            Ok(Nil)
          },
          fn() { process.send(cleanup_called, "cleanup") },
          fn() { process.send(release_called, "release") },
          fn(level, event, fields) {
            process.send(log_subject, LogEntry(level, event, fields))
          },
        )
      process.send(result_subject, result)
    })

  let assert Ok(#(stop_subject, continue_subject)) =
    process.receive(ready, within: 1000)
  process.send(stop_subject, lifecycle.TestStop("first"))
  assert process.receive(shutdown_called, within: 1000)
    == Ok(lifecycle.TestStop("first"))
  process.send(stop_subject, lifecycle.TestStop("duplicate"))
  process.send(continue_subject, Nil)

  assert process.receive(cleanup_called, within: 1000) == Ok("cleanup")
  assert process.receive(release_called, within: 1000) == Ok("release")
  assert process.receive(result_subject, within: 1000)
    == Ok(lifecycle.ShutdownComplete)
  test_async.assert_no_extra_message_within(shutdown_called, 50)
  test_async.assert_no_extra_message_within(cleanup_called, 20)
  test_async.assert_no_extra_message_within(release_called, 20)
}

pub fn shutdown_timeout_returns_error_test() {
  let stop_subject = process.new_subject()
  let cleanup_called = process.new_subject()
  let release_called = process.new_subject()
  let log_subject = process.new_subject()

  process.send(stop_subject, lifecycle.TestStop("timeout-test"))
  let result =
    lifecycle.run_until_stop(
      stop_subject,
      fn(_) { Error(Nil) },
      fn() { process.send(cleanup_called, "cleanup") },
      fn() { process.send(release_called, "release") },
      fn(level, event, fields) {
        process.send(log_subject, LogEntry(level, event, fields))
      },
    )

  assert result == lifecycle.ShutdownTimedOut
  assert process.receive(cleanup_called, within: 1000) == Ok("cleanup")
  assert process.receive(release_called, within: 1000) == Ok("release")
  assert has_log_event(log_subject, "daemon_stop_requested", 5)
  assert has_log_event(log_subject, "daemon_shutdown_timeout", 5)
  test_async.assert_no_extra_message_within(cleanup_called, 20)
  test_async.assert_no_extra_message_within(release_called, 20)
}

pub fn shutdown_crash_still_cleans_up_and_releases_test() {
  let stop_subject = process.new_subject()
  let cleanup_called = process.new_subject()
  let release_called = process.new_subject()
  let log_subject = process.new_subject()

  process.send(stop_subject, lifecycle.TestStop("crash-test"))
  let result =
    lifecycle.run_until_stop(
      stop_subject,
      fn(_) { panic as "shutdown crashed" },
      fn() { process.send(cleanup_called, "cleanup") },
      fn() { process.send(release_called, "release") },
      fn(level, event, fields) {
        process.send(log_subject, LogEntry(level, event, fields))
      },
    )

  assert result == lifecycle.ShutdownTimedOut
  assert process.receive(cleanup_called, within: 1000) == Ok("cleanup")
  assert process.receive(release_called, within: 1000) == Ok("release")
  assert has_log_event(log_subject, "daemon_stop_requested", 5)
  assert has_log_event(log_subject, "daemon_shutdown_timeout", 5)
}

fn has_log_event(
  subject: process.Subject(LogEntry),
  expected: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 100) {
        Ok(LogEntry(_, event, _)) ->
          case event == expected {
            True -> True
            False -> has_log_event(subject, expected, attempts - 1)
          }
        Error(_) -> has_log_event(subject, expected, attempts - 1)
      }
  }
}
