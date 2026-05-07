import gleam/erlang/process
import scherzo/lifecycle
import scherzo/signal
import test_async

pub fn install_with_ffi_reports_metadata_and_cleans_up_once_test() {
  let stop_subject = process.new_subject()
  let cleanup_subject = process.new_subject()

  let assert Ok(installation) =
    signal.install_with_ffi(
      stop_subject,
      fn(_) { Ok(#("fake-handle", "12345")) },
      fn(handle) { process.send(cleanup_subject, handle) },
    )

  assert installation.installed_signals == [lifecycle.Sigterm]
  assert installation.os_pid == "12345"

  installation.cleanup()
  installation.cleanup()

  assert process.receive(cleanup_subject, within: 1000) == Ok("fake-handle")
  test_async.assert_no_extra_message_within(cleanup_subject, 50)
}

pub fn install_with_ffi_maps_install_failure_test() {
  let stop_subject = process.new_subject()
  let cleanup_subject = process.new_subject()

  assert signal.install_with_ffi(
      stop_subject,
      fn(_) { Error("boom") },
      fn(handle) { process.send(cleanup_subject, handle) },
    )
    == Error(signal.InstallFailed("boom"))
  test_async.assert_no_extra_message_within(cleanup_subject, 50)
}
