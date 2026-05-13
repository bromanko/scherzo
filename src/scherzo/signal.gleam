import gleam/erlang/process
import gleam/string
import scherzo/lifecycle

type CleanupMessage {
  Cleanup(process.Subject(Nil))
}

pub type SignalHandle

pub type SignalError {
  SignalServerUnavailable(reason: String)
  InstallFailed(reason: String)
  HandlerVerificationFailed(reason: String)
  UnexpectedFfiFailure(function: String, detail: String)
}

pub type Installation {
  Installation(
    cleanup: fn() -> Nil,
    installed_signals: List(lifecycle.StopReason),
    os_pid: String,
  )
}

pub fn install(
  subject: process.Subject(lifecycle.StopReason),
) -> Result(Installation, SignalError) {
  install_with_ffi(subject, ffi_install_sigterm, ffi_cleanup_sigterm)
}

pub fn install_with_ffi(
  subject: process.Subject(lifecycle.StopReason),
  ffi_install: fn(process.Subject(lifecycle.StopReason)) ->
    Result(#(handle, String), String),
  ffi_cleanup: fn(handle) -> Nil,
) -> Result(Installation, SignalError) {
  case ffi_install(subject) {
    Error(message) -> Error(raw_signal_error("install_sigterm", message))
    Ok(#(handle, os_pid)) -> {
      case start_cleanup_server(handle, ffi_cleanup) {
        Error(message) -> {
          ffi_cleanup(handle)
          Error(message)
        }
        Ok(cleanup_subject) ->
          Ok(Installation(
            cleanup: fn() { cleanup_once(cleanup_subject) },
            installed_signals: [lifecycle.Sigterm],
            os_pid: os_pid,
          ))
      }
    }
  }
}

fn start_cleanup_server(
  handle: handle,
  ffi_cleanup: fn(handle) -> Nil,
) -> Result(process.Subject(CleanupMessage), SignalError) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      cleanup_loop(subject, handle, ffi_cleanup)
    })
  case process.receive(ready, within: 1000) {
    Error(Nil) -> {
      process.kill(pid)
      Error(InstallFailed("signal_cleanup_server_start_timeout"))
    }
    Ok(subject) -> Ok(subject)
  }
}

fn cleanup_once(subject: process.Subject(CleanupMessage)) -> Nil {
  let ack = process.new_subject()
  case process.subject_owner(subject) {
    Error(_) -> Nil
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      case process.is_alive(pid) {
        False -> {
          process.demonitor_process(monitor)
          Nil
        }
        True -> {
          process.send(subject, Cleanup(ack))
          let selector =
            process.new_selector()
            |> process.select(ack)
            |> process.select_specific_monitor(monitor, fn(_) { Nil })
          let _ = process.selector_receive(selector, within: 1000)
          process.demonitor_process(monitor)
          Nil
        }
      }
    }
  }
}

pub fn error_message(error: SignalError) -> String {
  case error {
    SignalServerUnavailable(reason) -> reason
    InstallFailed(reason) -> reason
    HandlerVerificationFailed(reason) -> reason
    UnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
  }
}

fn raw_signal_error(function: String, message: String) -> SignalError {
  case string_contains(message, "erl_signal_server unavailable") {
    True -> SignalServerUnavailable(message)
    False ->
      case string_contains(message, "remained installed") {
        True -> HandlerVerificationFailed(message)
        False ->
          case string_contains(message, "unexpected_ffi_failure:") {
            True -> UnexpectedFfiFailure(function, message)
            False -> InstallFailed(message)
          }
      }
  }
}

fn string_contains(value: String, needle: String) -> Bool {
  string.contains(value, needle)
}

fn cleanup_loop(
  subject: process.Subject(CleanupMessage),
  handle: handle,
  ffi_cleanup: fn(handle) -> Nil,
) -> Nil {
  case process.receive_forever(subject) {
    Cleanup(ack) -> {
      ffi_cleanup(handle)
      process.send(ack, Nil)
      Nil
    }
  }
}

@external(erlang, "scherzo_signal_ffi", "install_sigterm")
fn ffi_install_sigterm(
  subject: process.Subject(lifecycle.StopReason),
) -> Result(#(SignalHandle, String), String)

@external(erlang, "scherzo_signal_ffi", "cleanup_sigterm")
fn ffi_cleanup_sigterm(handle: SignalHandle) -> Nil
