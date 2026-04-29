import gleam/erlang/process
import scherzo/lifecycle

type CleanupMessage {
  Cleanup(process.Subject(Nil))
}

pub type SignalHandle

pub type Installation {
  Installation(
    cleanup: fn() -> Nil,
    installed_signals: List(lifecycle.StopReason),
    os_pid: String,
  )
}

pub fn install(
  subject: process.Subject(lifecycle.StopReason),
) -> Result(Installation, String) {
  install_with_ffi(subject, ffi_install_sigterm, ffi_cleanup_sigterm)
}

pub fn install_with_ffi(
  subject: process.Subject(lifecycle.StopReason),
  ffi_install: fn(process.Subject(lifecycle.StopReason)) ->
    Result(#(handle, String), String),
  ffi_cleanup: fn(handle) -> Nil,
) -> Result(Installation, String) {
  case ffi_install(subject) {
    Error(message) -> Error(message)
    Ok(#(handle, os_pid)) -> {
      let cleanup_subject = start_cleanup_server(handle, ffi_cleanup)
      Ok(Installation(
        cleanup: fn() { cleanup_once(cleanup_subject) },
        installed_signals: [lifecycle.Sigterm],
        os_pid: os_pid,
      ))
    }
  }
}

fn start_cleanup_server(
  handle: handle,
  ffi_cleanup: fn(handle) -> Nil,
) -> process.Subject(CleanupMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      cleanup_loop(subject, handle, ffi_cleanup)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
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
