import gleam/erlang/process
import scherzo/log

pub type StopReason {
  Sigterm
  ManagedLaunchCredentialRejected(String)
  TestStop(String)
}

pub type ShutdownResult {
  ShutdownComplete
  ShutdownTimedOut
}

pub type ShutdownReport {
  ShutdownReport(result: ShutdownResult, reason: StopReason)
}

pub fn run_until_stop(
  stop_subject: process.Subject(StopReason),
  shutdown: fn(StopReason) -> Result(Nil, Nil),
  cleanup_stop_source: fn() -> Nil,
  release: fn() -> Nil,
  logger: fn(String, String, List(log.Field)) -> Nil,
) -> ShutdownResult {
  let report =
    run_until_stop_with_reason(
      stop_subject,
      shutdown,
      cleanup_stop_source,
      release,
      logger,
    )
  report.result
}

pub fn run_until_stop_with_reason(
  stop_subject: process.Subject(StopReason),
  shutdown: fn(StopReason) -> Result(Nil, Nil),
  cleanup_stop_source: fn() -> Nil,
  release: fn() -> Nil,
  logger: fn(String, String, List(log.Field)) -> Nil,
) -> ShutdownReport {
  let reason = process.receive_forever(stop_subject)
  logger("info", "daemon_stop_requested", [
    #("reason", reason_to_string(reason)),
  ])
  let result = case safe_shutdown(shutdown, reason) {
    Ok(Nil) -> {
      logger("info", "daemon_shutdown_complete", [])
      ShutdownComplete
    }
    Error(Nil) -> {
      logger("error", "daemon_shutdown_timeout", [])
      ShutdownTimedOut
    }
  }
  cleanup_stop_source()
  release()
  ShutdownReport(result: result, reason: reason)
}

@external(erlang, "scherzo_lifecycle_ffi", "safe_shutdown")
fn safe_shutdown(
  shutdown: fn(StopReason) -> Result(Nil, Nil),
  reason: StopReason,
) -> Result(Nil, Nil)

pub fn reason_to_string(reason: StopReason) -> String {
  case reason {
    Sigterm -> "sigterm"
    ManagedLaunchCredentialRejected(_) -> "managed_launch_credential_rejected"
    TestStop(reason) -> reason
  }
}
