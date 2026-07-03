import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

pub type Phase {
  CliBootstrap
  ControlFileDiscovery
  DaemonConnect
  RequestRoundTrip
  DaemonActorQuery
  OperationAdmission
  OperationWait
  CommandStepWatchdog
  ExternalApi
}

pub type Accepted {
  AcceptedTrue
  AcceptedFalse
  AcceptedUnknown
}

pub type TimeoutError {
  TimeoutError(
    phase: Phase,
    timeout_ms: Int,
    accepted: Accepted,
    retryable: Bool,
    message: String,
    suggested_next_command: Option(String),
  )
}

pub fn phase_string(phase: Phase) -> String {
  case phase {
    CliBootstrap -> "cli_bootstrap"
    ControlFileDiscovery -> "control_file_discovery"
    DaemonConnect -> "daemon_connect"
    RequestRoundTrip -> "request_round_trip"
    DaemonActorQuery -> "daemon_actor_query"
    OperationAdmission -> "operation_admission"
    OperationWait -> "operation_wait"
    CommandStepWatchdog -> "command_step_watchdog"
    ExternalApi -> "external_api"
  }
}

pub fn accepted_json(accepted: Accepted) -> json.Json {
  case accepted {
    AcceptedTrue -> json.bool(True)
    AcceptedFalse -> json.bool(False)
    AcceptedUnknown -> json.string("unknown")
  }
}

pub fn retryable_string(retryable: Bool) -> String {
  case retryable {
    True -> "yes"
    False -> "no"
  }
}

pub fn error_json(error: TimeoutError) -> json.Json {
  let TimeoutError(
    phase: phase,
    timeout_ms: timeout_ms,
    accepted: accepted,
    retryable: retryable,
    message: message,
    suggested_next_command: suggested,
  ) = error
  let base = [
    #("code", json.string("timeout")),
    #("phase", json.string(phase_string(phase))),
    #("timeout_ms", json.int(timeout_ms)),
    #("accepted", accepted_json(accepted)),
    #("retryable", json.bool(retryable)),
    #("message", json.string(message)),
  ]
  case suggested {
    Some(command) -> [#("suggested_next_command", json.string(command)), ..base]
    None -> base
  }
  |> list.reverse
  |> json.object
}

pub fn error_lines(error: TimeoutError) -> List(String) {
  let TimeoutError(
    phase: phase,
    accepted: accepted,
    retryable: retryable,
    message: message,
    suggested_next_command: suggested,
    ..,
  ) = error
  let accepted_line = case accepted {
    AcceptedTrue -> "Accepted: true"
    AcceptedFalse -> "Accepted: false"
    AcceptedUnknown -> "Accepted: unknown"
  }
  let next_line = case suggested {
    Some(command) -> ["Next: " <> command]
    None -> []
  }
  [
    message,
    "Phase: " <> phase_string(phase),
    accepted_line,
    "Retryable: " <> retryable_string(retryable),
  ]
  |> list.append(next_line)
}
