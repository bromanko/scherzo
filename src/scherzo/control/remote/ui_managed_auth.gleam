import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/control/remote/ui_protocol
import scherzo/managed_launch/grant as managed_launch_grant

pub type ManagedLaunchAuth {
  ManagedLaunchAuth(
    launch_credential: Option(String),
    launch_expires_at_ms: Int,
    runtime_credential: Option(String),
  )
}

pub fn credential_for_connect(
  auth: Option(ManagedLaunchAuth),
  durable_credential: String,
  now_ms: Int,
) -> #(Option(ManagedLaunchAuth), Result(String, String)) {
  case auth {
    None -> #(None, Ok(durable_credential))
    Some(auth) -> {
      let auth = prune_launch_credential(auth, now_ms)
      case auth.runtime_credential {
        Some(credential) -> #(Some(auth), Ok(credential))
        None ->
          case auth.launch_credential {
            Some(credential) -> #(Some(auth), Ok(credential))
            None -> #(
              Some(auth),
              Error(
                "managed launch grant expired before runtime credential exchange completed",
              ),
            )
          }
      }
    }
  }
}

pub fn prune_expired_launch_credential(
  auth: Option(ManagedLaunchAuth),
  now_ms: Int,
) -> Option(ManagedLaunchAuth) {
  case auth {
    Some(auth) -> Some(prune_launch_credential(auth, now_ms))
    None -> None
  }
}

pub fn capture_runtime_credential(
  auth: Option(ManagedLaunchAuth),
  runtime_credential: Option(String),
  now_ms: Int,
) -> Option(ManagedLaunchAuth) {
  case auth, runtime_credential {
    Some(auth), Some(runtime_credential) ->
      Some(
        ManagedLaunchAuth(..auth, runtime_credential: Some(runtime_credential))
        |> prune_launch_credential(now_ms),
      )
    _, _ -> auth
  }
}

pub fn redaction_secrets(
  auth: Option(ManagedLaunchAuth),
  durable_credential: String,
  base_secrets: List(String),
) -> List(String) {
  case auth {
    Some(auth) -> list.append(auth_secrets(auth), base_secrets)
    None -> [durable_credential, ..base_secrets]
  }
}

pub fn is_managed_launch(auth: Option(ManagedLaunchAuth)) -> Bool {
  case auth {
    Some(_) -> True
    None -> False
  }
}

pub fn is_permanent_auth_rejection(message: String) -> Bool {
  let normalized = message |> string.lowercase
  case websocket_http_status_code(normalized) {
    Some(code) -> code == "401" || code == "403"
    None ->
      case websocket_close_reason(normalized) {
        Some(#(code, reason)) ->
          code == "1008" && contains_auth_rejection_marker(reason)
        None -> contains_auth_rejection_marker(normalized)
      }
  }
}

fn websocket_http_status_code(message: String) -> Option(String) {
  case string.starts_with(message, "websocket_http_status:") {
    True -> {
      let rest =
        string.drop_start(message, string.length("websocket_http_status:"))
      case string.split_once(rest, on: ":") {
        Ok(#(code, _)) -> Some(code)
        Error(Nil) -> Some(rest)
      }
    }
    False -> None
  }
}

fn websocket_close_reason(message: String) -> Option(#(String, String)) {
  case string.starts_with(message, "websocket_close:") {
    True -> {
      let rest = string.drop_start(message, string.length("websocket_close:"))
      case string.split_once(rest, on: ":") {
        Ok(#(code, reason)) -> Some(#(code, reason))
        Error(Nil) -> Some(#(rest, ""))
      }
    }
    False -> None
  }
}

fn contains_auth_rejection_marker(message: String) -> Bool {
  string.contains(message, "managed launch grant revoked")
  || string.contains(message, "managed launch hello mismatch")
  || string.contains(message, "managed launch credential rejected")
  || string.contains(message, "credential-invalid")
  || string.contains(message, "credential_invalid")
  || string.contains(message, "credential invalid")
  || string.contains(message, "invalid credential")
  || string.contains(message, "credential revoked")
  || string.contains(message, "daemon identity revoked")
}

pub fn rejection_status_message() -> String {
  "managed launch credential is unavailable or rejected; Core will exit for supervised relaunch"
}

pub fn command_denied(
  metadata: ui_protocol.RuntimeMetadata,
  operator_command: command.OperatorCommand,
) -> Option(command.CommandResult) {
  case ui_protocol.runtime_managed_launch_context(metadata) {
    Some(context) ->
      case list.contains(context.capabilities, managed_launch_grant.Command) {
        True -> None
        False ->
          Some(command.not_allowed(
            operator_command,
            "managed_launch_command_capability_denied",
            Some("managed launch grant does not allow remote commands"),
          ))
      }
    None -> None
  }
}

pub fn command_without_apply_result(
  metadata: ui_protocol.RuntimeMetadata,
  command_bridge_enabled: Bool,
  expected_daemon_id: String,
  expected_boot_id: String,
  operator_command: command.OperatorCommand,
  daemon_id: String,
  boot_id: String,
) -> Option(command.CommandResult) {
  case command_denied(metadata, operator_command) {
    Some(result) -> Some(result)
    None ->
      case command_bridge_enabled {
        False ->
          Some(command.not_allowed(
            operator_command,
            "command_bridge_disabled",
            Some("remote command bridge is disabled"),
          ))
        True ->
          case daemon_id == expected_daemon_id, boot_id == expected_boot_id {
            False, _ ->
              Some(command.not_allowed(
                operator_command,
                "daemon_id_mismatch",
                Some("server command daemonId does not match this daemon"),
              ))
            _, False ->
              Some(command.not_allowed(
                operator_command,
                "boot_id_mismatch",
                Some("server command bootId does not match this daemon boot"),
              ))
            True, True -> None
          }
      }
  }
}

pub fn query_denied(
  metadata: ui_protocol.RuntimeMetadata,
) -> Option(query_types.QueryError) {
  case ui_protocol.runtime_managed_launch_context(metadata) {
    Some(context) ->
      case list.contains(context.capabilities, managed_launch_grant.Query) {
        True -> None
        False ->
          Some(query_types.QueryError(
            query_types.UnsupportedQuery,
            "managed launch grant does not allow remote queries",
          ))
      }
    None -> None
  }
}

pub fn query_without_execute_error(
  metadata: ui_protocol.RuntimeMetadata,
  expected_daemon_id: String,
  expected_boot_id: String,
  daemon_id: String,
  boot_id: String,
) -> Option(query_types.QueryError) {
  case query_denied(metadata) {
    Some(error) -> Some(error)
    None ->
      case daemon_id == expected_daemon_id, boot_id == expected_boot_id {
        False, _ ->
          Some(query_types.QueryError(
            query_types.QueryBackendFailed,
            "query_request daemonId does not match this daemon",
          ))
        _, False ->
          Some(query_types.QueryError(
            query_types.QueryBackendFailed,
            "query_request bootId does not match this daemon boot",
          ))
        True, True -> None
      }
  }
}

pub fn update_known_dispatch_paused(
  current: Bool,
  operator_command: command.OperatorCommand,
  result: command.CommandResult,
) -> Bool {
  case result.status {
    command.Applied ->
      case operator_command {
        command.PauseDispatch -> True
        command.ResumeDispatch -> False
        _ -> current
      }
    _ -> current
  }
}

fn prune_launch_credential(
  auth: ManagedLaunchAuth,
  now_ms: Int,
) -> ManagedLaunchAuth {
  case now_ms >= auth.launch_expires_at_ms {
    True -> ManagedLaunchAuth(..auth, launch_credential: None)
    False -> auth
  }
}

fn auth_secrets(auth: ManagedLaunchAuth) -> List(String) {
  let secrets = case auth.runtime_credential {
    Some(credential) -> [credential]
    None -> []
  }
  case auth.launch_credential {
    Some(credential) -> [credential, ..secrets]
    None -> secrets
  }
}
