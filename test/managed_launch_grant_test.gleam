import birl
import gleam/option.{Some}
import scherzo/managed_launch/grant

pub fn managed_launch_grant_decodes_valid_v1_contract_test() {
  let assert Ok(parsed) =
    grant.decode_string(valid_grant_json(), now_ms("2026-07-01T00:00:00Z"))

  assert parsed.launch_id == "launch-1"
  assert parsed.endpoint.base_url == "https://ui.example.test"
  assert parsed.endpoint.websocket_url == "wss://ui.example.test/api/daemons/ws"
  assert parsed.credential == "launch-secret"
  assert parsed.daemon_label == Some("Workspace MacBook")
  assert parsed.capabilities == [grant.State, grant.Query, grant.Command]
  assert parsed.command_bridge_enabled
  assert parsed.expires_at_ms == now_ms("2026-07-01T00:10:00Z")
}

pub fn managed_launch_grant_rejects_missing_launch_id_test() {
  let assert Error(error) =
    grant.decode_string(
      "{\"version\":1,\"endpoint\":\"https://ui.example.test\",\"credential\":\"launch-secret\",\"capabilities\":[\"state\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2026-07-01T00:10:00Z\"}",
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "invalid_grant_json"
}

pub fn managed_launch_grant_rejects_invalid_endpoint_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("endpoint", "\"ftp://ui.example.test\"")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "invalid_grant_endpoint"
}

pub fn managed_launch_grant_rejects_empty_credential_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("credential", "\"   \"")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "empty_grant_credential"
}

pub fn managed_launch_grant_rejects_unsupported_capability_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("capabilities", "[\"state\",\"admin\"]")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "unsupported_grant_capability"
  assert grant.error_message(error)
    == "grant capability is not supported: admin"
}

pub fn managed_launch_grant_requires_state_capability_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("capabilities", "[\"query\",\"command\"]")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "missing_state_capability"
}

pub fn managed_launch_grant_rejects_malformed_expiry_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("expiresAt", "\"tomorrow\"")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "invalid_grant_expiry"
}

pub fn managed_launch_grant_rejects_expired_grant_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("expiresAt", "\"2026-06-30T23:59:59Z\"")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "grant_expired"
}

pub fn managed_launch_grant_rejects_daemon_identity_override_test() {
  let assert Error(error) =
    grant.decode_string(
      valid_grant_json_with(#("daemonId", "\"daemon_override\"")),
      now_ms("2026-07-01T00:00:00Z"),
    )

  assert grant.error_code(error) == "daemon_identity_override_rejected"
}

pub fn managed_launch_grant_lists_supported_capabilities_test() {
  assert grant.capabilities_to_strings([grant.State, grant.Query, grant.Command])
    == ["state", "query", "command"]
}

fn valid_grant_json() -> String {
  valid_grant_json_with(#("unused", "unused"))
}

fn valid_grant_json_with(override: #(String, String)) -> String {
  let #(key, value) = override
  let base = [
    #("version", "1"),
    #("launchId", "\"launch-1\""),
    #("endpoint", "\"https://ui.example.test\""),
    #("credential", "\"launch-secret\""),
    #("daemonLabel", "\"Workspace MacBook\""),
    #("capabilities", "[\"state\",\"query\",\"command\"]"),
    #("commandBridgeEnabled", "true"),
    #("expiresAt", "\"2026-07-01T00:10:00Z\""),
  ]

  let pairs = case key == "unused" {
    True -> base
    False -> replace_pair(base, key, value)
  }

  "{" <> join_pairs(pairs) <> "}"
}

fn replace_pair(
  pairs: List(#(String, String)),
  key: String,
  value: String,
) -> List(#(String, String)) {
  case pairs {
    [] -> [#(key, value)]
    [pair, ..rest] -> {
      let #(pair_key, _) = pair
      case pair_key == key {
        True -> [#(key, value), ..rest]
        False -> [pair, ..replace_pair(rest, key, value)]
      }
    }
  }
}

fn join_pairs(pairs: List(#(String, String))) -> String {
  case pairs {
    [] -> ""
    [#(key, value)] -> "\"" <> key <> "\":" <> value
    [#(key, value), ..rest] ->
      "\"" <> key <> "\":" <> value <> "," <> join_pairs(rest)
  }
}

fn now_ms(iso: String) -> Int {
  let assert Ok(time) = birl.parse(iso)
  birl.to_unix_milli(time)
}
