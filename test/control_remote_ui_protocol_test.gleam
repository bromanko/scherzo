import gleam/option.{Some}
import gleam/string
import scherzo/control/remote/ui_protocol

pub fn ui_protocol_encodes_daemon_messages_test() {
  let hello =
    ui_protocol.encode_client_message(ui_protocol.DaemonHello(
      "daemon_abc",
      "boot_abc",
    ))
  assert string.contains(hello, "daemon_hello")
  assert string.contains(hello, "daemonId")

  let state =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(42, False, [
        ui_protocol.SessionSnapshot(
          "session-1",
          "Demo",
          "LIV-1",
          "running",
          3,
          99,
        ),
      ]),
    )
  assert string.contains(state, "daemon_state")
  assert string.contains(state, "dispatchPaused")
  assert string.contains(state, "sessionId")
}

pub fn ui_protocol_decodes_server_messages_test() {
  let assert Ok(ui_protocol.ServerHello(Some(1500))) =
    ui_protocol.decode_server_message(
      "{\"type\":\"server_hello\",\"heartbeatIntervalMs\":1500}",
    )
  let assert Ok(ui_protocol.CredentialRevoked(reason)) =
    ui_protocol.decode_server_message(
      "{\"type\":\"credential_revoked\",\"reason\":\"revoked\"}",
    )
  assert reason == "revoked"

  let assert Ok(ui_protocol.DaemonIdentityRevoked(identity_reason)) =
    ui_protocol.decode_server_message(
      "{\"type\":\"daemon_identity_revoked\",\"reason\":\"identity revoked\"}",
    )
  assert identity_reason == "identity revoked"
}
