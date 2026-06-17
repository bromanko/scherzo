import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/control/remote/ui_protocol

pub fn ui_protocol_encodes_daemon_messages_test() {
  let hello =
    ui_protocol.encode_client_message(ui_protocol.DaemonHello(
      "daemon_abc",
      "boot_abc",
      None,
    ))
  assert string.contains(hello, "daemon_hello")
  assert string.contains(hello, "daemonId")

  let state =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(42, False, None, [
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

pub fn ui_protocol_encodes_daemon_label_metadata_test() {
  let hello =
    ui_protocol.encode_client_message(ui_protocol.DaemonHello(
      "daemon_abc",
      "boot_abc",
      Some("Project Foo / MacBook"),
    ))
  assert string.contains(hello, "\"daemonLabel\":\"Project Foo / MacBook\"")

  let heartbeat =
    ui_protocol.encode_client_message(ui_protocol.Heartbeat(
      42,
      Some("Project Foo / MacBook"),
    ))
  assert string.contains(heartbeat, "\"daemonLabel\":\"Project Foo / MacBook\"")

  let state =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(42, False, Some("Project Foo / MacBook"), []),
    )
  assert string.contains(state, "\"daemonLabel\":\"Project Foo / MacBook\"")
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

pub fn ui_protocol_decodes_work_item_action_server_command_test() {
  let payload =
    "{\"type\":\"server_command\",\"serverCommandId\":\"cmd-1\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"command\":{\"type\":\"work_item_action\",\"action_id\":\"work_subtask.cancel\",\"action_instance_id\":\"wia_1\",\"target_kind\":\"workflow_subtask\",\"target_provider\":\"linear\",\"target_id\":\"issue-1\",\"observed_fingerprint\":\"fp-1\",\"idempotency_key\":\"idem-1\",\"params\":[]}}"

  let assert Ok(ui_protocol.ServerCommand(
    _,
    _,
    _,
    command.WorkItemAction(request),
  )) = ui_protocol.decode_server_message(payload)
  assert request.action_id == "work_subtask.cancel"
  assert request.target_id == "issue-1"
}

pub fn ui_protocol_decodes_query_request_test() {
  let payload =
    "{\"type\":\"query_request\",\"queryId\":\"query-1\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"query\":{\"version\":1,\"type\":\"status\"}}"

  let assert Ok(ui_protocol.QueryRequest(query_id, daemon_id, boot_id, query)) =
    ui_protocol.decode_server_message(payload)

  assert query_id == "query-1"
  assert daemon_id == "daemon_abc"
  assert boot_id == "boot_abc"
  assert query == query_types.Status
}

pub fn ui_protocol_encodes_query_response_test() {
  let ok_payload =
    ui_protocol.encode_query_response(
      "query-ok",
      Ok(
        query_types.StatusResponse(
          query_types.StatusDto(
            daemon_id: "daemon_abc",
            boot_id: "boot_abc",
            dispatch_paused: False,
            ui_server_enabled: True,
            supported_queries: ["status"],
          ),
        ),
      ),
    )
  assert string.contains(ok_payload, "\"type\":\"query_response\"")
  assert string.contains(ok_payload, "\"queryId\":\"query-ok\"")
  assert string.contains(ok_payload, "\"ok\":true")
  assert string.contains(ok_payload, "\"status\":{")

  let error_payload =
    ui_protocol.encode_query_response(
      "query-error",
      Error(query_types.QueryError(query_types.QueryTimeout, "query timed out")),
    )
  assert string.contains(error_payload, "\"type\":\"query_response\"")
  assert string.contains(error_payload, "\"queryId\":\"query-error\"")
  assert string.contains(error_payload, "\"ok\":false")
  assert string.contains(error_payload, "\"code\":\"query_timeout\"")
}

pub fn ui_protocol_rejects_malformed_query_request_with_query_id_test() {
  let payload =
    "{\"type\":\"query_request\",\"queryId\":\"query-bad\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"query\":{\"version\":1,\"type\":\"mystery\"}}"

  let assert Error(ui_protocol.DecodeError(code: code, message: message)) =
    ui_protocol.decode_server_message(payload)
  assert code == "unsupported_query"
  assert string.contains(message, "unsupported query type")

  let assert Ok(#(query_id, query_error)) =
    ui_protocol.decode_query_request_rejection(payload)
  assert query_id == "query-bad"
  assert query_error
    == query_types.QueryError(
      query_types.UnsupportedQuery,
      "unsupported query type: mystery",
    )
}

pub fn ui_protocol_rejects_bad_json_test() {
  let assert Error(ui_protocol.DecodeError(code: code, message: message)) =
    ui_protocol.decode_server_message("{not-json}")
  assert code == "bad_json"
  assert message == "malformed UI websocket JSON"
}
