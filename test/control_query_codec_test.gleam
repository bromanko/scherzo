import gleam/dynamic/decode
import gleam/json
import gleam/string
import scherzo/control/query/codec
import scherzo/control/query/cursor
import scherzo/control/query/dto
import scherzo/control/query/types

pub fn status_query_request_roundtrip_test() {
  let encoded = codec.request_to_string(types.Status)

  assert string.contains(encoded, "\"type\":\"status\"")
  let assert Ok(decoded) = codec.decode_request(encoded)
  assert decoded == types.Status
}

pub fn status_query_response_roundtrip_test() {
  let response =
    types.default_status_source(daemon_id: "daemon-1", boot_id: "boot-1")
    |> dto.status_from_source
    |> types.StatusResponse

  let encoded = codec.response_to_string(response)

  assert string.contains(encoded, "\"ok\":true")
  let assert Ok(decoded) = codec.decode_response(encoded)
  assert decoded == response
}

pub fn shared_query_codec_decodes_nested_local_and_remote_payloads_test() {
  let request = codec.request_to_string(types.Status)
  let local_line = "{\"query\":" <> request <> "}"
  let remote_line = "{\"payload\":" <> request <> "}"

  let assert Ok(local_dynamic) = nested_field(local_line, "query")
  let assert Ok(remote_dynamic) = nested_field(remote_line, "payload")

  assert codec.decode_request_dynamic(local_dynamic) == Ok(types.Status)
  assert codec.decode_request_dynamic(remote_dynamic) == Ok(types.Status)
}

pub fn cursor_encode_decode_roundtrip_test() {
  let encoded = cursor.encode_offset(42)

  assert encoded == "cursor:42"
  assert cursor.decode_offset(encoded) == Ok(42)
}

pub fn invalid_cursor_maps_to_safe_query_error_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    cursor.decode_offset("cursor:-1")

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
}

pub fn query_error_json_uses_stable_codes_test() {
  let encoded =
    codec.error_to_string(types.QueryError(
      types.QueryOverloaded,
      "query workers are busy",
    ))

  assert string.contains(encoded, "\"ok\":false")
  assert string.contains(encoded, "\"code\":\"query_overloaded\"")

  let assert Error(types.QueryError(code: code, message: message)) =
    codec.decode_response(encoded)
  assert code == types.QueryOverloaded
  assert message == "query workers are busy"
}

pub fn status_dto_redacts_secret_and_raw_payload_inputs_test() {
  let encoded =
    types.StatusSource(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      dispatch_paused: True,
      ui_server_enabled: True,
      supported_queries: ["status"],
      local_control_token: "local-secret-token",
      enrollment_token: "remote-secret-token",
      tracker_payload: "{\"raw\":true}",
      workflow_internals: ["provider:linear", "workflow:execplan"],
    )
    |> dto.status_from_source
    |> dto.status_to_json
    |> json.to_string

  assert string.contains(encoded, "\"daemon_id\":\"daemon-1\"")
  assert string.contains(encoded, "\"ui_server_enabled\":true")
  assert !string.contains(encoded, "local_control_token")
  assert !string.contains(encoded, "enrollment_token")
  assert !string.contains(encoded, "tracker_payload")
  assert !string.contains(encoded, "workflow_internals")
  assert !string.contains(encoded, "local-secret-token")
  assert !string.contains(encoded, "remote-secret-token")
  assert !string.contains(encoded, "provider:linear")
}

fn nested_field(line: String, field_name: String) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode.run(value, decode.at([field_name], decode.dynamic))
    Error(_) -> Error([])
  }
}
