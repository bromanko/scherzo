import gleam/dynamic/decode
import gleam/json
import gleam/string
import scherzo/control/query/codec
import scherzo/control/query/cursor
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/session/tokens as session_tokens

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

pub fn metrics_query_request_roundtrip_test() {
  let encoded = codec.request_to_string(types.Metrics)

  assert string.contains(encoded, "\"type\":\"metrics\"")
  let assert Ok(decoded) = codec.decode_request(encoded)
  assert decoded == types.Metrics
}

pub fn metrics_query_response_roundtrip_test() {
  let response =
    types.default_operational_metrics_source(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
    )
    |> dto.operational_metrics_from_source
    |> types.MetricsResponse

  let encoded = codec.response_to_string(response)

  assert string.contains(encoded, "\"ok\":true")
  assert string.contains(encoded, "\"type\":\"metrics\"")
  assert string.contains(encoded, "\"schema_version\":1")
  let assert Ok(decoded) = codec.decode_response(encoded)
  assert decoded == response
}

pub fn metrics_query_response_rejects_unknown_schema_version_test() {
  let response =
    types.default_operational_metrics_source(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
    )
    |> dto.operational_metrics_from_source
    |> types.MetricsResponse
  let encoded =
    codec.response_to_string(response)
    |> string.replace(
      each: "\"schema_version\":1",
      with: "\"schema_version\":2",
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    codec.decode_response(encoded)
  assert code == types.QueryBackendFailed
  assert message == "unsupported metrics schema version"
}

pub fn shared_query_codec_decodes_nested_local_and_remote_payloads_test() {
  let request = codec.request_to_string(types.Status)
  let local_line = "{\"query\":" <> request <> "}"
  let remote_line = "{\"payload\":" <> request <> "}"

  let assert Ok(local_dynamic) = nested_field(local_line, "query")
  let assert Ok(remote_dynamic) = nested_field(remote_line, "payload")

  assert codec.decode_request_dynamic(local_dynamic) == Ok(types.Status)
  assert codec.decode_request_dynamic(remote_dynamic) == Ok(types.Status)

  let metrics_request = codec.request_to_string(types.Metrics)
  let metrics_line = "{\"payload\":" <> metrics_request <> "}"
  let assert Ok(metrics_dynamic) = nested_field(metrics_line, "payload")
  assert codec.decode_request_dynamic(metrics_dynamic) == Ok(types.Metrics)
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

pub fn metrics_dto_uses_narrow_non_secret_source_test() {
  let encoded =
    types.OperationalMetricsSource(
      ..types.default_operational_metrics_source(
        daemon_id: "daemon-1",
        boot_id: "boot-1",
      ),
      sampled_at_ms: 123,
      dispatch_paused: True,
      ui_server_enabled: True,
      remote_client_status: "connected",
      workflow_count: 2,
      active_sessions: 1,
      aggregate_tokens: session_tokens.TokenTotals(
        input: 10,
        output: 20,
        cache_read: 3,
        cache_write: 4,
        total: 37,
      ),
    )
    |> dto.operational_metrics_from_source
    |> dto.operational_metrics_to_json
    |> json.to_string

  assert string.contains(encoded, "\"daemon_id\":\"daemon-1\"")
  assert string.contains(encoded, "\"schema_version\":1")
  assert string.contains(encoded, "\"active_sessions\":1")
  assert string.contains(encoded, "\"total\":37")
  assert !string.contains(encoded, "local_control_token")
  assert !string.contains(encoded, "enrollment_token")
  assert !string.contains(encoded, "tracker_payload")
  assert !string.contains(encoded, "workflow_internals")
  assert !string.contains(encoded, "api_key")
  assert !string.contains(encoded, "provider:linear")
  assert !string.contains(encoded, "raw failure payload")
}

fn nested_field(line: String, field_name: String) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode.run(value, decode.at([field_name], decode.dynamic))
    Error(_) -> Error([])
  }
}
