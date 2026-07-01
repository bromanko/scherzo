import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/managed_launch/status

pub fn managed_launch_status_round_trips_v1_json_test() {
  let value =
    status.Status(
      launch_id: Some("launch-1"),
      phase: "startup",
      ok: False,
      code: "instance_lock_held",
      message: "existing daemon holds the workspace lock",
      updated_at_ms: 42,
    )

  let assert Ok(decoded) = value |> status.to_string |> status.decode_string
  assert decoded == value
}

pub fn managed_launch_status_redacts_secret_from_json_and_log_fields_test() {
  let secret = "launch-secret"
  let value =
    status.Status(
      launch_id: Some("launch-1"),
      phase: "startup",
      ok: False,
      code: "grant_invalid",
      message: "credential launch-secret was rejected",
      updated_at_ms: 99,
    )

  let redacted_json = status.to_redacted_string(value, [secret])
  assert !string.contains(redacted_json, secret)
  assert string.contains(redacted_json, "[REDACTED]")

  let log_fields = status.to_log_fields(value, [secret])
  let log_line =
    string.join(
      list.map(log_fields, fn(field) {
        let #(key, value) = field
        key <> "=" <> value
      }),
      with: " ",
    )
  assert !string.contains(log_line, secret)
  assert string.contains(log_line, "message=credential [REDACTED] was rejected")
}

pub fn managed_launch_status_rejects_unsupported_version_test() {
  let assert Error(error) =
    status.decode_string(
      "{\"version\":2,\"launchId\":null,\"phase\":\"startup\",\"ok\":false,\"code\":\"grant_invalid\",\"message\":\"bad\",\"updatedAtMs\":1}",
    )

  assert status.error_code(error) == "unsupported_managed_launch_status_version"
}

pub fn managed_launch_status_allows_null_launch_id_test() {
  let assert Ok(decoded) =
    status.decode_string(
      "{\"version\":1,\"launchId\":null,\"phase\":\"startup\",\"ok\":true,\"code\":\"ready\",\"message\":\"ok\",\"updatedAtMs\":1}",
    )

  assert decoded
    == status.Status(
      launch_id: None,
      phase: "startup",
      ok: True,
      code: "ready",
      message: "ok",
      updated_at_ms: 1,
    )
}
