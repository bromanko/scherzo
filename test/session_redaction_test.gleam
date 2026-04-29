import gleam/option.{Some}
import gleam/string
import scherzo/session/event
import scherzo/session/json as session_json
import scherzo/session/redaction

pub fn raw_json_redaction_removes_sensitive_keys_and_configured_secrets_test() {
  let raw =
    "{\"token\":\"tok-123\",\"nested\":{\"api_key\":\"api-123\",\"authorization\":\"Bearer auth-123\",\"not_secret\":\"contains configured-secret\"},\"items\":[{\"secret\":\"secret-123\"}]}"

  let redacted =
    redaction.redact_raw_json(raw, ["configured-secret", "auth-123"])

  assert string.contains(redacted.value, "[REDACTED]")
  assert !string.contains(redacted.value, "tok-123")
  assert !string.contains(redacted.value, "api-123")
  assert !string.contains(redacted.value, "auth-123")
  assert !string.contains(redacted.value, "secret-123")
  assert !string.contains(redacted.value, "configured-secret")
  assert redacted.truncated == False
}

pub fn raw_json_redaction_truncates_large_payload_test() {
  let large = string.repeat("x", times: redaction.max_raw_json_bytes + 1024)
  let raw = "{\"message\":\"" <> large <> "\"}"

  let redacted = redaction.redact_raw_json(raw, [])

  assert redacted.truncated == True
  assert string.length(redacted.value) <= redaction.max_raw_json_bytes
}

pub fn raw_json_redaction_keeps_multibyte_truncation_json_encodable_test() {
  let multibyte = string.repeat("☃", times: redaction.max_raw_json_bytes)
  let raw = "{\"message\":\"" <> multibyte <> "\"}"

  let redacted = redaction.redact_raw_json(raw, [])

  assert redacted.truncated == True
  let payload =
    event.EventPayload(
      ..event.empty_payload(event.PiRaw, "unknown_raw"),
      raw_json: Some(redacted),
    )
  let encoded = session_json.payload_to_string(payload)
  assert string.contains(encoded, "\"truncated\":true")
  assert string.contains(encoded, "\"raw_json\"")
}

pub fn raw_json_redaction_fails_closed_for_malformed_payload_test() {
  let redacted =
    redaction.redact_raw_json("{\"token\":\"tok-123\"", ["tok-123"])

  assert string.contains(redacted.value, "unavailable")
  assert !string.contains(redacted.value, "tok-123")
}
