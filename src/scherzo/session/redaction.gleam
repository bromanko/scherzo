import scherzo/session/event

pub const max_raw_json_bytes = 16_384

pub const redaction_failure_placeholder = "[unavailable redaction failed]"

pub fn redact_raw_json(
  raw: String,
  secrets: List(String),
) -> event.RedactedRawJson {
  let #(value, truncated) =
    redact_raw_json_fail_closed_ffi(
      raw,
      secrets,
      max_raw_json_bytes,
      redaction_failure_placeholder,
    )
  event.RedactedRawJson(value: value, truncated: truncated)
}

@external(erlang, "scherzo_redaction_ffi", "redact_raw_json_fail_closed")
fn redact_raw_json_fail_closed_ffi(
  raw: String,
  secrets: List(String),
  max_bytes: Int,
  failure_placeholder: String,
) -> #(String, Bool)
