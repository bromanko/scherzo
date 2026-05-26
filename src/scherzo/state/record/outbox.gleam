import gleam/json

pub const context_name = "outbox"

pub fn pending_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  dedupe_key: String,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("dedupe_key", json.string(dedupe_key)),
  ]
}

pub fn pending_v2_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("dedupe_key", json.string(dedupe_key)),
    #("payload_json", json.string(payload_json)),
  ]
}

pub fn completed_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
  ]
}

pub fn failed_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error_code: String,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("error_code", json.string(error_code)),
  ]
}
