import gleam/json
import gleam/list

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

pub fn pending_v2_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
      #("payload_json", json.string(payload_json)),
    ]),
  )
}

pub fn attempted_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
  attempt_count: Int,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("dedupe_key", json.string(dedupe_key)),
    #("payload_json", json.string(payload_json)),
    #("attempt_count", json.int(attempt_count)),
  ]
}

pub fn attempted_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
  attempt_count: Int,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
      #("payload_json", json.string(payload_json)),
      #("attempt_count", json.int(attempt_count)),
    ]),
  )
}

pub fn retry_scheduled_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
  error_code: String,
  attempt_count: Int,
  next_attempt_at_ms: Int,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("dedupe_key", json.string(dedupe_key)),
    #("payload_json", json.string(payload_json)),
    #("error_code", json.string(error_code)),
    #("attempt_count", json.int(attempt_count)),
    #("next_attempt_at_ms", json.int(next_attempt_at_ms)),
  ]
}

pub fn retry_scheduled_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
  dedupe_key: String,
  payload_json: String,
  error_code: String,
  attempt_count: Int,
  next_attempt_at_ms: Int,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
      #("payload_json", json.string(payload_json)),
      #("error_code", json.string(error_code)),
      #("attempt_count", json.int(attempt_count)),
      #("next_attempt_at_ms", json.int(next_attempt_at_ms)),
    ]),
  )
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

pub fn completed_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [#("outbox_kind", json.string(outbox_kind))]),
  )
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

pub fn failed_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
  error_code: String,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [
      #("outbox_kind", json.string(outbox_kind)),
      #("error_code", json.string(error_code)),
    ]),
  )
}

pub fn permanently_failed_entries(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error_code: String,
  attempt_count: Int,
) -> List(#(String, json.Json)) {
  [
    #("outbox_id", json.string(outbox_id)),
    #("issue_id", json.string(issue_id)),
    #("outbox_kind", json.string(outbox_kind)),
    #("error_code", json.string(error_code)),
    #("attempt_count", json.int(attempt_count)),
  ]
}

pub fn permanently_failed_with_task_entries(
  outbox_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outbox_kind: String,
  error_code: String,
  attempt_count: Int,
) -> List(#(String, json.Json)) {
  list.append(
    [#("outbox_id", json.string(outbox_id))],
    list.append(task_ref_entries, [
      #("outbox_kind", json.string(outbox_kind)),
      #("error_code", json.string(error_code)),
      #("attempt_count", json.int(attempt_count)),
    ]),
  )
}
