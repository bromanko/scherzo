import gleam/json
import gleam/option.{type Option, None, Some}

pub const context_name = "issue_recovery"

pub fn retry_scheduled_entries(
  issue_id: String,
  issue_identifier: String,
  delay_ms: Int,
  generation: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("delay_ms", json.int(delay_ms)),
    #("generation", json.int(generation)),
    #("reason", json.string(reason)),
  ]
}

pub fn retry_cancelled_entries(
  issue_id: String,
  generation: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("generation", json.int(generation)),
    #("reason", json.string(reason)),
  ]
}

pub fn issue_counter_entries(
  issue_id: String,
  issue_identifier: String,
  failure_attempts: Int,
  worker_sessions: Int,
  observed_updated_at_ms: Int,
  source_run_id: Option(String),
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("failure_attempts", json.int(failure_attempts)),
    #("worker_sessions", json.int(worker_sessions)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    #("source_run_id", option_string(source_run_id)),
  ]
}

pub fn known_workspace_entries(
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("workspace_path", json.string(workspace_path)),
  ]
}

pub fn issue_parked_entries(
  issue_id: String,
  issue_identifier: String,
  reason: String,
  observed_updated_at_ms: Int,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
  ]
}

pub fn issue_parked_v2_entries(
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
    #("release_policy", json.string(release_policy)),
    #("issue_fingerprint", json.string(issue_fingerprint)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
  ]
}

pub fn issue_unparked_entries(
  issue_id: String,
  issue_identifier: String,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
  ]
}

fn option_string(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}
