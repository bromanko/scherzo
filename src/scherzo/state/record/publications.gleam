import gleam/json
import gleam/option.{type Option, None, Some}

pub const context_name = "publications"

pub fn attempt_recorded_entries(
  run_id: String,
  workflow_id: String,
  publication_id: String,
  series_id: String,
  attempt_id: String,
  status: String,
  required: Bool,
  retryable: Bool,
  retry_execution_available: Bool,
  version_id: Option(String),
  manifest_ref: Option(String),
  manifest_sha256: Option(String),
  manifest_bytes: Option(Int),
  error_code: Option(String),
  error_message: Option(String),
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("publication_id", json.string(publication_id)),
    #("series_id", json.string(series_id)),
    #("attempt_id", json.string(attempt_id)),
    #("status", json.string(status)),
    #("required", json.bool(required)),
    #("retryable", json.bool(retryable)),
    #("retry_execution_available", json.bool(retry_execution_available)),
    #("version_id", optional_string(version_id)),
    #("manifest_ref", optional_string(manifest_ref)),
    #("manifest_sha256", optional_string(manifest_sha256)),
    #("manifest_bytes", optional_int(manifest_bytes)),
    #("error_code", optional_string(error_code)),
    #("error_message", optional_string(error_message)),
  ]
}

fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn optional_int(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
