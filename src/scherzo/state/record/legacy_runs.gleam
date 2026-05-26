import gleam/json
import gleam/option.{type Option}
import gleam/result

pub const context_name = "legacy_runs"

pub type Fields {
  Fields(
    run_id: Option(String),
    issue_id: Option(String),
    issue_identifier: Option(String),
    workspace_path: Option(String),
    classification: Option(String),
    token_total: Option(Int),
    turns: Option(Int),
    reason: Option(String),
  )
}

pub type DecodedBody {
  RunStartedBody(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
  RunFinishedBody(
    run_id: String,
    issue_id: String,
    classification: String,
    token_total: Int,
    turns: Int,
  )
  RunInterruptedBody(run_id: String, issue_id: String, reason: String)
}

pub fn run_started_entries(
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("workspace_path", json.string(workspace_path)),
  ]
}

pub fn run_finished_entries(
  run_id: String,
  issue_id: String,
  classification: String,
  token_total: Int,
  turns: Int,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("issue_id", json.string(issue_id)),
    #("classification", json.string(classification)),
    #("token_total", json.int(token_total)),
    #("turns", json.int(turns)),
  ]
}

pub fn run_interrupted_entries(
  run_id: String,
  issue_id: String,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("issue_id", json.string(issue_id)),
    #("reason", json.string(reason)),
  ]
}

pub fn decode(
  kind: String,
  fields: Fields,
  required_string: fn(Option(String), String) -> Result(String, error),
  required_int: fn(Option(Int), String) -> Result(Int, error),
  unknown_kind: fn(String) -> error,
) -> Result(DecodedBody, error) {
  case kind {
    "run_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      Ok(RunStartedBody(run_id, issue_id, issue_identifier, workspace_path))
    }
    "run_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use classification <- result.try(required_string(
        fields.classification,
        "classification",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      Ok(RunFinishedBody(run_id, issue_id, classification, token_total, turns))
    }
    "run_interrupted" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RunInterruptedBody(run_id, issue_id, reason))
    }
    _ -> Error(unknown_kind(kind))
  }
}
