import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

pub const context_name = "steps"

pub fn prepared_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  workspace_path: String,
  run_root: String,
  source_workspace_name: Option(String),
  source_workspace_path: Option(String),
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("workspace_name", json.string(workspace_name)),
    #("workspace_path", json.string(workspace_path)),
    #("run_root", json.string(run_root)),
    #("source_workspace_name", optional_string(source_workspace_name)),
    #("source_workspace_path", optional_string(source_workspace_path)),
  ]
}

pub fn started_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  operator_session_id: String,
  external_session_ref: Option(String),
  continuation_capable: Bool,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("operator_session_id", json.string(operator_session_id)),
    #("external_session_ref", optional_string(external_session_ref)),
    #("continuation_capable", json.bool(continuation_capable)),
  ]
}

pub fn continuation_started_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  session_id: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("session_id", json.string(session_id)),
  ]
}

pub fn pi_session_recorded_entries(
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  workflow_id: String,
  workflow_fingerprint: String,
  step_id: String,
  workspace_name: String,
  attempt_index: Int,
  workspace_path: String,
  session_id: String,
  session_file: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("workflow_id", json.string(workflow_id)),
    #("workflow_fingerprint", json.string(workflow_fingerprint)),
    #("step_id", json.string(step_id)),
    #("workspace_name", json.string(workspace_name)),
    #("attempt_index", json.int(attempt_index)),
    #("workspace_path", json.string(workspace_path)),
    #("session_id", json.string(session_id)),
    #("session_file", json.string(session_file)),
  ]
}

pub fn pi_session_recorded_with_task_entries(
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  task_ref_entries: List(#(String, json.Json)),
  workflow_id: String,
  workflow_fingerprint: String,
  step_id: String,
  workspace_name: String,
  attempt_index: Int,
  workspace_path: String,
  session_id: String,
  session_file: String,
) -> List(#(String, json.Json)) {
  list.append(
    [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
    ],
    list.append(task_ref_entries, [
      #("workflow_id", json.string(workflow_id)),
      #("workflow_fingerprint", json.string(workflow_fingerprint)),
      #("step_id", json.string(step_id)),
      #("workspace_name", json.string(workspace_name)),
      #("attempt_index", json.int(attempt_index)),
      #("workspace_path", json.string(workspace_path)),
      #("session_id", json.string(session_id)),
      #("session_file", json.string(session_file)),
    ]),
  )
}

pub fn finished_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  outcome: String,
  artifact_ref: String,
  artifact_sha256: String,
  workspace_name: String,
  workspace_path: String,
  token_total: Int,
  turns: Int,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("outcome", json.string(outcome)),
    #("artifact_ref", json.string(artifact_ref)),
    #("artifact_sha256", json.string(artifact_sha256)),
    #("workspace_name", json.string(workspace_name)),
    #("workspace_path", json.string(workspace_path)),
    #("token_total", json.int(token_total)),
    #("turns", json.int(turns)),
  ]
}

pub fn recovery_started_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  model: Option(String),
  prompt_ref: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("failed_attempt_index", json.int(failed_attempt_index)),
    #("recovery_attempt_number", json.int(recovery_attempt_number)),
    #("recovery_session_id", json.string(recovery_session_id)),
    #("model", optional_string(model)),
    #("prompt_ref", json.string(prompt_ref)),
  ]
}

pub fn recovery_finished_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  result: String,
  summary: String,
  reason: String,
  retry_attempt_index: Option(Int),
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("failed_attempt_index", json.int(failed_attempt_index)),
    #("recovery_attempt_number", json.int(recovery_attempt_number)),
    #("recovery_session_id", json.string(recovery_session_id)),
    #("result", json.string(result)),
    #("summary", json.string(summary)),
    #("reason", json.string(reason)),
    #("retry_attempt_index", optional_int(retry_attempt_index)),
  ]
}

pub fn interrupted_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("reason", json.string(reason)),
  ]
}

pub fn superseded_entries(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  superseded_by_attempt_index: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #("superseded_by_attempt_index", json.int(superseded_by_attempt_index)),
    #("reason", json.string(reason)),
  ]
}

pub fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}

fn optional_int(value: Option(Int)) -> json.Json {
  case value {
    Some(inner) -> json.int(inner)
    None -> json.null()
  }
}
