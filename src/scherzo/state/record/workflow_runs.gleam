import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

pub const context_name = "workflow_runs"

pub fn started_entries(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  issue_id: String,
  issue_identifier: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  run_root: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("workflow_fingerprint", json.string(workflow_fingerprint)),
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("issue_fingerprint", json.string(issue_fingerprint)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    #("run_root", json.string(run_root)),
  ]
}

pub fn started_with_task_entries(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  issue_id: String,
  issue_identifier: String,
  task_ref_entries: List(#(String, json.Json)),
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  run_root: String,
) -> List(#(String, json.Json)) {
  list.append(
    [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("workflow_fingerprint", json.string(workflow_fingerprint)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
    ],
    list.append(task_ref_entries, [
      #("issue_fingerprint", json.string(issue_fingerprint)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
      #("run_root", json.string(run_root)),
    ]),
  )
}

pub fn provenance_repaired_entries(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  issue_id: String,
  issue_identifier: String,
  task_ref_entries: List(#(String, json.Json)),
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  run_root: String,
  repair_mode: String,
  source_evidence: List(String),
) -> List(#(String, json.Json)) {
  list.append(
    started_with_task_entries(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      task_ref_entries,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
    ),
    [
      #("repair_mode", json.string(repair_mode)),
      #("source_evidence", json.array(source_evidence, of: json.string)),
    ],
  )
}

pub fn finished_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  outcome: String,
  token_total: Int,
  turns: Int,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("issue_id", json.string(issue_id)),
    #("outcome", json.string(outcome)),
    #("token_total", json.int(token_total)),
    #("turns", json.int(turns)),
  ]
}

pub fn finished_with_task_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  task_ref_entries: List(#(String, json.Json)),
  outcome: String,
  token_total: Int,
  turns: Int,
) -> List(#(String, json.Json)) {
  list.append(
    [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("issue_id", json.string(issue_id)),
    ],
    list.append(task_ref_entries, [
      #("outcome", json.string(outcome)),
      #("token_total", json.int(token_total)),
      #("turns", json.int(turns)),
    ]),
  )
}

pub fn contract_record_entries(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  artifact_ref: String,
  artifact_sha256: String,
  artifact_bytes: Int,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("workflow_fingerprint", json.string(workflow_fingerprint)),
    #("artifact_ref", json.string(artifact_ref)),
    #("artifact_sha256", json.string(artifact_sha256)),
    #("artifact_bytes", json.int(artifact_bytes)),
  ]
}

pub fn diagnostic_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  reason: String,
) -> List(#(String, json.Json)) {
  base_reason_entries(run_id, workflow_id, issue_id, reason)
}

pub fn interrupted_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  reason: String,
) -> List(#(String, json.Json)) {
  base_reason_entries(run_id, workflow_id, issue_id, reason)
}

pub fn superseded_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  superseded_by_run_id: String,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("issue_id", json.string(issue_id)),
    #("superseded_by_run_id", json.string(superseded_by_run_id)),
    #("reason", json.string(reason)),
  ]
}

pub fn repair_requested_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  issue_identifier: String,
  requested_target: String,
  requested_step_id: Option(String),
  selected_step_id: String,
  failed_attempt_index: Int,
  next_attempt_index: Int,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("requested_target", json.string(requested_target)),
    #("requested_step_id", optional_string(requested_step_id)),
    #("selected_step_id", json.string(selected_step_id)),
    #("failed_attempt_index", json.int(failed_attempt_index)),
    #("next_attempt_index", json.int(next_attempt_index)),
    #("reason", json.string(reason)),
  ]
}

pub fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}

fn base_reason_entries(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  reason: String,
) -> List(#(String, json.Json)) {
  [
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(workflow_id)),
    #("issue_id", json.string(issue_id)),
    #("reason", json.string(reason)),
  ]
}
