import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

pub const context_name = "workstreams"

pub fn created_entries(
  workstream_id: String,
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  [
    #("workstream_id", json.string(workstream_id)),
    #("idempotency_key", json.string(idempotency_key)),
  ]
}

pub fn created_with_task_entries(
  workstream_id: String,
  task_ref_entries: List(#(String, json.Json)),
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  list.append(
    [#("workstream_id", json.string(workstream_id))],
    list.append(task_ref_entries, [
      #("idempotency_key", json.string(idempotency_key)),
    ]),
  )
}

pub fn assigned_entries(
  workstream_id: String,
  assignment_id: String,
  workflow_id: String,
  playbook_id: Option(String),
  reason: String,
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  [
    #("workstream_id", json.string(workstream_id)),
    #("assignment_id", json.string(assignment_id)),
    #("workflow_id", json.string(workflow_id)),
    #("playbook_id", option_string(playbook_id)),
    #("reason", json.string(reason)),
    #("idempotency_key", json.string(idempotency_key)),
  ]
}

pub fn artifact_entries(
  workstream_id: String,
  artifact_id: String,
  artifact_type: String,
  snapshot_ref: String,
  snapshot_sha256: String,
  snapshot_bytes: Int,
  original_path: String,
  contract_type: String,
  media_type: String,
  producer_workflow_id: String,
  producer_run_id: String,
  producer_step_id: String,
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  [
    #("workstream_id", json.string(workstream_id)),
    #("artifact_id", json.string(artifact_id)),
    #("artifact_type", json.string(artifact_type)),
    #("snapshot_ref", json.string(snapshot_ref)),
    #("snapshot_sha256", json.string(snapshot_sha256)),
    #("snapshot_bytes", json.int(snapshot_bytes)),
    #("original_path", json.string(original_path)),
    #("contract_type", json.string(contract_type)),
    #("media_type", json.string(media_type)),
    #("producer_workflow_id", json.string(producer_workflow_id)),
    #("producer_run_id", json.string(producer_run_id)),
    #("producer_step_id", json.string(producer_step_id)),
    #("idempotency_key", json.string(idempotency_key)),
  ]
}

pub fn handoff_entries(
  workstream_id: String,
  handoff_id: String,
  handoff_ref: String,
  handoff_sha256: String,
  handoff_bytes: Int,
  source_workflow_id: String,
  source_run_id: String,
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  [
    #("workstream_id", json.string(workstream_id)),
    #("handoff_id", json.string(handoff_id)),
    #("handoff_ref", json.string(handoff_ref)),
    #("handoff_sha256", json.string(handoff_sha256)),
    #("handoff_bytes", json.int(handoff_bytes)),
    #("source_workflow_id", json.string(source_workflow_id)),
    #("source_run_id", json.string(source_run_id)),
    #("idempotency_key", json.string(idempotency_key)),
  ]
}

pub fn phase_run_entries(
  workstream_id: String,
  phase_run_id: String,
  action_id: String,
  workflow_id: String,
  input_bundle_ref: String,
  input_bundle_sha256: String,
  input_bundle_bytes: Int,
  idempotency_key: String,
) -> List(#(String, json.Json)) {
  [
    #("workstream_id", json.string(workstream_id)),
    #("phase_run_id", json.string(phase_run_id)),
    #("action_id", json.string(action_id)),
    #("workflow_id", json.string(workflow_id)),
    #("input_bundle_ref", json.string(input_bundle_ref)),
    #("input_bundle_sha256", json.string(input_bundle_sha256)),
    #("input_bundle_bytes", json.int(input_bundle_bytes)),
    #("idempotency_key", json.string(idempotency_key)),
  ]
}

fn option_string(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}
