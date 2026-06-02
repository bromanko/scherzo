import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub const context_name = "workstreams"

pub type Fields {
  Fields(
    workstream_id: Option(String),
    assignment_id: Option(String),
    workflow_id: Option(String),
    playbook_id: Option(String),
    reason: Option(String),
    idempotency_key: Option(String),
    artifact_id: Option(String),
    artifact_type: Option(String),
    snapshot_ref: Option(String),
    snapshot_sha256: Option(String),
    snapshot_bytes: Option(Int),
    original_path: Option(String),
    contract_type: Option(String),
    media_type: Option(String),
    producer_workflow_id: Option(String),
    producer_run_id: Option(String),
    producer_step_id: Option(String),
    handoff_id: Option(String),
    handoff_ref: Option(String),
    handoff_sha256: Option(String),
    handoff_bytes: Option(Int),
    source_workflow_id: Option(String),
    source_run_id: Option(String),
    phase_run_id: Option(String),
    action_id: Option(String),
    input_bundle_ref: Option(String),
    input_bundle_sha256: Option(String),
    input_bundle_bytes: Option(Int),
  )
}

pub type BodyConstructors(body, task_ref) {
  BodyConstructors(
    created: fn(String, task_ref, String) -> body,
    assigned: fn(String, String, String, Option(String), String, String) -> body,
    artifact_recorded: fn(
      String,
      String,
      String,
      String,
      String,
      Int,
      String,
      String,
      String,
      String,
      String,
      String,
      String,
    ) -> body,
    handoff_recorded: fn(
      String,
      String,
      String,
      String,
      Int,
      String,
      String,
      String,
    ) -> body,
    phase_run_queued: fn(
      String,
      String,
      String,
      String,
      String,
      String,
      Int,
      String,
    ) -> body,
  )
}

pub fn decode(
  kind: String,
  fields: Fields,
  constructors: BodyConstructors(body, task_ref),
  required_task_ref: fn() -> Result(task_ref, error),
  required_string: fn(Option(String), String) -> Result(String, error),
  required_int: fn(Option(Int), String) -> Result(Int, error),
  unknown_kind: fn(String) -> error,
) -> Result(body, error) {
  let BodyConstructors(
    created,
    assigned,
    artifact_recorded,
    handoff_recorded,
    phase_run_queued,
  ) = constructors

  case kind {
    "workstream_created" -> {
      use workstream_id <- result.try(required_string(
        fields.workstream_id,
        "workstream_id",
      ))
      use task_ref <- result.try(required_task_ref())
      use idempotency_key <- result.try(required_string(
        fields.idempotency_key,
        "idempotency_key",
      ))
      Ok(created(workstream_id, task_ref, idempotency_key))
    }
    "workstream_assigned" -> {
      use workstream_id <- result.try(required_string(
        fields.workstream_id,
        "workstream_id",
      ))
      use assignment_id <- result.try(required_string(
        fields.assignment_id,
        "assignment_id",
      ))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use idempotency_key <- result.try(required_string(
        fields.idempotency_key,
        "idempotency_key",
      ))
      Ok(assigned(
        workstream_id,
        assignment_id,
        workflow_id,
        fields.playbook_id,
        reason,
        idempotency_key,
      ))
    }
    "workstream_artifact_recorded" -> {
      use workstream_id <- result.try(required_string(
        fields.workstream_id,
        "workstream_id",
      ))
      use artifact_id <- result.try(required_string(
        fields.artifact_id,
        "artifact_id",
      ))
      use artifact_type <- result.try(required_string(
        fields.artifact_type,
        "artifact_type",
      ))
      use snapshot_ref <- result.try(required_string(
        fields.snapshot_ref,
        "snapshot_ref",
      ))
      use snapshot_sha256 <- result.try(required_string(
        fields.snapshot_sha256,
        "snapshot_sha256",
      ))
      use snapshot_bytes <- result.try(required_int(
        fields.snapshot_bytes,
        "snapshot_bytes",
      ))
      use original_path <- result.try(required_string(
        fields.original_path,
        "original_path",
      ))
      use contract_type <- result.try(required_string(
        fields.contract_type,
        "contract_type",
      ))
      use media_type <- result.try(required_string(
        fields.media_type,
        "media_type",
      ))
      use producer_workflow_id <- result.try(required_string(
        fields.producer_workflow_id,
        "producer_workflow_id",
      ))
      use producer_run_id <- result.try(required_string(
        fields.producer_run_id,
        "producer_run_id",
      ))
      use producer_step_id <- result.try(required_string(
        fields.producer_step_id,
        "producer_step_id",
      ))
      use idempotency_key <- result.try(required_string(
        fields.idempotency_key,
        "idempotency_key",
      ))
      Ok(artifact_recorded(
        workstream_id,
        artifact_id,
        artifact_type,
        snapshot_ref,
        snapshot_sha256,
        snapshot_bytes,
        original_path,
        contract_type,
        media_type,
        producer_workflow_id,
        producer_run_id,
        producer_step_id,
        idempotency_key,
      ))
    }
    "workstream_handoff_recorded" -> {
      use workstream_id <- result.try(required_string(
        fields.workstream_id,
        "workstream_id",
      ))
      use handoff_id <- result.try(required_string(
        fields.handoff_id,
        "handoff_id",
      ))
      use handoff_ref <- result.try(required_string(
        fields.handoff_ref,
        "handoff_ref",
      ))
      use handoff_sha256 <- result.try(required_string(
        fields.handoff_sha256,
        "handoff_sha256",
      ))
      use handoff_bytes <- result.try(required_int(
        fields.handoff_bytes,
        "handoff_bytes",
      ))
      use source_workflow_id <- result.try(required_string(
        fields.source_workflow_id,
        "source_workflow_id",
      ))
      use source_run_id <- result.try(required_string(
        fields.source_run_id,
        "source_run_id",
      ))
      use idempotency_key <- result.try(required_string(
        fields.idempotency_key,
        "idempotency_key",
      ))
      Ok(handoff_recorded(
        workstream_id,
        handoff_id,
        handoff_ref,
        handoff_sha256,
        handoff_bytes,
        source_workflow_id,
        source_run_id,
        idempotency_key,
      ))
    }
    "workstream_phase_run_queued" -> {
      use workstream_id <- result.try(required_string(
        fields.workstream_id,
        "workstream_id",
      ))
      use phase_run_id <- result.try(required_string(
        fields.phase_run_id,
        "phase_run_id",
      ))
      use action_id <- result.try(required_string(fields.action_id, "action_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use input_bundle_ref <- result.try(required_string(
        fields.input_bundle_ref,
        "input_bundle_ref",
      ))
      use input_bundle_sha256 <- result.try(required_string(
        fields.input_bundle_sha256,
        "input_bundle_sha256",
      ))
      use input_bundle_bytes <- result.try(required_int(
        fields.input_bundle_bytes,
        "input_bundle_bytes",
      ))
      use idempotency_key <- result.try(required_string(
        fields.idempotency_key,
        "idempotency_key",
      ))
      Ok(phase_run_queued(
        workstream_id,
        phase_run_id,
        action_id,
        workflow_id,
        input_bundle_ref,
        input_bundle_sha256,
        input_bundle_bytes,
        idempotency_key,
      ))
    }
    other -> Error(unknown_kind(other))
  }
}

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
