import gleam/option.{type Option, None, Some}
import scherzo/hash
import scherzo/state/record

const record_hash_chars = 12

pub fn workstream_created_record_id(
  workstream_id: String,
  task_ref: record.TaskRefFields,
  idempotency_key: String,
) -> String {
  record_id(
    "workstream_created",
    workstream_id,
    idempotency_key,
    task_ref_discriminator(task_ref),
  )
}

pub fn workstream_assigned_record_id(
  workstream_id: String,
  assignment_id: String,
  idempotency_key: String,
) -> String {
  record_id(
    "workstream_assigned",
    workstream_id,
    idempotency_key,
    assignment_id,
  )
}

pub fn workstream_artifact_recorded_record_id(
  workstream_id: String,
  artifact_id: String,
  snapshot_ref: String,
  snapshot_sha256: String,
  idempotency_key: String,
) -> String {
  record_id(
    "workstream_artifact_recorded",
    workstream_id,
    idempotency_key,
    artifact_id <> "|" <> snapshot_ref <> "|" <> snapshot_sha256,
  )
}

pub fn workstream_handoff_recorded_record_id(
  workstream_id: String,
  handoff_id: String,
  handoff_ref: String,
  handoff_sha256: String,
  idempotency_key: String,
) -> String {
  record_id(
    "workstream_handoff_recorded",
    workstream_id,
    idempotency_key,
    handoff_id <> "|" <> handoff_ref <> "|" <> handoff_sha256,
  )
}

pub fn workstream_phase_run_queued_record_id(
  workstream_id: String,
  action_id: String,
  input_bundle_sha256: String,
  idempotency_key: String,
) -> String {
  record_id(
    "workstream_phase_run_queued",
    workstream_id,
    idempotency_key,
    action_id <> "|" <> input_bundle_sha256,
  )
}

pub fn workstream_created(
  at_ms: Int,
  workstream_id: String,
  task_ref: record.TaskRefFields,
  idempotency_key: String,
) -> record.LedgerRecord {
  record.with_id(
    workstream_created_record_id(workstream_id, task_ref, idempotency_key),
    at_ms,
    record.WorkstreamCreated(workstream_id, task_ref, idempotency_key),
  )
}

pub fn workstream_assigned(
  at_ms: Int,
  workstream_id: String,
  assignment_id: String,
  workflow_id: String,
  playbook_id: Option(String),
  reason: String,
  idempotency_key: String,
) -> record.LedgerRecord {
  record.with_id(
    workstream_assigned_record_id(workstream_id, assignment_id, idempotency_key),
    at_ms,
    record.WorkstreamAssigned(
      workstream_id,
      assignment_id,
      workflow_id,
      playbook_id,
      reason,
      idempotency_key,
    ),
  )
}

pub fn workstream_artifact_recorded(
  at_ms: Int,
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
) -> record.LedgerRecord {
  record.with_id(
    workstream_artifact_recorded_record_id(
      workstream_id,
      artifact_id,
      snapshot_ref,
      snapshot_sha256,
      idempotency_key,
    ),
    at_ms,
    record.WorkstreamArtifactRecorded(
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
    ),
  )
}

pub fn workstream_handoff_recorded(
  at_ms: Int,
  workstream_id: String,
  handoff_id: String,
  handoff_ref: String,
  handoff_sha256: String,
  handoff_bytes: Int,
  source_workflow_id: String,
  source_run_id: String,
  idempotency_key: String,
) -> record.LedgerRecord {
  record.with_id(
    workstream_handoff_recorded_record_id(
      workstream_id,
      handoff_id,
      handoff_ref,
      handoff_sha256,
      idempotency_key,
    ),
    at_ms,
    record.WorkstreamHandoffRecorded(
      workstream_id,
      handoff_id,
      handoff_ref,
      handoff_sha256,
      handoff_bytes,
      source_workflow_id,
      source_run_id,
      idempotency_key,
    ),
  )
}

pub fn workstream_phase_run_queued(
  at_ms: Int,
  workstream_id: String,
  phase_run_id: String,
  action_id: String,
  workflow_id: String,
  input_bundle_ref: String,
  input_bundle_sha256: String,
  input_bundle_bytes: Int,
  idempotency_key: String,
) -> record.LedgerRecord {
  record.with_id(
    workstream_phase_run_queued_record_id(
      workstream_id,
      action_id,
      input_bundle_sha256,
      idempotency_key,
    ),
    at_ms,
    record.WorkstreamPhaseRunQueued(
      workstream_id,
      phase_run_id,
      action_id,
      workflow_id,
      input_bundle_ref,
      input_bundle_sha256,
      input_bundle_bytes,
      idempotency_key,
    ),
  )
}

fn record_id(
  kind: String,
  workstream_id: String,
  idempotency_key: String,
  discriminator: String,
) -> String {
  let digest =
    hash.short_sha256_hex(
      kind
        <> "|"
        <> workstream_id
        <> "|"
        <> idempotency_key
        <> "|"
        <> discriminator,
      record_hash_chars,
    )
  kind <> ":" <> workstream_id <> ":" <> digest
}

fn task_ref_discriminator(task_ref: record.TaskRefFields) -> String {
  let record.TaskRefFields(
    task_backend_kind,
    task_remote_id,
    task_key,
    task_url,
  ) = task_ref
  task_backend_kind
  <> "|"
  <> task_remote_id
  <> "|"
  <> option_value(task_key)
  <> "|"
  <> option_value(task_url)
}

fn option_value(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
