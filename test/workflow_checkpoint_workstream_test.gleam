import gleam/bit_array
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/artifact_store
import scherzo/state/ledger as state_ledger
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workstream/ledger
import simplifile

pub fn workflow_checkpoint_snapshots_generated_workstream_bytes_test() {
  let root = "test/tmp/workflow-checkpoint-workstream/generated-bytes"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/handoffs/handoff-1.json",
      "application/json",
      bit_array.from_string("{\"ok\":true}"),
    )

  assert string.starts_with(snapshot.ref, "workstream-artifacts/sha256/")
  let assert Ok(contents) =
    artifact_store.read_artifact_unverified(
      artifact_store.new(root),
      snapshot.ref,
    )
  assert contents == "{\"ok\":true}"
}

pub fn workflow_checkpoint_snapshots_existing_artifact_refs_test() {
  let root = "test/tmp/workflow-checkpoint-workstream/existing-ref"
  reset_dir(root)
  let store = artifact_store.new(root)
  let assert Ok(existing) =
    artifact_store.write_output_blob(store, "run-1", "bundle", ".json", "{}")
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(snapshot) =
    checkpoint.snapshot_existing_artifact_ref(
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "application/json",
    )

  assert snapshot.sha256 == existing.sha256
  assert snapshot.original_path == existing.ref
}

pub fn workflow_checkpoint_appends_workstream_records_idempotently_test() {
  let root = "test/tmp/workflow-checkpoint-workstream/idempotent-append"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let ledger_record =
    ledger.workstream_created(
      123,
      "linear:LIV-436",
      record.linear_task_ref_fields("issue-1", Some("LIV-436"), None),
      "create-1",
    )

  let assert Ok(state_ledger.Appended) =
    checkpoint.append_workstream_record_idempotent(ledger_record)
  let assert Ok(state_ledger.AlreadyRecorded(existing_record: existing)) =
    checkpoint.append_workstream_record_idempotent(ledger_record)

  assert existing.body == ledger_record.body
}

pub fn workflow_checkpoint_rejects_conflicting_workstream_records_test() {
  let root = "test/tmp/workflow-checkpoint-workstream/conflicting-append"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let first =
    ledger.workstream_created(
      123,
      "linear:LIV-436",
      record.linear_task_ref_fields("issue-1", Some("LIV-436"), None),
      "create-1",
    )
  let conflicting =
    record.with_id(
      first.record_id,
      124,
      record.WorkstreamCreated(
        workstream_id: "linear:LIV-436",
        task_ref: record.linear_task_ref_fields(
          "issue-2",
          Some("LIV-436"),
          None,
        ),
        idempotency_key: "create-1",
      ),
    )

  let assert Ok(state_ledger.Appended) =
    checkpoint.append_workstream_record_idempotent(first)
  let assert Error(workflow_checkpoint.CheckpointAppendFailed(reason)) =
    checkpoint.append_workstream_record_idempotent(conflicting)

  assert string.starts_with(reason, "record_id_conflict:")
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}
