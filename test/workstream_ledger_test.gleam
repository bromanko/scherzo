import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/state/ledger as state_ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/workstream/id
import scherzo/workstream/ledger
import simplifile
import support/test_helpers

const artifact_ref = "workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json"

const artifact_ref_v2 = "workstream-artifacts/sha256/dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd.json"

const handoff_ref = "workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json"

const bundle_ref = "workstream-artifacts/sha256/cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.json"

pub fn linear_workstream_id_is_stable_test() {
  assert id.linear_workstream_id(" LIV-393 ") == Ok("linear:LIV-393")
  assert id.linear_workstream_id("") == Error(id.EmptyIdentifier)
  assert id.linear_workstream_id("LIV 393")
    == Error(id.IdentifierContainsWhitespace)
}

pub fn workstream_record_ids_are_stable_test() {
  let task_ref = sample_task_ref()

  assert ledger.workstream_created_record_id(
      "linear:LIV-393",
      task_ref,
      "ws-create-1",
    )
    == "workstream_created:linear:LIV-393:32ecc2be3082"
  assert ledger.workstream_assigned_record_id(
      "linear:LIV-393",
      "assignment-1",
      "ws-assign-1",
    )
    == "workstream_assigned:linear:LIV-393:94cc29cea3d6"
  assert ledger.workstream_artifact_recorded_record_id(
      "linear:LIV-393",
      "artifact-1",
      artifact_ref,
      string.repeat("a", times: 64),
      "ws-artifact-1",
    )
    == "workstream_artifact_recorded:linear:LIV-393:4a7d99923d38"
  assert ledger.workstream_handoff_recorded_record_id(
      "linear:LIV-393",
      "handoff-1",
      handoff_ref,
      string.repeat("b", times: 64),
      "ws-handoff-1",
    )
    == "workstream_handoff_recorded:linear:LIV-393:52e7a9c7adef"
  assert ledger.workstream_phase_run_queued_record_id(
      "linear:LIV-393",
      "action-1",
      string.repeat("c", times: 64),
      "ws-phase-1",
    )
    == "workstream_phase_run_queued:linear:LIV-393:dbcece44ecde"
}

pub fn workstream_record_bodies_roundtrip_test() {
  assert_roundtrip(workstream_created_record())
  assert_roundtrip(workstream_assigned_record())
  assert_roundtrip(workstream_artifact_record())
  assert_roundtrip(workstream_handoff_record())
  assert_roundtrip(workstream_phase_run_record())
}

pub fn idempotent_ledger_append_coalesces_exact_retry_test() {
  let root = "test/tmp/workstream-ledger/idempotent-retry"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let first = workstream_artifact_record()
  let retry =
    ledger.workstream_artifact_recorded(
      9999,
      "linear:LIV-393",
      "artifact-1",
      "scherzo.workstream.v1",
      artifact_ref,
      string.repeat("a", times: 64),
      123,
      "docs/plan.md",
      "handoff",
      "application/json",
      "execplan",
      "run-1",
      "step-1",
      "ws-artifact-1",
    )

  let assert Ok(state_ledger.Appended) =
    state_ledger.append_idempotent(path, first, True)
  let assert Ok(state_ledger.AlreadyRecorded(existing_record: existing)) =
    state_ledger.append_idempotent(path, retry, True)

  assert existing == first
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == [first]
  let assert Ok(contents) = simplifile.read(path.current_path)
  assert list.length(string.split(string.trim(contents), on: "\n")) == 1
}

pub fn idempotent_ledger_append_rejects_same_id_different_body_test() {
  let root = "test/tmp/workstream-ledger/idempotent-conflict"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let first = workstream_artifact_record()
  let conflicting = conflicting_workstream_artifact_record(first)

  let assert Ok(state_ledger.Appended) =
    state_ledger.append_idempotent(path, first, True)
  let assert Error(state_ledger.RecordIdConflict(record_id)) =
    state_ledger.append_idempotent(path, conflicting, True)

  assert record_id == first.record_id
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == [first]
}

pub fn idempotent_ledger_append_coalesces_exact_retry_after_compaction_test() {
  let root = "test/tmp/workstream-ledger/idempotent-retry-after-compaction"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let first = workstream_artifact_record()
  let retry =
    ledger.workstream_artifact_recorded(
      9999,
      "linear:LIV-393",
      "artifact-1",
      "scherzo.workstream.v1",
      artifact_ref,
      string.repeat("a", times: 64),
      123,
      "docs/plan.md",
      "handoff",
      "application/json",
      "execplan",
      "run-1",
      "step-1",
      "ws-artifact-1",
    )

  let assert Ok(state_ledger.Appended) =
    state_ledger.append_idempotent(path, first, True)
  let assert Ok(Nil) = state_ledger.compact(path)
  let assert Ok(state_ledger.AlreadyRecorded(existing_record: existing)) =
    state_ledger.append_idempotent(path, retry, True)

  assert existing == first
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == []
}

pub fn idempotent_ledger_append_rejects_conflict_after_compaction_test() {
  let root = "test/tmp/workstream-ledger/idempotent-conflict-after-compaction"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let first = workstream_artifact_record()
  let conflicting = conflicting_workstream_artifact_record(first)

  let assert Ok(state_ledger.Appended) =
    state_ledger.append_idempotent(path, first, True)
  let assert Ok(Nil) = state_ledger.compact(path)
  let assert Error(state_ledger.RecordIdConflict(record_id)) =
    state_ledger.append_idempotent(path, conflicting, True)

  assert record_id == first.record_id
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == []
}

pub fn append_workstream_start_records_coalesces_duplicate_start_test() {
  let root = "test/tmp/workstream-ledger/workstream-start-duplicate"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let created = workstream_created_record()
  let queued = workstream_phase_run_record()

  let assert Ok(state_ledger.WorkstreamStartRecordsAppended) =
    state_ledger.append_workstream_start_records(
      path,
      [created, queued],
      queued,
      True,
    )
  let assert Ok(state_ledger.WorkstreamStartRecordsDuplicate(existing_run)) =
    state_ledger.append_workstream_start_records(
      path,
      [created, queued],
      queued,
      True,
    )

  assert existing_run.action_id == "action-1"
  assert existing_run.idempotency_key == "ws-phase-1"
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == [created, queued]
}

pub fn append_workstream_start_records_rejects_conflicting_start_test() {
  let root = "test/tmp/workstream-ledger/workstream-start-conflict"
  test_helpers.reset_dir(root)
  let assert Ok(path) = state_ledger.path_for_workspace_root(root)
  let created = workstream_created_record()
  let queued = workstream_phase_run_record()
  let conflicting =
    ledger.workstream_phase_run_queued(
      1005,
      "linear:LIV-393",
      "phase-run-2",
      "action-1",
      "execplan-implementation",
      bundle_ref,
      string.repeat("d", times: 64),
      790,
      "ws-phase-2",
    )

  let assert Ok(state_ledger.WorkstreamStartRecordsAppended) =
    state_ledger.append_workstream_start_records(
      path,
      [created, queued],
      queued,
      True,
    )
  let assert Ok(state_ledger.WorkstreamStartRecordsConflict(existing_run)) =
    state_ledger.append_workstream_start_records(
      path,
      [conflicting],
      conflicting,
      True,
    )

  assert existing_run.action_id == "action-1"
  assert existing_run.idempotency_key == "ws-phase-1"
  let assert Ok(read) = state_ledger.read_records(path)
  assert read.records == [created, queued]
}

pub fn workstream_projection_replays_core_records_test() {
  let folded =
    projection.fold([
      workstream_created_record(),
      workstream_assigned_record(),
      workstream_artifact_record(),
      workstream_handoff_record(),
      workstream_phase_run_record(),
    ])

  let assert Ok(workstream) = dict.get(folded.workstreams, "linear:LIV-393")
  let assert Some(task_ref) = workstream.task_ref
  assert task_ref.task_remote_id == "issue-393"
  let assert Some(assignment) = workstream.latest_assignment
  assert assignment.assignment_id == "assignment-1"
  assert dict.size(workstream.artifacts) == 1
  assert dict.size(workstream.handoffs) == 1
  assert dict.size(workstream.queued_phase_runs) == 1
  let assert Ok(artifact) = dict.get(workstream.artifacts, artifact_ref)
  assert artifact.snapshot_sha256 == string.repeat("a", times: 64)
  let assert Ok(handoff) = dict.get(workstream.handoffs, handoff_ref)
  assert handoff.handoff_bytes == 456
  let assert Ok(phase_run) =
    dict.get(workstream.queued_phase_runs, "phase-run-1")
  assert phase_run.input_bundle_ref == bundle_ref
}

pub fn workstream_projection_duplicate_artifacts_coalesce_test() {
  let folded =
    projection.fold([
      workstream_created_record(),
      workstream_artifact_record(),
      ledger.workstream_artifact_recorded(
        1005,
        "linear:LIV-393",
        "artifact-1",
        "scherzo.workstream.v1",
        artifact_ref,
        string.repeat("a", times: 64),
        123,
        "docs/plan.md",
        "handoff",
        "application/json",
        "execplan",
        "run-1",
        "step-1",
        "ws-artifact-duplicate",
      ),
      ledger.workstream_artifact_recorded(
        1006,
        "linear:LIV-393",
        "artifact-1",
        "scherzo.workstream.v1",
        artifact_ref_v2,
        string.repeat("d", times: 64),
        321,
        "docs/plan.md",
        "handoff",
        "application/json",
        "execplan",
        "run-1",
        "step-1",
        "ws-artifact-2",
      ),
    ])

  let assert Ok(workstream) = dict.get(folded.workstreams, "linear:LIV-393")
  assert dict.size(workstream.artifacts) == 2
  let assert Ok(latest_same_ref) = dict.get(workstream.artifacts, artifact_ref)
  assert latest_same_ref.recorded_at_ms == 1005
  let assert Ok(other_ref) = dict.get(workstream.artifacts, artifact_ref_v2)
  assert other_ref.snapshot_bytes == 321
}

fn assert_roundtrip(ledger_record: record.LedgerRecord) {
  let encoded = record.to_string(ledger_record)
  let assert Ok(decoded) = record.decode_string(encoded)
  assert decoded == ledger_record
}

fn sample_task_ref() -> record.TaskRefFields {
  record.linear_task_ref_fields(
    "issue-393",
    Some("LIV-393"),
    Some("https://linear.app/living-systems/issue/LIV-393"),
  )
}

fn workstream_created_record() -> record.LedgerRecord {
  ledger.workstream_created(
    1000,
    "linear:LIV-393",
    sample_task_ref(),
    "ws-create-1",
  )
}

fn workstream_assigned_record() -> record.LedgerRecord {
  ledger.workstream_assigned(
    1001,
    "linear:LIV-393",
    "assignment-1",
    "execplan-implementation",
    Some("playbook-1"),
    "manual_claim",
    "ws-assign-1",
  )
}

fn workstream_artifact_record() -> record.LedgerRecord {
  ledger.workstream_artifact_recorded(
    1002,
    "linear:LIV-393",
    "artifact-1",
    "scherzo.workstream.v1",
    artifact_ref,
    string.repeat("a", times: 64),
    123,
    "docs/plan.md",
    "handoff",
    "application/json",
    "execplan",
    "run-1",
    "step-1",
    "ws-artifact-1",
  )
}

fn conflicting_workstream_artifact_record(
  first: record.LedgerRecord,
) -> record.LedgerRecord {
  record.with_id(
    first.record_id,
    1003,
    record.WorkstreamArtifactRecorded(
      workstream_id: "linear:LIV-393",
      artifact_id: "artifact-1",
      artifact_type: "scherzo.workstream.v1",
      snapshot_ref: artifact_ref,
      snapshot_sha256: string.repeat("d", times: 64),
      snapshot_bytes: 123,
      original_path: "docs/plan.md",
      contract_type: "handoff",
      media_type: "application/json",
      producer_workflow_id: "execplan",
      producer_run_id: "run-1",
      producer_step_id: "step-1",
      idempotency_key: "ws-artifact-1",
    ),
  )
}

fn workstream_handoff_record() -> record.LedgerRecord {
  ledger.workstream_handoff_recorded(
    1003,
    "linear:LIV-393",
    "handoff-1",
    handoff_ref,
    string.repeat("b", times: 64),
    456,
    "execplan",
    "run-1",
    "ws-handoff-1",
  )
}

fn workstream_phase_run_record() -> record.LedgerRecord {
  ledger.workstream_phase_run_queued(
    1004,
    "linear:LIV-393",
    "phase-run-1",
    "action-1",
    "execplan-implementation",
    bundle_ref,
    string.repeat("c", times: 64),
    789,
    "ws-phase-1",
  )
}
