import gleam/bit_array
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/projection as state_projection
import scherzo/state/record
import scherzo/workstream/artifact_store
import scherzo/workstream/artifacts
import scherzo/workstream/ledger
import scherzo/workstream/projection
import scherzo/workstream/projection_snapshot
import scherzo/workstream/types
import support/test_helpers

pub fn inspection_projects_normal_workstream_state_test() {
  let root = "test/tmp/workstream-projection-inspection/normal"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let workstream_snapshot =
    write_snapshot(store, "workstream.json", workstream_json("blocked"))
  let action_snapshot =
    write_snapshot(store, "next-action.json", next_action_json())
  let handoff_snapshot = write_snapshot(store, "handoff.json", handoff_json())

  let projected =
    state_projection.fold([
      workstream_created_record(),
      artifact_record(
        "workstream-artifact",
        types.workstream_artifact_type,
        workstream_snapshot,
        1001,
      ),
      artifact_record(
        "next-action-artifact",
        types.next_action_artifact_type,
        action_snapshot,
        1002,
      ),
      handoff_record(handoff_snapshot),
      phase_run_record(),
    ])

  let assert [summary] = projection.summaries(projected, store)
  assert summary.status == "blocked"
  assert summary.artifact_count == 2
  assert summary.handoff_count == 1
  assert summary.queued_phase_run_count == 1

  let assert [inspection] =
    projection.inspect_by_ref(projected, store, "LIV-393")
  assert inspection.status == "blocked"
  assert list.length(inspection.artifacts) == 2
  let assert [phase] = inspection.phases
  assert phase.phase_id == "implementation"
  let assert [action] = inspection.next_actions
  assert action.state == "queued"
  assert action.resolved_by_phase_run_id == Some("phase-run-1")
  assert inspection.unresolved_next_actions == []
  assert inspection.warnings == []
}

pub fn partial_workstream_without_task_ref_still_inspects_test() {
  let root = "test/tmp/workstream-projection-inspection/partial"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let snapshot =
    write_snapshot(store, "workstream.json", workstream_json("open"))
  let projected =
    state_projection.fold([
      artifact_record(
        "workstream-artifact",
        types.workstream_artifact_type,
        snapshot,
        1001,
      ),
    ])

  let assert [inspection] =
    projection.inspect_by_ref(projected, store, "linear:LIV-393")
  assert inspection.task_ref == None
  assert inspection.status == "open"
  assert has_warning(inspection.warnings, "workstream_task_ref_missing")
}

pub fn missing_snapshot_is_reported_without_payload_test() {
  let root = "test/tmp/workstream-projection-inspection/missing"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let projected =
    state_projection.fold([
      workstream_created_record(),
      missing_artifact_record(),
    ])

  let assert [summary] = projection.summaries(projected, store)
  assert summary.status == "active"
  let assert [inspection] =
    projection.inspect_by_ref(projected, store, "LIV-393")
  assert inspection.status == "active"
  assert has_warning(inspection.warnings, "snapshot_missing")
}

pub fn malformed_snapshot_record_is_reported_before_reading_test() {
  let root = "test/tmp/workstream-projection-inspection/malformed"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let projected =
    state_projection.fold([
      workstream_created_record(),
      malformed_artifact_record(),
    ])

  let assert [summary] = projection.summaries(projected, store)
  assert summary.status == "active"
  let assert [inspection] =
    projection.inspect_by_ref(projected, store, "LIV-393")
  assert has_warning(inspection.warnings, "snapshot_sha256_invalid")
}

fn workstream_json(status: String) -> String {
  artifacts.workstream_to_string(
    types.WorkstreamArtifact(
      artifact_id: "workstream-artifact",
      workstream_id: "linear:LIV-393",
      issue: types.IssueRef(id: "issue-393"),
      status: status,
      summary: "current summary",
      produced_artifacts: [],
      next_actions: ["action-1"],
    ),
  )
}

fn next_action_json() -> String {
  artifacts.next_action_to_string(types.NextActionArtifact(
    artifact_id: "next-action-artifact",
    workstream_id: "linear:LIV-393",
    action_id: "action-1",
    workflow_id: "execplan-implementation",
    state: "available",
    priority: 1,
    inputs: ["handoff"],
    requires_gate: None,
    auto_enqueue: True,
  ))
}

fn handoff_json() -> String {
  artifacts.handoff_to_string(
    types.HandoffArtifact(
      artifact_id: "handoff-1",
      workstream_id: "linear:LIV-393",
      phase_id: "implementation",
      summary: "handoff summary",
      outputs: [],
      recommended_next_actions: ["action-1"],
      open_questions: [],
    ),
  )
}

fn write_snapshot(
  store: state_artifact_store.Store,
  original_path: String,
  contents: String,
) -> artifact_store.Snapshot {
  let assert Ok(snapshot) =
    artifact_store.snapshot_bytes(
      store,
      original_path,
      "application/json",
      bit_array.from_string(contents),
    )
  snapshot
}

fn workstream_created_record() -> record.LedgerRecord {
  ledger.workstream_created(
    1000,
    "linear:LIV-393",
    record.linear_task_ref_fields(
      "issue-393",
      Some("LIV-393"),
      Some("https://linear.app/living-systems/issue/LIV-393"),
    ),
    "ws-create-1",
  )
}

fn artifact_record(
  artifact_id: String,
  artifact_type: String,
  snapshot: artifact_store.Snapshot,
  recorded_at_ms: Int,
) -> record.LedgerRecord {
  ledger.workstream_artifact_recorded(
    recorded_at_ms,
    "linear:LIV-393",
    artifact_id,
    artifact_type,
    snapshot.ref,
    snapshot.sha256,
    snapshot.bytes,
    snapshot.original_path,
    "handoff",
    snapshot.media_type,
    "execplan",
    "run-1",
    "step-1",
    artifact_id <> "-idempotency",
  )
}

fn missing_artifact_record() -> record.LedgerRecord {
  let sha = string.repeat("a", times: 64)
  ledger.workstream_artifact_recorded(
    1001,
    "linear:LIV-393",
    "missing-artifact",
    types.workstream_artifact_type,
    "workstream-artifacts/sha256/" <> sha <> ".json",
    sha,
    64,
    "missing.json",
    "handoff",
    "application/json",
    "execplan",
    "run-1",
    "step-1",
    "missing-idempotency",
  )
}

fn malformed_artifact_record() -> record.LedgerRecord {
  ledger.workstream_artifact_recorded(
    1001,
    "linear:LIV-393",
    "malformed-artifact",
    types.workstream_artifact_type,
    "workstream-artifacts/sha256/not-a-sha.json",
    "not-a-sha",
    64,
    "malformed.json",
    "handoff",
    "application/json",
    "execplan",
    "run-1",
    "step-1",
    "malformed-idempotency",
  )
}

fn handoff_record(snapshot: artifact_store.Snapshot) -> record.LedgerRecord {
  ledger.workstream_handoff_recorded(
    1003,
    "linear:LIV-393",
    "handoff-1",
    snapshot.ref,
    snapshot.sha256,
    snapshot.bytes,
    "execplan",
    "run-1",
    "handoff-idempotency",
  )
}

fn phase_run_record() -> record.LedgerRecord {
  let sha = string.repeat("b", times: 64)
  ledger.workstream_phase_run_queued(
    1004,
    "linear:LIV-393",
    "phase-run-1",
    "action-1",
    "execplan-implementation",
    "workstream-artifacts/sha256/" <> sha <> ".json",
    sha,
    128,
    "phase-run-idempotency",
  )
}

fn has_warning(
  warnings: List(projection_snapshot.ProjectionWarning),
  code: String,
) -> Bool {
  list.any(warnings, fn(warning) { warning.code == code })
}
