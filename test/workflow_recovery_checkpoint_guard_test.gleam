import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_recovery_checkpoint_guard
import simplifile
import support/test_helpers

fn root(name: String) -> String {
  "test/tmp/workflow-recovery-checkpoint-guard/" <> name
}

fn checkpoint_text(label: String) -> String {
  "{\"label\":\"" <> label <> "\"}"
}

fn write_checkpoint(root: String, ref: String, contents: String) -> String {
  let assert Ok(Nil) =
    artifact_store.restore_filesystem_artifact_bytes(
      root,
      ref,
      bit_array.from_string(contents),
    )
  hash.sha256_hex(contents)
}

fn projection_with_refs(
  attempt_ref: #(String, String),
  input_ref: #(String, String),
  output_ref: Option(#(String, String)),
) -> projection.Projection {
  let #(attempt_artifact_ref, attempt_sha256) = attempt_ref
  let #(input_artifact_ref, input_sha256) = input_ref
  let records = [
    record.with_id(
      "attempt-finished",
      1,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "upstream",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: attempt_artifact_ref,
        artifact_sha256: attempt_sha256,
        workspace_name: "main",
        workspace_path: "workspace/main",
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "inputs-recorded",
      2,
      record.WorkflowRunInputsRecorded(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "wf-1",
        artifact_ref: input_artifact_ref,
        artifact_sha256: input_sha256,
        artifact_bytes: 1,
      ),
    ),
  ]
  let records = case output_ref {
    Some(#(ref, sha256)) ->
      list.append(records, [
        record.with_id(
          "outputs-recorded",
          3,
          record.WorkflowRunOutputsRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-1",
            artifact_ref: ref,
            artifact_sha256: sha256,
            artifact_bytes: 1,
          ),
        ),
      ])
    None -> records
  }
  projection.fold(records)
}

fn snapshot_entry_refs(
  snapshot: workflow_recovery_checkpoint_guard.Snapshot,
) -> List(String) {
  snapshot.entries |> list.map(fn(entry) { entry.ref })
}

pub fn snapshot_includes_finished_attempt_and_manifest_refs_test() {
  let fixture = root("snapshot-includes")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let output_ref = "runs/run-1/outputs.v1.json"
  let attempt_sha =
    write_checkpoint(fixture, attempt_ref, checkpoint_text("attempt"))
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let output_sha =
    write_checkpoint(fixture, output_ref, checkpoint_text("output"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      Some(#(output_ref, output_sha)),
    )

  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      artifact_store.new(fixture),
      state,
      "run-1",
    )

  assert snapshot_entry_refs(snapshot) == [attempt_ref, input_ref, output_ref]
}

pub fn snapshot_deduplicates_identical_refs_test() {
  let fixture = root("snapshot-dedup")
  test_helpers.reset_dir(fixture)
  let shared_ref = "runs/run-1/upstream/attempt-1.json"
  let shared_sha =
    write_checkpoint(fixture, shared_ref, checkpoint_text("shared"))
  let state =
    projection_with_refs(
      #(shared_ref, shared_sha),
      #(shared_ref, shared_sha),
      None,
    )

  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      artifact_store.new(fixture),
      state,
      "run-1",
    )

  assert snapshot_entry_refs(snapshot) == [shared_ref]
}

pub fn snapshot_conflicting_duplicate_refs_fail_closed_test() {
  let fixture = root("snapshot-conflict")
  test_helpers.reset_dir(fixture)
  let shared_ref = "runs/run-1/upstream/attempt-1.json"
  let shared_sha =
    write_checkpoint(fixture, shared_ref, checkpoint_text("shared"))
  let state =
    projection_with_refs(
      #(shared_ref, shared_sha),
      #(shared_ref, "different-sha"),
      None,
    )

  let assert Error(error) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      artifact_store.new(fixture),
      state,
      "run-1",
    )
  assert workflow_recovery_checkpoint_guard.code(error)
    == workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed
}

pub fn snapshot_preflight_read_failure_returns_recovery_artifact_restore_failed_test() {
  let fixture = root("snapshot-read-failure")
  test_helpers.reset_dir(fixture)
  let state =
    projection_with_refs(
      #("runs/run-1/upstream/attempt-1.json", "missing"),
      #("runs/run-1/inputs.v1.json", "missing"),
      None,
    )

  let assert Error(error) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      artifact_store.new(fixture),
      state,
      "run-1",
    )
  assert workflow_recovery_checkpoint_guard.code(error)
    == workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed
}

pub fn snapshot_preflight_hash_mismatch_returns_recovery_artifact_restore_failed_test() {
  let fixture = root("snapshot-hash-mismatch")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_sha =
    write_checkpoint(fixture, attempt_ref, checkpoint_text("attempt"))
  let _ = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, "wrong-sha"),
      None,
    )

  let assert Error(error) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      artifact_store.new(fixture),
      state,
      "run-1",
    )
  assert workflow_recovery_checkpoint_guard.code(error)
    == workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed
}

pub fn snapshot_preflight_local_path_missing_returns_recovery_artifact_restore_failed_test() {
  let fixture = root("snapshot-no-local-path")
  test_helpers.reset_dir(fixture)
  let store =
    artifact_store.custom(
      "hidden-local-path",
      artifact_store.StoreCallbacks(
        write: fn(_, _) { Ok(Nil) },
        read: fn(_) { Ok("") },
        write_bytes: fn(_, _) { Ok(Nil) },
        write_immutable_bytes: fn(_, _) { Ok(artifact_store.ImmutableExisting) },
        read_bytes: fn(_) { Ok(bit_array.from_string("{}")) },
        locate: fn(ref) {
          Ok(artifact_store.ArtifactLocation(
            ref: ref,
            uri: ref,
            display_path: ref,
            local_path: None,
          ))
        },
      ),
    )
  let state =
    projection_with_refs(
      #("runs/run-1/upstream/attempt-1.json", hash.sha256_hex("{}")),
      #("runs/run-1/inputs.v1.json", hash.sha256_hex("{}")),
      None,
    )

  let assert Error(error) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  assert workflow_recovery_checkpoint_guard.code(error)
    == workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed
}

pub fn postflight_without_mutation_reports_no_events_test() {
  let fixture = root("postflight-clean")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_sha =
    write_checkpoint(fixture, attempt_ref, checkpoint_text("attempt"))
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )

  let assert Ok(events) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  assert events == []
}

pub fn postflight_restores_mutated_step_artifact_test() {
  let fixture = root("postflight-mutate-step")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_text = checkpoint_text("attempt")
  let attempt_sha = write_checkpoint(fixture, attempt_ref, attempt_text)
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> attempt_ref
  let assert Ok(Nil) = simplifile.write(full_path, checkpoint_text("mutated"))

  let assert Ok(events) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  assert string.contains(
    workflow_recovery_checkpoint_guard.events_to_diagnostic(events),
    workflow_recovery_checkpoint_guard.protected_checkpoint_restored,
  )
  let assert Ok(restored) = simplifile.read(full_path)
  assert restored == attempt_text
}

pub fn postflight_restores_deleted_step_artifact_test() {
  let fixture = root("postflight-delete-step")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_text = checkpoint_text("attempt")
  let attempt_sha = write_checkpoint(fixture, attempt_ref, attempt_text)
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> attempt_ref
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)

  let assert Ok(_) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  let assert Ok(restored) = simplifile.read(full_path)
  assert restored == attempt_text
}

pub fn postflight_restores_deleted_checkpoint_parent_directory_test() {
  let fixture = root("postflight-delete-parent")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_text = checkpoint_text("attempt")
  let attempt_sha = write_checkpoint(fixture, attempt_ref, attempt_text)
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> attempt_ref
  let parent = path.dirname(full_path) |> result.unwrap(full_path)
  let assert Ok(Nil) = simplifile.delete(parent)

  let assert Ok(events) =
    workflow_recovery_checkpoint_guard.restore_after_recovery(fixture, snapshot)
  assert list.length(events) == 1
  let assert Ok(restored) = simplifile.read(full_path)
  assert restored == attempt_text
}

pub fn postflight_restores_deleted_output_manifest_test() {
  let fixture = root("postflight-delete-output")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let output_ref = "runs/run-1/outputs.v1.json"
  let attempt_sha =
    write_checkpoint(fixture, attempt_ref, checkpoint_text("attempt"))
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let output_text = checkpoint_text("output")
  let output_sha = write_checkpoint(fixture, output_ref, output_text)
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      Some(#(output_ref, output_sha)),
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> output_ref
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)

  let assert Ok(_) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  let assert Ok(restored) = simplifile.read(full_path)
  assert restored == output_text
}

pub fn postflight_restore_is_idempotent_after_success_test() {
  let fixture = root("postflight-idempotent")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_text = checkpoint_text("attempt")
  let attempt_sha = write_checkpoint(fixture, attempt_ref, attempt_text)
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> attempt_ref
  let assert Ok(Nil) = simplifile.write(full_path, checkpoint_text("mutated"))

  let assert Ok(first_events) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  assert list.length(first_events) == 1
  let assert Ok(second_events) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  assert second_events == []
}

pub fn postflight_restore_failure_returns_recovery_artifact_restore_failed_test() {
  let fixture = root("postflight-restore-failure")
  test_helpers.reset_dir(fixture)
  let attempt_ref = "runs/run-1/upstream/attempt-1.json"
  let input_ref = "runs/run-1/inputs.v1.json"
  let attempt_sha =
    write_checkpoint(fixture, attempt_ref, checkpoint_text("attempt"))
  let input_sha = write_checkpoint(fixture, input_ref, checkpoint_text("input"))
  let state =
    projection_with_refs(
      #(attempt_ref, attempt_sha),
      #(input_ref, input_sha),
      None,
    )
  let store = artifact_store.new(fixture)
  let assert Ok(snapshot) =
    workflow_recovery_checkpoint_guard.snapshot_for_projection(
      store,
      state,
      "run-1",
    )
  let full_path = fixture <> "/.scherzo-state/artifacts/" <> attempt_ref
  let parent = path.dirname(full_path) |> result.unwrap(full_path)
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)
  let assert Ok(Nil) = simplifile.create_directory_all(full_path)
  let assert Ok(Nil) = simplifile.create_directory_all(parent)

  let assert Error(error) =
    workflow_recovery_checkpoint_guard.restore_after_recovery_with_store(
      store,
      snapshot,
    )
  assert workflow_recovery_checkpoint_guard.code(error)
    == workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed
}
