import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/string
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import simplifile
import support/test_helpers
import test_async

pub fn record_body_hash_ignores_record_id_and_timestamp_test() {
  let first =
    record.with_id(
      "run-started-1",
      1000,
      record.RunStarted(
        run_id: "run-1",
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    )
  let retry = record.with_id("run-started-2", 9999, first.body)

  assert record.body_to_string(first.body) == record.body_to_string(retry.body)
}

pub fn cache_diagnostics_hydrate_once_and_hit_warm_append_test() {
  let root = "test/tmp/state-ledger-cache/warm-append"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Ok(Nil) = ledger.append(path, run_started_record(), False)
  let assert Ok(initial) = ledger.cache_diagnostics(path)
  assert initial.hydration_count == 1
  assert initial.reload_count == 0

  let assert Ok(_) = ledger.load_projection(path)
  let assert Ok(Nil) = ledger.append(path, retry_scheduled_record(), False)
  let assert Ok(after) = ledger.cache_diagnostics(path)

  assert after.hydration_count == 1
  assert after.reload_count == 0
  assert after.cache_hit_count >= 2
  assert after.record_id_index_size == 2
}

pub fn large_current_segment_warm_append_stays_single_hydration_test() {
  let root = "test/tmp/state-ledger-cache/large-current-segment"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let records = [run_started_record(), ..many_retry_records(300, 0, [])]
  let contents =
    records |> list.map(record.to_string) |> string.join(with: "\n")

  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.current_path, contents <> "\n")

  let assert Ok(_) = ledger.load_projection(path)
  let assert Ok(before) = ledger.cache_diagnostics(path)
  assert before.hydration_count == 1
  assert before.record_id_index_size == 301

  let assert Ok(Nil) = ledger.append(path, issue_parked_record(), False)
  let assert Ok(after) = ledger.cache_diagnostics(path)

  assert after.hydration_count == 1
  assert after.reload_count == 0
  assert after.cache_hit_count >= 1
  assert after.record_id_index_size == 302
}

pub fn load_projection_accepts_snapshot_without_record_index_metadata_test() {
  let root = "test/tmp/state-ledger-cache/old-snapshot"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let snapshot = projection.fold([run_started_record()]) |> projection.to_string
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, snapshot <> "\n")

  let assert Ok(loaded) = ledger.load_projection(path)
  assert loaded == projection.fold([run_started_record()])
}

pub fn load_projection_ignores_malformed_record_index_metadata_test() {
  let root = "test/tmp/state-ledger-cache/malformed-snapshot-metadata"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let base_snapshot =
    projection.fold([run_started_record()]) |> projection.to_string
  let snapshot =
    string.drop_end(base_snapshot, 1)
    <> ",\"record_id_index_metadata\":\"bad\"}"
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, snapshot <> "\n")

  let assert Ok(loaded) = ledger.load_projection(path)
  assert loaded == projection.fold([run_started_record()])
}

pub fn compaction_writes_snapshot_record_index_metadata_test() {
  let root = "test/tmp/state-ledger-cache/compact-metadata"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let artifact = workstream_artifact_record()

  let assert Ok(ledger.Appended) =
    ledger.append_idempotent(path, artifact, True)
  let assert Ok(Nil) = ledger.compact(path)
  let assert Ok(contents) = simplifile.read(path.snapshot_path)
  assert string.contains(contents, "record_id_index_metadata")
  let assert Ok(ledger.AlreadyRecorded(existing_record: existing)) =
    ledger.append_idempotent(path, workstream_artifact_retry_record(), True)
  assert existing == artifact
}

pub fn failed_compaction_preserves_snapshot_record_index_metadata_test() {
  let root = "test/tmp/state-ledger-cache/failed-compaction-metadata"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let artifact = workstream_artifact_record()
  let saved_archive_dir = path.archive_dir <> "-saved"

  let assert Ok(ledger.Appended) =
    ledger.append_idempotent(path, artifact, True)
  let assert Ok(Nil) = ledger.compact(path)
  let assert Ok(Nil) = ledger.append(path, issue_parked_record(), False)
  let assert Ok(Nil) = simplifile.rename(path.archive_dir, saved_archive_dir)
  let assert Ok(Nil) = simplifile.write(path.archive_dir, "blocked")

  let assert Error(ledger.Io(_)) = ledger.compact(path)

  let assert Ok(Nil) = simplifile.delete(path.archive_dir)
  let assert Ok(Nil) = simplifile.rename(saved_archive_dir, path.archive_dir)
  let assert Ok(ledger.AlreadyRecorded(existing_record: existing)) =
    ledger.append_idempotent(path, workstream_artifact_retry_record(), True)
  assert existing == artifact
}

pub fn external_current_mutation_forces_reload_before_append_test() {
  let root = "test/tmp/state-ledger-cache/external-reload-before-append"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)

  let assert Ok(_) = ledger.load_projection(path)
  let assert Ok(before) = ledger.cache_diagnostics(path)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      record.to_string(workflow_run_started_record()) <> "\n",
    )

  let assert Ok(Nil) =
    ledger.append(path, step_attempt_prepared_record(), False)
  let assert Ok(projected) = ledger.load_projection(path)
  let assert Ok(_) = dict.get(projected.workflow_runs, "workflow-run-1")
  let assert Ok(_) =
    dict.get(
      projected.step_attempts,
      projection.step_attempt_key("workflow-run-1", "build", 1),
    )
  let assert Ok(after) = ledger.cache_diagnostics(path)

  assert after.reload_count == before.reload_count + 1
  assert after.fingerprint_mismatch_count
    == before.fingerprint_mismatch_count + 1
}

pub fn external_snapshot_replacement_forces_reload_test() {
  let root = "test/tmp/state-ledger-cache/external-snapshot-reload"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let initial = projection.fold([run_started_record()]) |> projection.to_string
  let replacement =
    projection.fold([run_started_record(), retry_scheduled_record()])
    |> projection.to_string
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, initial <> "\n")

  let assert Ok(_) = ledger.load_projection(path)
  let assert Ok(before) = ledger.cache_diagnostics(path)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, replacement <> "\n")

  let assert Ok(reloaded) = ledger.load_projection(path)
  let assert Ok(after) = ledger.cache_diagnostics(path)

  assert reloaded
    == projection.fold([run_started_record(), retry_scheduled_record()])
  assert after.reload_count == before.reload_count + 1
  assert after.fingerprint_mismatch_count
    == before.fingerprint_mismatch_count + 1
}

pub fn same_size_snapshot_replacement_forces_reload_test() {
  let root = "test/tmp/state-ledger-cache/same-size-snapshot-reload"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let initial = projection.fold([run_started_record()]) |> projection.to_string
  let replacement_record = replacement_run_started_record()
  let replacement =
    projection.fold([replacement_record]) |> projection.to_string
  let replacement_path = path.snapshot_path <> ".replacement"
  assert string.length(initial) == string.length(replacement)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, initial <> "\n")

  let assert Ok(_) = ledger.load_projection(path)
  let assert Ok(before) = ledger.cache_diagnostics(path)
  let assert Ok(Nil) = simplifile.write(replacement_path, replacement <> "\n")
  let assert Ok(Nil) = simplifile.rename(replacement_path, path.snapshot_path)

  let assert Ok(reloaded) = ledger.load_projection(path)
  let assert Ok(after) = ledger.cache_diagnostics(path)

  assert reloaded == projection.fold([replacement_record])
  assert after.reload_count == before.reload_count + 1
  assert after.fingerprint_mismatch_count
    == before.fingerprint_mismatch_count + 1
}

pub fn corrupt_external_mutation_fails_closed_test() {
  let root = "test/tmp/state-ledger-cache/corrupt-reload"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let start = run_started_record()
  let assert Ok(Nil) = ledger.append(path, start, False)
  let assert Ok(contents) = simplifile.read(path.current_path)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      contents <> "{\n" <> record.to_string(retry_scheduled_record()) <> "\n",
    )

  let assert Error(ledger.CorruptRecord(line: 2, reason: _)) =
    ledger.append(path, issue_parked_record(), False)
}

pub fn concurrent_same_id_same_body_is_idempotent_test() {
  let root = "test/tmp/state-ledger-cache/concurrent-duplicate"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let barrier = test_async.new_barrier()
  let subject = process.new_subject()
  let record = run_started_record()

  let _ =
    process.spawn(fn() {
      test_async.block_until_released(barrier)
      process.send(subject, ledger.append_idempotent(path, record, True))
    })
  let _ =
    process.spawn(fn() {
      test_async.block_until_released(barrier)
      process.send(subject, ledger.append_idempotent(path, record, True))
    })

  test_async.release_barrier(barrier)
  test_async.release_barrier(barrier)
  let first = test_async.expect_message(subject)
  let second = test_async.expect_message(subject)

  assert has_appended_and_duplicate(first, second)
  let assert Ok(read) = ledger.read_records(path)
  assert list.length(read.records) == 1
}

pub fn concurrent_same_id_different_body_conflicts_test() {
  let root = "test/tmp/state-ledger-cache/concurrent-conflict"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let barrier = test_async.new_barrier()
  let subject = process.new_subject()
  let first = run_started_record()
  let conflicting =
    record.with_id(
      first.record_id,
      1001,
      record.RunStarted(
        run_id: "run-2",
        issue_id: "issue-2",
        issue_identifier: "SCH-2",
        workspace_path: ".scherzo/workspaces/SCH-2",
      ),
    )

  let _ =
    process.spawn(fn() {
      test_async.block_until_released(barrier)
      process.send(subject, ledger.append_idempotent(path, first, True))
    })
  let _ =
    process.spawn(fn() {
      test_async.block_until_released(barrier)
      process.send(subject, ledger.append_idempotent(path, conflicting, True))
    })

  test_async.release_barrier(barrier)
  test_async.release_barrier(barrier)
  let left = test_async.expect_message(subject)
  let right = test_async.expect_message(subject)

  assert has_appended_and_conflict(left, right, first.record_id)
  let assert Ok(read) = ledger.read_records(path)
  assert list.length(read.records) == 1
}

fn has_appended_and_duplicate(
  left: Result(ledger.AppendIdempotentResult, ledger.AppendIdempotentError),
  right: Result(ledger.AppendIdempotentResult, ledger.AppendIdempotentError),
) -> Bool {
  case left, right {
    Ok(ledger.Appended), Ok(ledger.AlreadyRecorded(_)) -> True
    Ok(ledger.AlreadyRecorded(_)), Ok(ledger.Appended) -> True
    _, _ -> False
  }
}

fn has_appended_and_conflict(
  left: Result(ledger.AppendIdempotentResult, ledger.AppendIdempotentError),
  right: Result(ledger.AppendIdempotentResult, ledger.AppendIdempotentError),
  record_id: String,
) -> Bool {
  case left, right {
    Ok(ledger.Appended), Error(ledger.RecordIdConflict(conflict_id)) ->
      conflict_id == record_id
    Error(ledger.RecordIdConflict(conflict_id)), Ok(ledger.Appended) ->
      conflict_id == record_id
    _, _ -> False
  }
}

fn run_started_record() -> record.LedgerRecord {
  record.with_id(
    "run-started-1",
    1000,
    record.RunStarted(
      run_id: "run-1",
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      workspace_path: ".scherzo/workspaces/SCH-1",
    ),
  )
}

fn replacement_run_started_record() -> record.LedgerRecord {
  record.with_id(
    "run-started-2",
    1000,
    record.RunStarted(
      run_id: "run-2",
      issue_id: "issue-2",
      issue_identifier: "SCH-2",
      workspace_path: ".scherzo/workspaces/SCH-2",
    ),
  )
}

fn retry_scheduled_record() -> record.LedgerRecord {
  record.with_id(
    "retry-scheduled-1",
    2000,
    record.RetryScheduled(
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      delay_ms: 10_000,
      generation: 1,
      reason: "backoff",
    ),
  )
}

fn many_retry_records(
  remaining: Int,
  index: Int,
  acc: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      many_retry_records(remaining - 1, index + 1, [
        record.with_id(
          "retry-scheduled-" <> int.to_string(index + 2),
          2000 + index,
          record.RetryScheduled(
            issue_id: "issue-" <> int.to_string(index + 2),
            issue_identifier: "SCH-" <> int.to_string(index + 2),
            delay_ms: 10_000 + index,
            generation: 1,
            reason: "backoff",
          ),
        ),
        ..acc
      ])
  }
}

fn workflow_run_started_record() -> record.LedgerRecord {
  record.with_id(
    "workflow-run-started-1",
    4000,
    record.WorkflowRunStarted(
      "workflow-run-1",
      "default",
      "workflow-fingerprint",
      "issue-1",
      "LIV-1457",
      "issue-fingerprint",
      3999,
      "runs/workflow-run-1",
    ),
  )
}

fn step_attempt_prepared_record() -> record.LedgerRecord {
  record.with_id(
    "step-attempt-prepared-1",
    4001,
    record.StepAttemptPrepared(
      "workflow-run-1",
      "default",
      "build",
      1,
      "default",
      "test/tmp/workspaces/LIV-1457",
      "test/tmp/workspaces/LIV-1457",
      None,
      None,
    ),
  )
}

fn issue_parked_record() -> record.LedgerRecord {
  record.with_id(
    "issue-parked-1",
    3000,
    record.IssueParked(
      issue_id: "issue-3",
      issue_identifier: "SCH-3",
      reason: "blocked",
      observed_updated_at_ms: 2999,
    ),
  )
}

fn workstream_artifact_record() -> record.LedgerRecord {
  record.with_id(
    "workstream-artifact-1",
    1000,
    record.WorkstreamArtifactRecorded(
      workstream_id: "linear:LIV-1457",
      artifact_id: "artifact-1",
      artifact_type: "scherzo.workstream.v1",
      snapshot_ref: "artifacts/one.json",
      snapshot_sha256: string.repeat("a", times: 64),
      snapshot_bytes: 123,
      original_path: "docs/plans/LIV-1411-ledger-append-hot-path-cache.md",
      contract_type: "handoff",
      media_type: "application/json",
      producer_workflow_id: "execplan",
      producer_run_id: "run-1",
      producer_step_id: "step-1",
      idempotency_key: "artifact-1",
    ),
  )
}

fn workstream_artifact_retry_record() -> record.LedgerRecord {
  record.with_id(
    "workstream-artifact-1",
    9999,
    workstream_artifact_record().body,
  )
}
