import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/state/archive_coverage
import scherzo/state/archive_pruned_index
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import simplifile
import support/test_helpers
import test_async

pub fn compaction_failure_boundaries_preserve_online_state_test() {
  assert_failed_compaction_preserves_online(
    "test/tmp/state-ledger/retention-marker-failure",
    fn(path) { simplifile.write(path.archive_dir <> "/pruned-runs", "blocked") },
  )
  assert_failed_compaction_preserves_online(
    "test/tmp/state-ledger/retention-snapshot-failure",
    fn(path) { simplifile.create_directory_all(path.snapshot_path <> ".tmp") },
  )
  assert_failed_compaction_preserves_online(
    "test/tmp/state-ledger/retention-archive-failure",
    fn(path) {
      simplifile.create_directory_all(path.archive_dir <> "/segment-1.jsonl")
    },
  )
  assert_failed_compaction_preserves_online(
    "test/tmp/state-ledger/retention-coverage-failure",
    fn(path) {
      simplifile.create_directory_all(path.archive_dir <> "/coverage.json")
    },
  )
}

fn assert_failed_compaction_preserves_online(
  root: String,
  sabotage: fn(ledger.LedgerPath) -> Result(Nil, simplifile.FileError),
) -> Nil {
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let assert Ok(Nil) = sabotage(path)
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Error(_) =
    ledger.compact_with_retention(path, policy, fn() { 7001 })
  let assert Ok(current_after) = simplifile.read(path.current_path)
  assert current_after == current_before
  let assert Ok(projected) = ledger.load_projection(path)
  let assert Ok(_) = projection.workflow_run(projected, "workflow-run-1")
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Online)
}

pub fn preview_and_compaction_share_candidates_and_create_pruned_presence_test() {
  let root = "test/tmp/state-ledger/retention-preview"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      path,
      terminal_run_records() |> list.append(active_run_records()),
      False,
    )
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)

  let assert Ok(preview) = ledger.preview_compaction(path, policy, 7001)
  assert preview.candidate_run_ids == ["workflow-run-1"]
  assert preview.coverage_status == "established"

  let assert Ok(report) =
    ledger.compact_with_retention(path, policy, fn() { 7001 })
  assert report.candidate_run_ids == preview.candidate_run_ids
  assert report.policy_fingerprint == preview.policy_fingerprint
  assert report.pruned_run_ids == ["workflow-run-1"]

  let assert Ok(projected) = ledger.load_projection(path)
  let assert Error(_) = projection.workflow_run(projected, "workflow-run-1")
  let assert Ok(projection.WorkflowRunActive(..)) =
    projection.workflow_run(projected, "workflow-run-active")
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Pruned)

  let marker =
    archive_pruned_index.marker_path(path.archive_dir, "workflow-run-1")
  let assert Ok(marker_before) = simplifile.read(marker)
  assert marker_before == "workflow-run-1\n"
  let assert Ok(coverage_before) =
    archive_coverage.verify_stored(path.archive_dir)
  assert list.length(coverage_before.segments) == 1

  let assert Ok(second) =
    ledger.compact_with_retention(path, policy, fn() { 7001 })
  assert second.candidate_run_ids == []
  assert second.pruned_run_ids == []
  let assert Ok(projected_after_second) = ledger.load_projection(path)
  assert projected_after_second == projected
  let assert Ok(marker_after) = simplifile.read(marker)
  assert marker_after == marker_before
  let assert Ok(coverage_after) =
    archive_coverage.verify_stored(path.archive_dir)
  assert coverage_after == coverage_before
}

pub fn indexed_misses_do_not_starve_continuing_known_run_writes_test() {
  let root = "test/tmp/state-ledger/retention-concurrent-misses"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let start_misses = test_async.new_barrier()
  let batch_checkpoint = test_async.new_barrier()
  let misses_done = process.new_subject()
  process.spawn_unlinked(fn() {
    test_async.wait_at_barrier(start_misses)
    process.send(
      misses_done,
      repeat_unknown_presence_in_batches(path, 10, batch_checkpoint),
    )
  })

  test_async.release_barrier(start_misses)
  integers(1, 10)
  |> list.each(fn(batch) {
    test_async.release_barrier(batch_checkpoint)
    assert ledger.append(path, late_finish_record(6500 + batch), False)
      == Ok(Nil)
  })
  assert test_async.expect_message_within(misses_done, 120_000) == Ok(Nil)
  let assert Ok(projected) = ledger.load_projection(path)
  let assert Ok(projection.WorkflowRunFinished(finished_at_ms: 6510, ..)) =
    projection.workflow_run(projected, "workflow-run-1")
}

fn repeat_unknown_presence_in_batches(
  path: ledger.LedgerPath,
  remaining_batches: Int,
  checkpoint: test_async.Barrier,
) -> Result(Nil, ledger.LedgerError) {
  case remaining_batches {
    0 -> Ok(Nil)
    _ -> {
      use Nil <- result.try(repeat_unknown_presence(path, 100))
      test_async.wait_at_barrier(checkpoint)
      repeat_unknown_presence_in_batches(
        path,
        remaining_batches - 1,
        checkpoint,
      )
    }
  }
}

pub fn prune_and_late_append_serialize_to_one_of_two_safe_outcomes_test() {
  let root = "test/tmp/state-ledger/retention-prune-append-race"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let start_compaction = test_async.new_barrier()
  let start_append = test_async.new_barrier()
  let compacted = process.new_subject()
  let appended = process.new_subject()
  process.spawn_unlinked(fn() {
    test_async.wait_at_barrier(start_compaction)
    process.send(
      compacted,
      ledger.compact_with_retention(path, policy, fn() { 7001 }),
    )
  })
  process.spawn_unlinked(fn() {
    test_async.wait_at_barrier(start_append)
    process.send(appended, ledger.append(path, late_finish_record(6500), False))
  })

  test_async.release_barrier(start_compaction)
  test_async.release_barrier(start_append)
  let compact_result = test_async.expect_message_within(compacted, 5000)
  let append_result = test_async.expect_message_within(appended, 5000)
  let assert Ok(_) = compact_result
  case append_result {
    Ok(Nil) -> {
      let assert Ok(ledger.Online) =
        ledger.workflow_run_presence(path, "workflow-run-1")
    }
    Error(ledger.AggregateInvariantViolation(
      "pruned_workflow_run",
      "workflow-run-1",
    )) -> {
      let assert Ok(ledger.Pruned) =
        ledger.workflow_run_presence(path, "workflow-run-1")
    }
    _ -> panic as "unexpected append race result"
  }
}

fn repeat_unknown_presence(
  path: ledger.LedgerPath,
  remaining: Int,
) -> Result(Nil, ledger.LedgerError) {
  case remaining {
    0 -> Ok(Nil)
    _ ->
      case ledger.workflow_run_presence(path, "never-known") {
        Ok(ledger.Unknown) -> repeat_unknown_presence(path, remaining - 1)
        Ok(_) -> Error(ledger.Io("unexpected historical presence"))
        Error(error) -> Error(error)
      }
  }
}

fn late_finish_record(finished_at_ms: Int) -> record.LedgerRecord {
  record.with_id(
    "late-finish-" <> int.to_string(finished_at_ms),
    finished_at_ms,
    record.WorkflowRunFinished(
      "workflow-run-1",
      "default",
      "issue-1",
      "completed",
      10,
      2,
    ),
  )
}

pub fn prune_and_late_append_both_serialized_outcomes_are_deterministic_test() {
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)

  let append_first_root =
    "test/tmp/state-ledger/retention-serialized-append-first"
  test_helpers.reset_dir(append_first_root)
  let assert Ok(append_first_path) =
    ledger.path_for_workspace_root(append_first_root)
  let assert Ok(Nil) =
    ledger.append_many(append_first_path, terminal_run_records(), False)
  assert ledger.append(append_first_path, late_finish_record(6500), False)
    == Ok(Nil)
  let assert Ok(append_first_report) =
    ledger.compact_with_retention(append_first_path, policy, fn() { 7001 })
  assert append_first_report.pruned_run_ids == []
  assert ledger.workflow_run_presence(append_first_path, "workflow-run-1")
    == Ok(ledger.Online)

  let prune_first_root =
    "test/tmp/state-ledger/retention-serialized-prune-first"
  test_helpers.reset_dir(prune_first_root)
  let assert Ok(prune_first_path) =
    ledger.path_for_workspace_root(prune_first_root)
  let assert Ok(Nil) =
    ledger.append_many(prune_first_path, terminal_run_records(), False)
  let assert Ok(prune_first_report) =
    ledger.compact_with_retention(prune_first_path, policy, fn() { 7001 })
  assert prune_first_report.pruned_run_ids == ["workflow-run-1"]
  let assert Error(ledger.AggregateInvariantViolation(
    "pruned_workflow_run",
    "workflow-run-1",
  )) = ledger.append(prune_first_path, late_finish_record(6500), False)
  assert ledger.workflow_run_presence(prune_first_path, "workflow-run-1")
    == Ok(ledger.Pruned)
}

pub fn late_append_to_pruned_run_has_distinct_stable_error_test() {
  let root = "test/tmp/state-ledger/retention-late-append"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, policy, fn() { 7001 })

  let late =
    record.with_id(
      "late-finish",
      8000,
      record.WorkflowRunFinished(
        "workflow-run-1",
        "default",
        "issue-1",
        "completed",
        10,
        2,
      ),
    )
  let assert Error(ledger.AggregateInvariantViolation(
    "pruned_workflow_run",
    "workflow-run-1",
  )) = ledger.append(path, late, False)
}

pub fn late_append_to_pruned_run_rejects_all_run_owned_record_families_test() {
  let root = "test/tmp/state-ledger/retention-late-append-families"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let policy = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, policy, fn() { 7001 })

  late_run_owned_records()
  |> list.each(fn(ledger_record) {
    let assert Error(ledger.AggregateInvariantViolation(
      "pruned_workflow_run",
      "workflow-run-1",
    )) = ledger.append(path, ledger_record, False)
  })
}

fn late_run_owned_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "late-provenance-repaired",
      8001,
      record.WorkflowRunProvenanceRepaired(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "issue-1",
        "ABC-1",
        record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
        "issue-fingerprint",
        5000,
        "test/tmp/workflow-run-1",
        "repair",
        ["artifact"],
      ),
    ),
    record.with_id(
      "late-inputs",
      8002,
      record.WorkflowRunInputsRecorded(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "inputs.json",
        "sha-inputs",
        1,
      ),
    ),
    record.with_id(
      "late-interface-snapshot",
      8003,
      record.WorkflowInterfaceSnapshotRecorded(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "interface.json",
        "sha-interface",
        1,
      ),
    ),
    record.with_id(
      "late-outputs",
      8004,
      record.WorkflowRunOutputsRecorded(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "outputs.json",
        "sha-outputs",
        1,
      ),
    ),
    record.with_id(
      "late-publication",
      8005,
      record.PublicationAttemptRecorded(
        "workflow-run-1",
        "default",
        "publication-1",
        "series-1",
        "attempt-1",
        "planned",
        True,
        True,
        False,
        None,
        None,
        None,
        None,
        None,
        None,
      ),
    ),
    record.with_id(
      "late-repair-requested",
      8006,
      record.WorkflowRepairRequested(
        "workflow-run-1",
        "default",
        "issue-1",
        "ABC-1",
        "retry_step",
        Some("implement"),
        "implement",
        1,
        2,
        "rerun",
      ),
    ),
    record.with_id(
      "late-step-recovery-started",
      8007,
      record.WorkflowStepRecoveryStarted(
        "workflow-run-1",
        "default",
        "implement",
        1,
        1,
        "recovery-session-1",
        Some("gpt-5"),
        "prompt.md",
      ),
    ),
    record.with_id(
      "late-step-recovery-finished",
      8008,
      record.WorkflowStepRecoveryFinished(
        "workflow-run-1",
        "default",
        "implement",
        1,
        1,
        "recovery-session-1",
        "recheck",
        "done",
        "accepted",
        Some(2),
      ),
    ),
    record.with_id(
      "late-control-operation",
      8009,
      record.ControlOperationQueued(
        "operation-1",
        "retry",
        "retry-step",
        "implement",
        Some("workflow-run-1"),
        Some("issue-1"),
        Some("ABC-1"),
        Some("implement"),
        None,
      ),
    ),
    record.with_id(
      "late-issue-counter",
      8010,
      record.IssueCounterUpdated(
        "issue-1",
        "ABC-1",
        1,
        1,
        8000,
        Some("workflow-run-1"),
      ),
    ),
    record.with_id(
      "late-scheduled-run",
      8011,
      record.ScheduledRunPending(
        "nightly",
        "default",
        9000,
        "workflow-run-1",
        "schedule",
        8000,
      ),
    ),
  ]
}

pub fn malformed_and_unreadable_index_rejections_append_nothing_test() {
  let root = "test/tmp/state-ledger/retention-index-unavailable"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let marker =
    archive_pruned_index.marker_path(path.archive_dir, "workflow-run-1")
  let assert Ok(current_before) = simplifile.read(path.current_path)

  let assert Ok(Nil) = simplifile.write(marker, "malformed")
  let assert Error(ledger.Io(_)) =
    ledger.append(path, late_finish_record(6500), False)
  let assert Ok(current_after_malformed) = simplifile.read(path.current_path)
  assert current_after_malformed == current_before

  let assert Ok(Nil) = simplifile.delete(marker)
  let assert Ok(Nil) = simplifile.create_directory_all(marker)
  let assert Error(ledger.Io(_)) =
    ledger.append(path, late_finish_record(6501), False)
  let assert Ok(current_after_unreadable) = simplifile.read(path.current_path)
  assert current_after_unreadable == current_before
}

pub fn covered_retain_all_rebuild_restores_pruned_history_test() {
  let root = "test/tmp/state-ledger/retention-rebuild"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Error(_) =
    projection.workflow_run(result_projection(path), "workflow-run-1")

  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)
  let assert Ok(report) = ledger.rebuild_from_archives(path, disabled)
  assert report.restored_projection_bytes > 0
  let assert Ok(_) =
    projection.workflow_run(result_projection(path), "workflow-run-1")
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Online)
}

pub fn archive_rebuild_preflight_failures_are_mutation_free_test() {
  let root = "test/tmp/state-ledger/retention-rebuild-preflight"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Ok(snapshot_before) = simplifile.read(path.snapshot_path)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let assert Ok(archive_before) = simplifile.read_directory(path.archive_dir)
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)

  let assert Error(ledger.Io(memory_error)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() { Error("memsup unavailable") },
      fn() { Ok(1_000_000_000_000_000) },
    )
  assert string.starts_with(
    memory_error,
    "available memory probe failed: memsup unavailable; raw_input_bytes=",
  )
  assert string.contains(memory_error, "; estimated_output_bytes=")
  let assert Error(ledger.Io(disk_error)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() { Ok(1_000_000_000_000_000) },
      fn() { Error("disksup unavailable") },
    )
  assert string.starts_with(
    disk_error,
    "free disk probe failed: disksup unavailable; raw_input_bytes=",
  )
  assert string.contains(disk_error, "; estimated_output_bytes=")
  let assert Error(ledger.Io(_)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() { Ok(0) },
      fn() { Ok(1_000_000_000_000_000) },
    )
  let assert Error(ledger.Io(_)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() { Ok(1_000_000_000_000_000) },
      fn() { Ok(0) },
    )
  let assert Error(ledger.Io(_)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      enabled,
      fn() { Ok(1_000_000_000_000_000) },
      fn() { Ok(1_000_000_000_000_000) },
    )

  let assert Ok(snapshot_after) = simplifile.read(path.snapshot_path)
  let assert Ok(current_after) = simplifile.read(path.current_path)
  let assert Ok(archive_after) = simplifile.read_directory(path.archive_dir)
  assert snapshot_after == snapshot_before
  assert current_after == current_before
  assert archive_after == archive_before
}

pub fn invalid_archive_coverage_rebuilds_fail_before_mutation_test() {
  assert_invalid_archive_coverage_rebuild_is_mutation_free(
    "test/tmp/state-ledger/rebuild-missing-segment",
    fn(path) {
      let assert Ok(Nil) =
        simplifile.delete(path.archive_dir <> "/segment-2.jsonl")
      Nil
    },
  )
  assert_invalid_archive_coverage_rebuild_is_mutation_free(
    "test/tmp/state-ledger/rebuild-reordered-manifest",
    fn(path) {
      let assert Ok(manifest) = archive_coverage.verify_stored(path.archive_dir)
      let assert [first, second] = manifest.segments
      let assert Ok(Nil) =
        archive_coverage.write(
          path.archive_dir,
          archive_coverage.Manifest([second, first]),
        )
      Nil
    },
  )
  assert_invalid_archive_coverage_rebuild_is_mutation_free(
    "test/tmp/state-ledger/rebuild-duplicate-manifest-entry",
    fn(path) {
      let assert Ok(manifest) = archive_coverage.verify_stored(path.archive_dir)
      let assert [first, second] = manifest.segments
      let assert Ok(Nil) =
        archive_coverage.write(
          path.archive_dir,
          archive_coverage.Manifest([first, first, second]),
        )
      Nil
    },
  )
  assert_invalid_archive_coverage_rebuild_is_mutation_free(
    "test/tmp/state-ledger/rebuild-hash-mismatch",
    fn(path) {
      let segment_path = path.archive_dir <> "/segment-2.jsonl"
      let assert Ok(contents) = simplifile.read(segment_path)
      let assert Ok(Nil) =
        simplifile.write(segment_path, contents <> "tampered\n")
      Nil
    },
  )
}

type RebuildDiskState {
  RebuildDiskState(
    snapshot: String,
    current: String,
    coverage: String,
    ledger_entries: List(String),
    archive_entries: List(String),
    segment_one: Option(String),
    segment_two: Option(String),
  )
}

fn assert_invalid_archive_coverage_rebuild_is_mutation_free(
  root: String,
  tamper: fn(ledger.LedgerPath) -> Nil,
) -> Nil {
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Ok(Nil) = ledger.append_many(path, active_run_records(), False)
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)
  let assert Ok(_) =
    ledger.compact_with_retention(path, disabled, fn() { 8001 })
  let assert Ok(valid_coverage) =
    archive_coverage.verify_stored(path.archive_dir)
  assert list.length(valid_coverage.segments) == 2

  tamper(path)
  let before = rebuild_disk_state(path)
  let probe_subject = process.new_subject()
  let assert Error(ledger.Io(reason)) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() {
        process.send(probe_subject, "memory")
        Ok(1_000_000_000_000_000)
      },
      fn() {
        process.send(probe_subject, "disk")
        Ok(1_000_000_000_000_000)
      },
    )
  assert string.contains(
    reason,
    "archive segment names, sizes, order, or hashes do not match coverage manifest",
  )
  test_async.assert_no_extra_message(probe_subject)
  assert rebuild_disk_state(path) == before
}

fn rebuild_disk_state(path: ledger.LedgerPath) -> RebuildDiskState {
  let assert Ok(snapshot) = simplifile.read(path.snapshot_path)
  let assert Ok(current) = simplifile.read(path.current_path)
  let assert Ok(coverage) =
    simplifile.read(archive_coverage.manifest_path(path.archive_dir))
  let assert Ok(ledger_entries) = simplifile.read_directory(path.ledger_dir)
  let assert Ok(archive_entries) = simplifile.read_directory(path.archive_dir)
  RebuildDiskState(
    snapshot: snapshot,
    current: current,
    coverage: coverage,
    ledger_entries: ledger_entries |> list.sort(by: string.compare),
    archive_entries: archive_entries |> list.sort(by: string.compare),
    segment_one: optional_file_contents(path.archive_dir <> "/segment-1.jsonl"),
    segment_two: optional_file_contents(path.archive_dir <> "/segment-2.jsonl"),
  )
}

fn optional_file_contents(path: String) -> Option(String) {
  case simplifile.read(path) {
    Ok(contents) -> Some(contents)
    Error(_) -> None
  }
}

pub fn archive_rebuild_rotation_failure_restores_prior_ledger_state_test() {
  let root = "test/tmp/state-ledger/rebuild-archive-rotation-failure"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Ok(Nil) = ledger.append_many(path, active_run_records(), False)
  let assert Ok(snapshot_before) = simplifile.read(path.snapshot_path)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let assert Ok(coverage_before) =
    simplifile.read(archive_coverage.manifest_path(path.archive_dir))
  let assert Ok(archive_before) = simplifile.read_directory(path.archive_dir)
  let boundary = process.new_subject()
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)

  let assert Error(ledger.Io("injected archive rotation failure")) =
    ledger.rebuild_from_archives_with_capabilities(
      path,
      disabled,
      fn() { Ok(1_000_000_000_000_000) },
      fn() { Ok(1_000_000_000_000_000) },
      fn(path, segment_number) {
        let assert Ok(rebuilt_snapshot) = simplifile.read(path.snapshot_path)
        let assert Ok(rebuilt) = projection.decode_string(rebuilt_snapshot)
        process.send(boundary, #(
          segment_number,
          projection.has_workflow_run(rebuilt, "workflow-run-1"),
          projection.has_workflow_run(rebuilt, "workflow-run-active"),
        ))
        Error(ledger.Io("injected archive rotation failure"))
      },
    )
  assert test_async.expect_message(boundary) == #(2, True, True)

  let assert Ok(snapshot_after) = simplifile.read(path.snapshot_path)
  let assert Ok(current_after) = simplifile.read(path.current_path)
  let assert Ok(coverage_after) =
    simplifile.read(archive_coverage.manifest_path(path.archive_dir))
  let assert Ok(archive_after) = simplifile.read_directory(path.archive_dir)
  assert snapshot_after == snapshot_before
  assert current_after == current_before
  assert coverage_after == coverage_before
  assert archive_after == archive_before
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Pruned)
  assert ledger.workflow_run_presence(path, "workflow-run-active")
    == Ok(ledger.Online)
}

pub fn archive_rebuild_snapshot_and_manifest_failures_restore_online_state_test() {
  assert_failed_rebuild_preserves_online(
    "test/tmp/state-ledger/rebuild-snapshot-failure",
    fn(path) { simplifile.create_directory_all(path.snapshot_path <> ".tmp") },
  )
  assert_failed_rebuild_preserves_online(
    "test/tmp/state-ledger/rebuild-manifest-failure",
    fn(path) {
      simplifile.create_directory_all(
        archive_coverage.manifest_path(path.archive_dir) <> ".tmp",
      )
    },
  )
}

fn assert_failed_rebuild_preserves_online(
  root: String,
  sabotage: fn(ledger.LedgerPath) -> Result(Nil, simplifile.FileError),
) -> Nil {
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Ok(Nil) = ledger.append_many(path, active_run_records(), False)
  let assert Ok(Nil) = sabotage(path)
  let assert Ok(snapshot_before) = simplifile.read(path.snapshot_path)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let assert Ok(coverage_before) =
    simplifile.read(archive_coverage.manifest_path(path.archive_dir))
  let assert Ok(archive_before) = simplifile.read_directory(path.archive_dir)
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)

  let assert Error(_) =
    ledger.rebuild_from_archives_with_probes(
      path,
      disabled,
      fn() { Ok(1_000_000_000_000_000) },
      fn() { Ok(1_000_000_000_000_000) },
    )

  let assert Ok(snapshot_after) = simplifile.read(path.snapshot_path)
  let assert Ok(current_after) = simplifile.read(path.current_path)
  let assert Ok(coverage_after) =
    simplifile.read(archive_coverage.manifest_path(path.archive_dir))
  let assert Ok(archive_after) = simplifile.read_directory(path.archive_dir)
  assert snapshot_after == snapshot_before
  assert current_after == current_before
  assert coverage_after == coverage_before
  assert archive_after == archive_before
  let projected = result_projection(path)
  let assert Error(_) = projection.workflow_run(projected, "workflow-run-1")
  let assert Ok(projection.WorkflowRunActive(..)) =
    projection.workflow_run(projected, "workflow-run-active")
  Nil
}

fn result_projection(path: ledger.LedgerPath) -> projection.Projection {
  let assert Ok(projected) = ledger.load_projection(path)
  projected
}

pub fn disabled_compaction_preserves_existing_archive_coverage_test() {
  let root = "test/tmp/state-ledger/retention-disabled-coverage"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 1000, 10_000, 25)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 7001 })
  let assert Ok(Nil) = ledger.append_many(path, active_run_records(), False)
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)

  let assert Ok(report) =
    ledger.compact_with_retention(path, disabled, fn() { 8001 })

  assert report.coverage_status == "disabled"
  let assert Ok(coverage) = archive_coverage.verify_stored(path.archive_dir)
  assert list.length(coverage.segments) == 2
  let assert Ok(rebuild) = ledger.rebuild_from_archives(path, disabled)
  assert rebuild.archived_current == False
}

pub fn disabled_retention_is_projection_equivalent_test() {
  let root = "test/tmp/state-ledger/retention-disabled"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, terminal_run_records(), False)
  let assert Ok(before) = ledger.load_projection(path)
  let disabled = config_types.ProjectionRetentionConfig(False, 1000, 10_000, 25)
  let assert Ok(report) =
    ledger.compact_with_retention(path, disabled, fn() { 7001 })
  let assert Ok(after) = ledger.load_projection(path)

  assert before == after
  assert report.pruned_run_ids == []
  assert report.coverage_status == "disabled"
}

fn active_run_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "workflow-run-active-started",
      5500,
      record.WorkflowRunStartedWithTask(
        "workflow-run-active",
        "default",
        "workflow-fingerprint",
        "issue-active",
        "ABC-active",
        record.linear_task_ref_fields("issue-active", Some("ABC-active"), None),
        "issue-fingerprint-active",
        5400,
        "test/tmp/workflow-run-active",
      ),
    ),
  ]
}

fn integers(from: Int, through: Int) -> List(Int) {
  case from > through {
    True -> []
    False -> [from, ..integers(from + 1, through)]
  }
}

fn terminal_run_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "workflow-run-started",
      5000,
      record.WorkflowRunStartedWithTask(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "issue-1",
        "ABC-1",
        record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
        "issue-fingerprint",
        4900,
        "test/tmp/workflow-run-1",
      ),
    ),
    record.with_id(
      "workflow-run-finished",
      6000,
      record.WorkflowRunFinished(
        "workflow-run-1",
        "default",
        "issue-1",
        "completed",
        10,
        2,
      ),
    ),
  ]
}
