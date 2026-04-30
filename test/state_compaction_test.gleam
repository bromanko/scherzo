import gleam/dict
import gleam/list
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import simplifile

pub fn compact_preserves_projection_and_archives_current_segment_test() {
  let root = "test/tmp/state-ledger/compaction"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, initial_records(), False)
  let assert Ok(before) = ledger.load_projection(path)

  let assert Ok(Nil) = ledger.compact(path)

  let assert Ok(after) = ledger.load_projection(path)
  assert after == before
  let assert Ok(True) = simplifile.is_file(path.snapshot_path)
  let assert Ok(current_contents) = simplifile.read(path.current_path)
  assert current_contents == ""
  let assert Ok(archive_entries) = simplifile.read_directory(path.archive_dir)
  assert list.length(archive_entries) == 1
  let assert Ok(replayed) = ledger.replay(path)
  assert replayed.records == []
  assert replayed.truncated_tail == False
}

pub fn load_projection_replays_snapshot_plus_current_segment_test() {
  let root = "test/tmp/state-ledger/snapshot-plus-current"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, initial_records(), False)
  let assert Ok(Nil) = ledger.compact(path)
  let finish =
    record.with_id(
      "run-finished-1",
      4000,
      record.RunFinished(
        run_id: "run-1",
        issue_id: "issue-1",
        classification: "success",
        token_total: 111,
        turns: 5,
      ),
    )
  let assert Ok(Nil) = ledger.append(path, finish, False)

  let assert Ok(loaded) = ledger.load_projection(path)
  let assert Ok(projection.RunFinished(
    issue_id: "issue-1",
    classification: "success",
    token_total: 111,
    turns: 5,
    finished_at_ms: 4000,
  )) = dict.get(loaded.runs, "run-1")
  let assert Ok(projection.ParkedIssue(
    issue_identifier: "SCH-2",
    reason: "blocked",
    observed_updated_at_ms: 2900,
    parked_at_ms: 3000,
  )) = dict.get(loaded.parked_issues, "issue-2")
}

pub fn load_projection_rejects_malformed_snapshot_json_test() {
  assert_load_projection_rejects_snapshot(
    "test/tmp/state-ledger/snapshot-malformed-json",
    "{",
  )
}

pub fn load_projection_rejects_snapshot_with_wrong_kind_test() {
  assert_load_projection_rejects_snapshot(
    "test/tmp/state-ledger/snapshot-wrong-kind",
    "{\"schema_version\":1,\"kind\":\"not_projection\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}",
  )
}

pub fn load_projection_rejects_snapshot_with_unsupported_schema_version_test() {
  assert_load_projection_rejects_snapshot(
    "test/tmp/state-ledger/snapshot-unsupported-version",
    "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}",
  )
}

pub fn load_projection_rejects_snapshot_missing_required_arrays_test() {
  assert_load_projection_rejects_snapshot(
    "test/tmp/state-ledger/snapshot-missing-runs",
    "{\"schema_version\":1,\"kind\":\"projection_snapshot\",\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}",
  )
}

fn assert_load_projection_rejects_snapshot(
  root: String,
  contents: String,
) -> Nil {
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.snapshot_path, contents)

  let assert Error(ledger.CorruptRecord(line: 0, reason: _)) =
    ledger.load_projection(path)
  Nil
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn initial_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "run-started-1",
      1000,
      record.RunStarted(
        run_id: "run-1",
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    ),
    record.with_id(
      "retry-scheduled-1",
      2000,
      record.RetryScheduled(
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        due_at_ms: 10_000,
        generation: 2,
        reason: "backoff",
      ),
    ),
    record.with_id(
      "issue-parked-1",
      3000,
      record.IssueParked(
        issue_id: "issue-2",
        issue_identifier: "SCH-2",
        reason: "blocked",
        observed_updated_at_ms: 2900,
      ),
    ),
  ]
}
