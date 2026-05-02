import gleam/dict
import gleam/list
import gleam/string
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import simplifile

pub fn append_and_replay_records_test() {
  let root = "test/tmp/state-ledger/append-replay"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let records = [
    run_started_record(),
    retry_scheduled_record(),
    issue_parked_record(),
    command_completed_record(),
  ]

  let assert Ok(Nil) = ledger.append_many(path, records, True)
  let assert Ok(replayed) = ledger.replay(path)

  assert path.current_path == root <> "/.scherzo-state/ledger/current.jsonl"
  assert list.length(replayed.records) == 4
  assert replayed.truncated_tail == False
  let assert Ok(projection.RunRunning(
    issue_id: "issue-1",
    issue_identifier: "SCH-1",
    workspace_path: ".scherzo/workspaces/SCH-1",
    started_at_ms: 1000,
  )) = dict.get(replayed.projection.runs, "run-1")
  let assert Ok(projection.RetryScheduled(
    issue_identifier: "SCH-1",
    delay_ms: 10_000,
    generation: 2,
    reason: "backoff",
    scheduled_at_ms: 2000,
  )) = dict.get(replayed.projection.retries, "issue-1")
  let assert Ok(projection.ParkedIssue(
    issue_identifier: "SCH-2",
    reason: "blocked",
    observed_updated_at_ms: 2900,
    parked_at_ms: 3000,
    release_policy: "explicit_unpark_only",
    issue_fingerprint: "",
  )) = dict.get(replayed.projection.parked_issues, "issue-2")
  let assert Ok(projection.CommandCompleted(
    issue_id: "issue-1",
    status: "accepted",
    message_excerpt: "retry queued",
    completed_at_ms: 4000,
  )) = dict.get(replayed.projection.commands, "comment-1")
}

pub fn replay_ignores_truncated_trailing_line_test() {
  let root = "test/tmp/state-ledger/truncated-tail"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let line1 = record.to_string(run_started_record())
  let line2 = record.to_string(retry_scheduled_record())
  let assert Ok(Nil) =
    simplifile.write(path.current_path, line1 <> "\n" <> line2 <> "\n{")

  let assert Ok(replayed) = ledger.replay(path)
  assert replayed.truncated_tail == True
  assert list.length(replayed.records) == 2
}

pub fn replay_rejects_invalid_trailing_record_shape_test() {
  let root = "test/tmp/state-ledger/invalid-trailing-shape"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      "{\"schema_version\":1,\"record_id\":\"bad\",\"at_ms\":1,\"kind\":\"run_started\"}\n",
    )

  let assert Error(ledger.CorruptRecord(line: 1, reason: _)) =
    ledger.replay(path)
}

pub fn replay_rejects_malformed_middle_line_test() {
  let root = "test/tmp/state-ledger/malformed-middle"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let line1 = record.to_string(run_started_record())
  let line2 = record.to_string(retry_scheduled_record())
  let assert Ok(Nil) =
    simplifile.write(path.current_path, line1 <> "\n{" <> "\n" <> line2 <> "\n")

  let assert Error(ledger.CorruptRecord(line: 2, reason: _)) =
    ledger.replay(path)
}

pub fn append_redacted_record_does_not_persist_secret_test() {
  let root = "test/tmp/state-ledger/redacted-secret"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let unsafe =
    record.with_id(
      "command-secret-1",
      5000,
      record.LinearCommandCompleted(
        comment_id: "comment-secret",
        issue_id: "issue-1",
        status: "accepted",
        message_excerpt: "queued with secret-value",
      ),
    )
  let safe = record.redact_excerpts(unsafe, ["secret-value"])

  let assert Ok(Nil) = ledger.append(path, safe, False)
  let assert Ok(contents) = simplifile.read(path.current_path)
  assert !string.contains(contents, "secret-value")
  assert string.contains(contents, "[REDACTED]")
}

pub fn replay_rejects_unsupported_schema_version_test() {
  let root = "test/tmp/state-ledger/unsupported-version"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      "{\"schema_version\":2,\"record_id\":\"future\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}\n",
    )

  let assert Error(ledger.UnsupportedVersion(2)) = ledger.replay(path)
}

pub fn read_records_missing_current_returns_empty_test() {
  let root = "test/tmp/state-ledger/read-records-missing"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == []
  assert read.truncated_tail == False
}

pub fn read_records_empty_file_returns_empty_test() {
  let root = "test/tmp/state-ledger/read-records-empty"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.current_path, "")

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == []
  assert read.truncated_tail == False
}

pub fn read_records_valid_jsonl_returns_records_in_order_test() {
  let root = "test/tmp/state-ledger/read-records-valid"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let records = [run_started_record(), retry_scheduled_record()]
  let contents =
    record.to_string(run_started_record())
    <> "\n"
    <> record.to_string(retry_scheduled_record())
    <> "\n"
  let assert Ok(Nil) = simplifile.write(path.current_path, contents)

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == records
  assert read.truncated_tail == False
}

pub fn read_records_allows_empty_trailing_lines_test() {
  let root = "test/tmp/state-ledger/read-records-trailing-empty"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      record.to_string(run_started_record()) <> "\n\n",
    )

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == [run_started_record()]
  assert read.truncated_tail == False
}

pub fn read_records_ignores_final_malformed_json_as_truncated_tail_test() {
  let root = "test/tmp/state-ledger/read-records-truncated-tail"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      record.to_string(run_started_record()) <> "\n{",
    )

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == [run_started_record()]
  assert read.truncated_tail == True
}

pub fn read_records_rejects_malformed_middle_line_test() {
  let root = "test/tmp/state-ledger/read-records-malformed-middle"
  reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let contents =
    record.to_string(run_started_record())
    <> "\n{\n"
    <> record.to_string(retry_scheduled_record())
    <> "\n"
  let assert Ok(Nil) = simplifile.write(path.current_path, contents)

  let assert Error(ledger.CorruptRecord(line: 2, reason: _)) =
    ledger.read_records(path)
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
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

fn retry_scheduled_record() -> record.LedgerRecord {
  record.with_id(
    "retry-scheduled-1",
    2000,
    record.RetryScheduled(
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      delay_ms: 10_000,
      generation: 2,
      reason: "backoff",
    ),
  )
}

fn issue_parked_record() -> record.LedgerRecord {
  record.with_id(
    "issue-parked-1",
    3000,
    record.IssueParked(
      issue_id: "issue-2",
      issue_identifier: "SCH-2",
      reason: "blocked",
      observed_updated_at_ms: 2900,
    ),
  )
}

fn command_completed_record() -> record.LedgerRecord {
  record.with_id(
    "command-completed-1",
    4000,
    record.LinearCommandCompleted(
      comment_id: "comment-1",
      issue_id: "issue-1",
      status: "accepted",
      message_excerpt: "retry queued",
    ),
  )
}
