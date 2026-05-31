import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import simplifile
import support/test_helpers

pub fn append_and_replay_records_test() {
  let root = "test/tmp/state-ledger/append-replay"
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      "{\"schema_version\":2,\"record_id\":\"bad\",\"at_ms\":1,\"kind\":\"run_started\"}\n",
    )

  let assert Error(ledger.CorruptRecord(line: 1, reason: _)) =
    ledger.replay(path)
}

pub fn replay_rejects_malformed_middle_line_test() {
  let root = "test/tmp/state-ledger/malformed-middle"
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      "{\"schema_version\":3,\"record_id\":\"future\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}\n",
    )

  let assert Error(ledger.UnsupportedVersion(3)) = ledger.replay(path)
}

pub fn read_records_missing_current_returns_empty_test() {
  let root = "test/tmp/state-ledger/read-records-missing"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == []
  assert read.truncated_tail == False
}

pub fn read_records_empty_file_returns_empty_test() {
  let root = "test/tmp/state-ledger/read-records-empty"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) = simplifile.write(path.current_path, "")

  let assert Ok(read) = ledger.read_records(path)
  assert read.records == []
  assert read.truncated_tail == False
}

pub fn read_records_valid_jsonl_returns_records_in_order_test() {
  let root = "test/tmp/state-ledger/read-records-valid"
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
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
  test_helpers.reset_dir(root)
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

pub fn append_idempotent_publication_attempt_records_test() {
  let root = "test/tmp/state-ledger/publication-idempotent"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let run_started = workflow_run_started_record("workflow-started")
  let publication = publication_attempt_record("publication-attempt", "planned")
  let conflicting = publication_attempt_record("publication-attempt", "failed")

  let assert Ok(Nil) = ledger.append(path, run_started, True)
  let assert Ok(ledger.Appended) =
    ledger.append_idempotent(path, publication, True)
  let assert Ok(ledger.AlreadyRecorded(_)) =
    ledger.append_idempotent(path, publication, True)
  let assert Error(ledger.RecordIdConflict("publication-attempt")) =
    ledger.append_idempotent(path, conflicting, True)
}

pub fn read_records_rejects_malformed_middle_line_test() {
  let root = "test/tmp/state-ledger/read-records-malformed-middle"
  test_helpers.reset_dir(root)
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

pub fn append_rejects_orphan_step_attempt_records_test() {
  let root = "test/tmp/state-ledger/orphan-step-attempts"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  [
    step_attempt_prepared_record("prepared"),
    step_attempt_started_record("started"),
    step_attempt_continuation_started_record("continuation"),
    step_attempt_pi_session_recorded_record("pi-session"),
    step_attempt_pi_session_recorded_with_task_record("pi-session-task"),
    step_attempt_finished_record("finished"),
    step_attempt_interrupted_record("interrupted"),
    step_attempt_superseded_record("superseded"),
  ]
  |> list.each(fn(ledger_record) {
    let assert Error(ledger.AggregateInvariantViolation(
      reason: "orphan_step_attempt_without_workflow_run",
      run_id: "workflow-run-1",
    )) = ledger.append(path, ledger_record, True)
  })
}

pub fn append_rejects_workflow_terminal_records_without_parent_test() {
  let root = "test/tmp/state-ledger/orphan-workflow-terminals"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  [
    workflow_run_finished_record("finished"),
    workflow_run_finished_with_task_record("finished-task"),
    workflow_run_interrupted_record("interrupted"),
    workflow_run_superseded_record("superseded"),
  ]
  |> list.each(fn(ledger_record) {
    let assert Error(ledger.AggregateInvariantViolation(
      reason: "unknown_workflow_run",
      run_id: "workflow-run-1",
    )) = ledger.append(path, ledger_record, True)
  })
}

pub fn append_accepts_same_batch_workflow_parent_and_step_attempt_test() {
  let root = "test/tmp/state-ledger/same-batch-parent"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Ok(Nil) =
    ledger.append_many(
      path,
      [
        workflow_run_started_with_task_record("parent"),
        step_attempt_prepared_record("child"),
      ],
      True,
    )
  let assert Ok(projected) = ledger.load_projection(path)
  let assert Ok(_) = dict.get(projected.workflow_runs, "workflow-run-1")
  let assert Ok(_) =
    dict.get(
      projected.step_attempts,
      projection.step_attempt_key("workflow-run-1", "build", 1),
    )
}

pub fn append_accepts_step_attempt_after_earlier_workflow_parent_test() {
  let root = "test/tmp/state-ledger/earlier-parent"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Ok(Nil) =
    ledger.append(path, workflow_run_started_with_task_record("parent"), True)
  let assert Ok(Nil) =
    ledger.append(path, step_attempt_started_record("child"), True)
}

pub fn append_rejects_step_attempt_after_non_start_run_scoped_record_test() {
  let root = "test/tmp/state-ledger/non-start-does-not-create-parent"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)

  let assert Error(ledger.AggregateInvariantViolation(
    reason: "orphan_step_attempt_without_workflow_run",
    run_id: "workflow-run-1",
  )) =
    ledger.append_many(
      path,
      [
        workflow_run_inputs_recorded_record("inputs"),
        step_attempt_prepared_record("after-inputs"),
      ],
      True,
    )
  let assert Ok(read) = ledger.read_records(path)
  assert read.records == []
}

pub fn replay_preserves_historical_orphan_step_attempts_test() {
  let root = "test/tmp/state-ledger/historical-orphan"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      path.current_path,
      record.to_string(step_attempt_finished_record("historical")) <> "\n",
    )

  let assert Ok(replayed) = ledger.replay(path)
  let assert Ok(_) =
    dict.get(
      replayed.projection.step_attempts,
      projection.step_attempt_key("workflow-run-1", "build", 1),
    )
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

fn workflow_run_started_with_task_record(
  suffix: String,
) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-started-with-task-" <> suffix,
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
      "test/tmp/workspaces/ABC-1",
    ),
  )
}

fn workflow_run_inputs_recorded_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-inputs-recorded-" <> suffix,
    5500,
    record.WorkflowRunInputsRecorded(
      "workflow-run-1",
      "default",
      "workflow-fingerprint",
      "runs/workflow-run-1/input.json",
      "sha256",
      12,
    ),
  )
}

fn workflow_run_finished_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-finished-" <> suffix,
    6000,
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

fn workflow_run_finished_with_task_record(
  suffix: String,
) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-finished-with-task-" <> suffix,
    6001,
    record.WorkflowRunFinishedWithTask(
      "workflow-run-1",
      "default",
      "issue-1",
      record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      "completed",
      10,
      2,
    ),
  )
}

fn workflow_run_interrupted_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-interrupted-" <> suffix,
    6002,
    record.WorkflowRunInterrupted(
      "workflow-run-1",
      "default",
      "issue-1",
      "operator_abort",
    ),
  )
}

fn workflow_run_superseded_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-superseded-" <> suffix,
    6003,
    record.WorkflowRunSuperseded(
      "workflow-run-1",
      "default",
      "issue-1",
      "workflow-run-2",
      "replacement",
    ),
  )
}

fn step_attempt_prepared_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-prepared-" <> suffix,
    7000,
    record.StepAttemptPrepared(
      "workflow-run-1",
      "default",
      "build",
      1,
      "default",
      "test/tmp/workspaces/ABC-1",
      "test/tmp/workspaces/ABC-1",
      None,
      None,
    ),
  )
}

fn step_attempt_started_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-started-" <> suffix,
    7001,
    record.StepAttemptStarted(
      "workflow-run-1",
      "default",
      "build",
      1,
      "session-1",
      None,
      True,
    ),
  )
}

fn step_attempt_continuation_started_record(
  suffix: String,
) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-continuation-started-" <> suffix,
    7002,
    record.StepAttemptContinuationStarted(
      "workflow-run-1",
      "default",
      "build",
      1,
      "pi-session-1",
    ),
  )
}

fn step_attempt_pi_session_recorded_record(
  suffix: String,
) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-pi-session-recorded-" <> suffix,
    7003,
    record.StepAttemptPiSessionRecorded(
      "workflow-run-1",
      "issue-1",
      "ABC-1",
      "default",
      "workflow-fingerprint",
      "build",
      "default",
      1,
      "test/tmp/workspaces/ABC-1",
      "pi-session-1",
      "runs/workflow-run-1/pi.json",
    ),
  )
}

fn step_attempt_pi_session_recorded_with_task_record(
  suffix: String,
) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-pi-session-recorded-with-task-" <> suffix,
    7004,
    record.StepAttemptPiSessionRecordedWithTask(
      "workflow-run-1",
      "issue-1",
      "ABC-1",
      record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      "default",
      "workflow-fingerprint",
      "build",
      "default",
      1,
      "test/tmp/workspaces/ABC-1",
      "pi-session-1",
      "runs/workflow-run-1/pi.json",
    ),
  )
}

fn step_attempt_finished_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-finished-" <> suffix,
    7005,
    record.StepAttemptFinished(
      "workflow-run-1",
      "default",
      "build",
      1,
      "completed",
      "runs/workflow-run-1/build.json",
      "sha256",
      "default",
      "test/tmp/workspaces/ABC-1",
      12,
      3,
    ),
  )
}

fn step_attempt_interrupted_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-interrupted-" <> suffix,
    7006,
    record.StepAttemptInterrupted(
      "workflow-run-1",
      "default",
      "build",
      1,
      "operator_abort",
    ),
  )
}

fn step_attempt_superseded_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "step-attempt-superseded-" <> suffix,
    7007,
    record.StepAttemptSuperseded(
      "workflow-run-1",
      "default",
      "build",
      1,
      2,
      "retry",
    ),
  )
}

fn workflow_run_started_record(suffix: String) -> record.LedgerRecord {
  record.with_id(
    "workflow-run-started-" <> suffix,
    8000,
    record.WorkflowRunStarted(
      "run-1",
      "execplan",
      "wf-1",
      "issue-1",
      "LIV-739",
      "issue-fingerprint",
      7999,
      "root/run-1",
    ),
  )
}

fn publication_attempt_record(
  record_id: String,
  status: String,
) -> record.LedgerRecord {
  record.with_id(
    record_id,
    8001,
    record.PublicationAttemptRecorded(
      run_id: "run-1",
      workflow_id: "execplan",
      publication_id: "review_doc",
      series_id: "task-1:execplan:review_doc",
      attempt_id: "version-1",
      status: status,
      required: True,
      retryable: status == "failed",
      retry_execution_available: False,
      version_id: Some("version-1"),
      manifest_ref: Some("runs/run-1/publications/review_doc/version-1.json"),
      manifest_sha256: Some("sha-1"),
      manifest_bytes: Some(10),
      error_code: case status == "failed" {
        True -> Some("unknown_output")
        False -> None
      },
      error_message: case status == "failed" {
        True -> Some("missing")
        False -> None
      },
    ),
  )
}
