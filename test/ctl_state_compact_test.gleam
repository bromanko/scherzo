import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/file as control_file
import scherzo/ctl/state_handlers
import scherzo/instance_lock
import scherzo/path as scherzo_path
import scherzo/state/ledger
import scherzo/state/record
import simplifile
import support/test_helpers
import test_async

type OutMsg {
  OutLine(String)
}

type CompactJson {
  CompactJson(
    command: String,
    status: String,
    workspace_root: String,
    ledger_dir: String,
    current_path: String,
    snapshot_path: String,
    archive_dir: String,
    before: Option(CompactDetailsJson),
    after: Option(CompactDetailsJson),
    would_archive_current: Option(Bool),
    reason: Option(String),
    message: String,
  )
}

type CompactDetailsJson {
  CompactDetailsJson(
    current_exists: Bool,
    current_size_bytes: Int,
    current_record_count: Int,
    current_truncated_tail: Bool,
    snapshot_exists: Bool,
    snapshot_size_bytes: Int,
    archive_segment_count: Int,
  )
}

pub fn state_compact_dry_run_reports_without_modifying_ledger_test() {
  let root = "test/tmp/ctl-state-compact/dry-run"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, initial_records(), False)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let assert Error(simplifile.Enoent) = simplifile.read(path.snapshot_path)
  let assert Ok(archive_before) = simplifile.read_directory(path.archive_dir)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: False,
      dry_run: True,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let transcript = drain_output(subject)
  assert string.contains(transcript, "state compact dry_run")
  assert string.contains(transcript, "dry-run; ledger files were not modified")
  assert string.contains(transcript, "current_path: " <> path.current_path)
  assert string.contains(transcript, "snapshot_path: " <> path.snapshot_path)
  assert string.contains(transcript, "archive_dir: " <> path.archive_dir)
  assert string.contains(transcript, "would_archive_current: true")
  assert string.contains(transcript, "current_record_count_before: 3")

  let assert Ok(current_after) = simplifile.read(path.current_path)
  let assert Error(simplifile.Enoent) = simplifile.read(path.snapshot_path)
  let assert Ok(archive_after) = simplifile.read_directory(path.archive_dir)
  assert current_after == current_before
  assert archive_after == archive_before
}

pub fn state_compact_yes_compacts_and_preserves_projection_test() {
  let root = "test/tmp/ctl-state-compact/apply"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, initial_records(), False)
  let assert Ok(before) = ledger.load_projection(path)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.command == "state compact"
  assert decoded.status == "compacted"
  assert decoded.workspace_root == root
  assert decoded.ledger_dir == path.ledger_dir
  assert decoded.current_path == path.current_path
  assert decoded.snapshot_path == path.snapshot_path
  assert decoded.archive_dir == path.archive_dir
  assert decoded.would_archive_current == Some(True)
  assert decoded.reason == None
  assert decoded.message == "ledger compaction completed"
  let assert Some(before_details) = decoded.before
  assert before_details.current_exists
  assert before_details.current_size_bytes > 0
  assert before_details.current_record_count == 3
  assert !before_details.current_truncated_tail
  assert !before_details.snapshot_exists
  assert before_details.snapshot_size_bytes == 0
  assert before_details.archive_segment_count == 0
  let assert Some(after_details) = decoded.after
  assert after_details.current_exists
  assert after_details.current_size_bytes == 0
  assert after_details.current_record_count == 0
  assert !after_details.current_truncated_tail
  assert after_details.snapshot_exists
  assert after_details.snapshot_size_bytes > 0
  assert after_details.archive_segment_count == 1

  let assert Ok(after) = ledger.load_projection(path)
  assert after == before
  let assert Ok(current_contents) = simplifile.read(path.current_path)
  assert current_contents == ""
  let assert Ok(True) = simplifile.is_file(path.snapshot_path)
  let assert Ok(archive_entries) = simplifile.read_directory(path.archive_dir)
  assert list.length(archive_entries) == 1
}

pub fn state_compact_rejects_missing_handler_confirmation_test() {
  let root = "test/tmp/ctl-state-compact/missing-confirmation"
  test_helpers.reset_dir(root)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: False,
      dry_run: False,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let transcript = drain_output(subject)
  assert string.contains(transcript, "state compact rejected")
  assert string.contains(
    transcript,
    "message: pass --dry-run to inspect or --yes to compact",
  )
  assert string.contains(transcript, "reason: confirmation_required")
}

pub fn state_compact_rejects_conflicting_handler_confirmation_json_test() {
  let root = "test/tmp/ctl-state-compact/conflicting-confirmation"
  test_helpers.reset_dir(root)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: True,
      yes: True,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "rejected"
  assert decoded.reason == Some("confirmation_conflict")
  assert decoded.message == "pass exactly one of --dry-run or --yes"
  assert decoded.before == None
  assert decoded.after == None
  assert decoded.would_archive_current == None
}

pub fn state_compact_rejects_unsafe_root_before_inspection_test() {
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      "/",
      json_output: True,
      dry_run: True,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "rejected"
  assert decoded.reason == Some("unsafe_state_path")
  assert string.contains(
    decoded.message,
    "workspace root must not be empty or filesystem root",
  )
  assert decoded.before == None
  assert decoded.after == None
}

pub fn state_compact_rejects_symlinked_ledger_path_test() {
  let fixture = "test/tmp/ctl-state-compact/symlinked-ledger"
  let root = fixture <> "/workspace"
  let outside = fixture <> "/outside-ledger"
  test_helpers.reset_dir(fixture)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(outside_abs) = scherzo_path.absolute(outside)
  let assert Ok(Nil) =
    scherzo_path.symlink(outside_abs, root <> "/.scherzo-state/ledger")
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: True,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "rejected"
  assert decoded.reason == Some("unsafe_state_path")
  assert string.contains(decoded.message, "candidate path includes a symlink")
  assert decoded.before == None
  assert decoded.after == None
}

pub fn state_compact_dry_run_rejects_held_instance_lock_test() {
  let root = "test/tmp/ctl-state-compact/lock-held-dry-run"
  test_helpers.reset_dir(root)
  let assert Ok(lock) = instance_lock.acquire(root)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: True,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "rejected"
  assert decoded.reason == Some("instance_lock_held")
  assert string.contains(decoded.message, "instance lock already exists")
  instance_lock.release(lock)
}

pub fn state_compact_yes_rejects_live_control_file_test() {
  let root = "test/tmp/ctl-state-compact/control-file-present"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(path, initial_records(), False)
  let control_path = control_file.path_for_workspace(root)
  let assert Ok(Nil) =
    control_file.write(
      control_path,
      control_file.ControlFile(
        host: "127.0.0.1",
        port: 4010,
        token: "token",
        workspace_root: root,
        started_at_ms: 1000,
        command_timeout_ms: 1000,
      ),
    )
  let before_current = simplifile.read(path.current_path)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "failed"
  assert decoded.reason == Some("daemon_control_file_present")
  assert string.contains(decoded.message, ".scherzo-state/control.json")
  assert simplifile.read(path.current_path) == before_current
}

pub fn state_compact_yes_rejects_held_instance_lock_test() {
  let root = "test/tmp/ctl-state-compact/lock-held-yes"
  test_helpers.reset_dir(root)
  let assert Ok(lock) = instance_lock.acquire(root)
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: False,
      dry_run: False,
      yes: True,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let transcript = drain_output(subject)
  assert string.contains(transcript, "state compact rejected")
  assert string.contains(transcript, "reason: instance_lock_held")
  assert string.contains(transcript, "instance lock already exists")
  instance_lock.release(lock)
}

pub fn state_compact_dry_run_reports_invalid_snapshot_test() {
  let root = "test/tmp/ctl-state-compact/invalid-snapshot"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(path.archive_dir)
  let assert Ok(Nil) = simplifile.write(path.current_path, "")
  let assert Ok(Nil) =
    simplifile.write(path.snapshot_path, "{\"schema_version\":")
  let subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: True,
      yes: False,
      line: subject_line(subject),
    )
    == Ok(Nil)

  let decoded = expect_compact_json(subject)
  assert decoded.status == "failed"
  assert decoded.reason == Some("ledger_inspect_failed")
  assert string.contains(decoded.message, "corrupt ledger record at line 0")
  assert decoded.before == None
  assert decoded.after == None
}

fn subject_line(subject: process.Subject(OutMsg)) -> fn(String) -> Nil {
  fn(text) {
    process.send(subject, OutLine(text))
    Nil
  }
}

fn drain_output(subject: process.Subject(OutMsg)) -> String {
  subject
  |> test_async.drain_subject
  |> list.map(fn(message) {
    let OutLine(text) = message
    text
  })
  |> string.join(with: "\n")
}

fn expect_compact_json(subject: process.Subject(OutMsg)) -> CompactJson {
  let OutLine(line) = test_async.expect_message(subject)
  test_async.assert_no_extra_message(subject)
  let assert Ok(decoded) = json.parse(line, compact_json_decoder())
  decoded
}

fn compact_json_decoder() -> decode.Decoder(CompactJson) {
  use command <- decode.field("command", decode.string)
  use status <- decode.field("status", decode.string)
  use workspace_root <- decode.field("workspace_root", decode.string)
  use ledger_dir <- decode.field("ledger_dir", decode.string)
  use current_path <- decode.field("current_path", decode.string)
  use snapshot_path <- decode.field("snapshot_path", decode.string)
  use archive_dir <- decode.field("archive_dir", decode.string)
  use before <- decode.field(
    "before",
    decode.optional(compact_details_json_decoder()),
  )
  use after <- decode.field(
    "after",
    decode.optional(compact_details_json_decoder()),
  )
  use would_archive_current <- decode.field(
    "would_archive_current",
    decode.optional(decode.bool),
  )
  use reason <- decode.field("reason", decode.optional(decode.string))
  use message <- decode.field("message", decode.string)
  decode.success(CompactJson(
    command: command,
    status: status,
    workspace_root: workspace_root,
    ledger_dir: ledger_dir,
    current_path: current_path,
    snapshot_path: snapshot_path,
    archive_dir: archive_dir,
    before: before,
    after: after,
    would_archive_current: would_archive_current,
    reason: reason,
    message: message,
  ))
}

fn compact_details_json_decoder() -> decode.Decoder(CompactDetailsJson) {
  use current_exists <- decode.field("current_exists", decode.bool)
  use current_size_bytes <- decode.field("current_size_bytes", decode.int)
  use current_record_count <- decode.field("current_record_count", decode.int)
  use current_truncated_tail <- decode.field(
    "current_truncated_tail",
    decode.bool,
  )
  use snapshot_exists <- decode.field("snapshot_exists", decode.bool)
  use snapshot_size_bytes <- decode.field("snapshot_size_bytes", decode.int)
  use archive_segment_count <- decode.field("archive_segment_count", decode.int)
  decode.success(CompactDetailsJson(
    current_exists: current_exists,
    current_size_bytes: current_size_bytes,
    current_record_count: current_record_count,
    current_truncated_tail: current_truncated_tail,
    snapshot_exists: snapshot_exists,
    snapshot_size_bytes: snapshot_size_bytes,
    archive_segment_count: archive_segment_count,
  ))
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
        delay_ms: 10_000,
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
