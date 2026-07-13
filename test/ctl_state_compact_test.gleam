import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/file as control_file
import scherzo/ctl/state_handlers
import scherzo/instance_lock
import scherzo/path as scherzo_path
import scherzo/state/ledger
import scherzo/state/projection
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
  assert string.contains(transcript, "policy_fingerprint=")
  assert string.contains(transcript, "enabled=false")
  assert string.contains(transcript, "terminal_grace_ms=86400000")
  assert string.contains(transcript, "scheduled_max_age_ms=604800000")
  assert string.contains(transcript, "scheduled_last_per_job=25")
  assert string.contains(transcript, "coverage_status=disabled")
  assert string.contains(transcript, "candidates=0")
  assert string.contains(transcript, "blockers=")
  assert string.contains(transcript, "families=")
  assert string.contains(transcript, "projected_bytes=")
  assert string.contains(transcript, "reconstruction_raw_input_bytes=")
  assert string.contains(transcript, "reconstruction_required_memory_bytes=")
  assert string.contains(transcript, "reconstruction_required_disk_bytes=")
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
  assert string.starts_with(decoded.message, "ledger compaction completed")
  assert string.contains(decoded.message, "; policy_fingerprint=")
  assert string.contains(decoded.message, "; enabled=false")
  assert string.contains(decoded.message, "; terminal_grace_ms=86400000")
  assert string.contains(decoded.message, "; scheduled_max_age_ms=604800000")
  assert string.contains(decoded.message, "; scheduled_last_per_job=25")
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

pub fn state_compact_enabled_workspace_policy_preview_matches_apply_test() {
  let root = "test/tmp/ctl-state-compact/enabled-policy"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(path, terminal_workflow_records(), False)
  let assert Ok(Nil) = write_retention_config(root, True)
  let assert Ok(current_before) = simplifile.read(path.current_path)
  let preview_subject = process.new_subject()

  assert state_handlers.run_compact(
      root,
      json_output: False,
      dry_run: True,
      yes: False,
      line: subject_line(preview_subject),
    )
    == Ok(Nil)
  let preview = drain_output(preview_subject)
  assert string.contains(preview, "enabled=true")
  assert string.contains(preview, "terminal_grace_ms=0")
  assert string.contains(preview, "scheduled_max_age_ms=172800000")
  assert string.contains(preview, "scheduled_last_per_job=9")
  assert string.contains(preview, "candidates=1")
  let assert Ok(current_after_preview) = simplifile.read(path.current_path)
  assert current_after_preview == current_before

  let apply_subject = process.new_subject()
  assert state_handlers.run_compact(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      line: subject_line(apply_subject),
    )
    == Ok(Nil)
  let applied = expect_compact_json(apply_subject)
  assert applied.status == "compacted"
  assert string.contains(applied.message, "enabled=true")
  assert string.contains(applied.message, "terminal_grace_ms=0")
  assert string.contains(applied.message, "scheduled_max_age_ms=172800000")
  assert string.contains(applied.message, "scheduled_last_per_job=9")
  assert string.contains(applied.message, "candidates=1")
  assert string.contains(applied.message, "pruned=1")
  assert message_field(preview, "policy_fingerprint")
    == message_field(applied.message, "policy_fingerprint")
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Pruned)
}

pub fn state_compact_rebuild_preflight_failure_reports_resource_estimate_test() {
  let root = "test/tmp/ctl-state-compact/rebuild-resource-failure"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(path, terminal_workflow_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 0, 172_800_000, 9)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 3000 })
  let assert Ok(Nil) = write_retention_config(root, False)
  let assert Ok(snapshot_before) = simplifile.read(path.snapshot_path)
  let assert Ok(current_before) = simplifile.read(path.current_path)

  let memory_subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild_and_probes(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      memory_probe: fn() { Ok(0) },
      disk_probe: fn() { Ok(1_000_000_000_000_000) },
      line: subject_line(memory_subject),
    )
    == Ok(Nil)
  let memory_failure = expect_compact_json(memory_subject)
  assert memory_failure.status == "failed"
  assert memory_failure.reason == Some("io")
  assert string.contains(memory_failure.message, "raw_input_bytes=")
  assert string.contains(memory_failure.message, "estimated_output_bytes=")
  assert string.contains(memory_failure.message, "required_memory_bytes=")
  assert string.contains(memory_failure.message, "required_disk_bytes=")

  let disk_subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild_and_probes(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      memory_probe: fn() { Ok(1_000_000_000_000_000) },
      disk_probe: fn() { Ok(0) },
      line: subject_line(disk_subject),
    )
    == Ok(Nil)
  let disk_failure = expect_compact_json(disk_subject)
  assert disk_failure.status == "failed"
  assert disk_failure.reason == Some("io")
  assert string.contains(disk_failure.message, "raw_input_bytes=")
  assert string.contains(disk_failure.message, "estimated_output_bytes=")
  let assert Ok(snapshot_after) = simplifile.read(path.snapshot_path)
  let assert Ok(current_after) = simplifile.read(path.current_path)
  assert snapshot_after == snapshot_before
  assert current_after == current_before
  assert ledger.workflow_run_presence(path, "workflow-run-1")
    == Ok(ledger.Pruned)
}

pub fn state_compact_rebuild_from_archives_restores_pruned_history_test() {
  let root = "test/tmp/ctl-state-compact/rebuild-success"
  test_helpers.reset_dir(root)
  let assert Ok(path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(path, terminal_workflow_records(), False)
  let enabled = config_types.ProjectionRetentionConfig(True, 0, 172_800_000, 9)
  let assert Ok(_) = ledger.compact_with_retention(path, enabled, fn() { 3000 })
  let assert Ok(Nil) = write_retention_config(root, False)
  let assert Ok(before_rebuild) = ledger.load_projection(path)
  let assert Error(_) =
    projection.workflow_run(before_rebuild, "workflow-run-1")

  let missing_confirmation = process.new_subject()
  assert state_handlers.run_compact_with_rebuild(
      root,
      json_output: True,
      dry_run: False,
      yes: False,
      rebuild_from_archives: True,
      line: subject_line(missing_confirmation),
    )
    == Ok(Nil)
  let rejected = expect_compact_json(missing_confirmation)
  assert rejected.status == "rejected"
  assert rejected.reason == Some("confirmation_required")
  let assert Error(_) =
    projection.workflow_run(result_projection(path), "workflow-run-1")

  let subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild(
      root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      line: subject_line(subject),
    )
    == Ok(Nil)
  let rebuilt = expect_compact_json(subject)
  assert rebuilt.status == "rebuilt"
  assert rebuilt.reason == None
  assert string.contains(
    rebuilt.message,
    "retain-all archive reconstruction completed",
  )
  let assert Ok(_) =
    projection.workflow_run(result_projection(path), "workflow-run-1")
}

pub fn state_compact_rebuild_rejects_lock_enabled_policy_and_missing_coverage_test() {
  let lock_root = "test/tmp/ctl-state-compact/rebuild-lock"
  test_helpers.reset_dir(lock_root)
  let assert Ok(Nil) = write_retention_config(lock_root, False)
  let assert Ok(lock) = instance_lock.acquire(lock_root)
  let lock_subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild(
      lock_root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      line: subject_line(lock_subject),
    )
    == Ok(Nil)
  let lock_rejection = expect_compact_json(lock_subject)
  assert lock_rejection.status == "rejected"
  assert lock_rejection.reason == Some("instance_lock_held")
  instance_lock.release(lock)

  let enabled_root = "test/tmp/ctl-state-compact/rebuild-enabled"
  test_helpers.reset_dir(enabled_root)
  let assert Ok(enabled_path) = ledger.path_for_workspace_root(enabled_root)
  let assert Ok(Nil) =
    ledger.append_many(enabled_path, terminal_workflow_records(), False)
  let assert Ok(Nil) = write_retention_config(enabled_root, True)
  let assert Ok(enabled_current_before) =
    simplifile.read(enabled_path.current_path)
  let enabled_subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild(
      enabled_root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      line: subject_line(enabled_subject),
    )
    == Ok(Nil)
  let enabled_rejection = expect_compact_json(enabled_subject)
  assert enabled_rejection.status == "failed"
  assert enabled_rejection.reason == Some("io")
  assert string.contains(
    enabled_rejection.message,
    "requires projection retention to be disabled",
  )
  let assert Ok(enabled_current_after) =
    simplifile.read(enabled_path.current_path)
  assert enabled_current_after == enabled_current_before

  let coverage_root = "test/tmp/ctl-state-compact/rebuild-missing-coverage"
  test_helpers.reset_dir(coverage_root)
  let assert Ok(coverage_path) = ledger.path_for_workspace_root(coverage_root)
  let assert Ok(Nil) =
    ledger.append_many(coverage_path, terminal_workflow_records(), False)
  let assert Ok(Nil) = write_retention_config(coverage_root, False)
  let assert Ok(coverage_current_before) =
    simplifile.read(coverage_path.current_path)
  let coverage_subject = process.new_subject()
  assert state_handlers.run_compact_with_rebuild(
      coverage_root,
      json_output: True,
      dry_run: False,
      yes: True,
      rebuild_from_archives: True,
      line: subject_line(coverage_subject),
    )
    == Ok(Nil)
  let coverage_rejection = expect_compact_json(coverage_subject)
  assert coverage_rejection.status == "failed"
  assert coverage_rejection.reason == Some("io")
  assert string.contains(
    coverage_rejection.message,
    "coverage manifest is absent",
  )
  let assert Ok(coverage_current_after) =
    simplifile.read(coverage_path.current_path)
  assert coverage_current_after == coverage_current_before
  let assert Error(simplifile.Enoent) =
    simplifile.read(coverage_path.snapshot_path)
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

fn message_field(message: String, field: String) -> String {
  let assert Ok(#(_, suffix)) = string.split_once(message, on: field <> "=")
  case string.split_once(suffix, on: ";") {
    Ok(#(value, _)) -> value
    Error(Nil) -> suffix
  }
}

fn result_projection(path: ledger.LedgerPath) -> projection.Projection {
  let assert Ok(projected) = ledger.load_projection(path)
  projected
}

fn write_retention_config(root: String, enabled: Bool) {
  simplifile.write(
    root <> "/scherzo.yaml",
    "version: 1\n"
      <> "tracker:\n"
      <> "  api_key: test-api-key\n"
      <> "  linear:\n"
      <> "    project: demo-project\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n"
      <> "state_ledger:\n"
      <> "  projection_retention:\n"
      <> "    enabled: "
      <> case enabled {
      True -> "true\n"
      False -> "false\n"
    }
      <> "    terminal_grace: 0ms\n"
      <> "    scheduled_max_age: 48h\n"
      <> "    scheduled_last_per_job: 9\n",
  )
}

fn terminal_workflow_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "workflow-run-started",
      1000,
      record.WorkflowRunStartedWithTask(
        "workflow-run-1",
        "default",
        "workflow-fingerprint",
        "issue-1",
        "ABC-1",
        record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
        "issue-fingerprint",
        900,
        "test/tmp/workflow-run-1",
      ),
    ),
    record.with_id(
      "workflow-run-finished",
      2000,
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
