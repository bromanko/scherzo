import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/file as control_file
import scherzo/error
import scherzo/instance_lock
import scherzo/path
import scherzo/state/ledger
import scherzo/state/local_artifacts
import scherzo/state/projection
import scherzo/state/projection/retention
import scherzo/state/record
import scherzo/workflow_repair
import simplifile
import yay

type StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: String,
    run_id: String,
    repair_status: String,
    repair_mode: String,
    source_evidence: List(String),
    reason: Option(String),
    message: Option(String),
  )
}

type StateCompactDetails {
  StateCompactDetails(
    current_exists: Bool,
    current_size_bytes: Int,
    current_record_count: Int,
    current_truncated_tail: Bool,
    snapshot_exists: Bool,
    snapshot_size_bytes: Int,
    archive_segment_count: Int,
  )
}

type StateCompactResult {
  StateCompactResult(
    status: String,
    workspace_root: String,
    ledger_dir: String,
    current_path: String,
    snapshot_path: String,
    archive_dir: String,
    before: Option(StateCompactDetails),
    after: Option(StateCompactDetails),
    would_archive_current: Option(Bool),
    reason: Option(String),
    message: String,
  )
}

type StateCompactInspectError {
  CompactLedgerReadFailed(error: ledger.LedgerError)
  CompactFileInfoFailed(path: String, error: simplifile.FileError)
}

pub fn run_status(
  root: String,
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let status = local_artifacts.inspect_state(root)
  case json_output {
    True ->
      output_line(
        status |> local_artifacts.state_status_to_json |> json.to_string,
      )
    False -> print_state_status(status, output_line)
  }
  Ok(Nil)
}

pub fn run_archive_old(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result =
    local_artifacts.archive_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_discard_old(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result =
    local_artifacts.discard_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_reinitialize(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result = local_artifacts.reinitialize_state(root, yes: yes)
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_compact(
  root: String,
  json_output json_output: Bool,
  dry_run dry_run: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  run_compact_with_rebuild(
    root,
    json_output: json_output,
    dry_run: dry_run,
    yes: yes,
    rebuild_from_archives: False,
    line: output_line,
  )
}

pub fn run_compact_with_rebuild(
  root: String,
  json_output json_output: Bool,
  dry_run dry_run: Bool,
  yes yes: Bool,
  rebuild_from_archives rebuild_from_archives: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  run_compact_with_rebuild_capability(
    root,
    json_output,
    dry_run,
    yes,
    rebuild_from_archives,
    ledger.rebuild_from_archives,
    output_line,
  )
}

pub fn run_compact_with_rebuild_and_probes(
  root: String,
  json_output json_output: Bool,
  dry_run dry_run: Bool,
  yes yes: Bool,
  rebuild_from_archives rebuild_from_archives: Bool,
  memory_probe memory_probe: fn() -> Result(Int, String),
  disk_probe disk_probe: fn() -> Result(Int, String),
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  run_compact_with_rebuild_capability(
    root,
    json_output,
    dry_run,
    yes,
    rebuild_from_archives,
    fn(ledger_path, policy) {
      ledger.rebuild_from_archives_with_probes(
        ledger_path,
        policy,
        memory_probe,
        disk_probe,
      )
    },
    output_line,
  )
}

fn run_compact_with_rebuild_capability(
  root: String,
  json_output: Bool,
  dry_run: Bool,
  yes: Bool,
  rebuild_from_archives: Bool,
  rebuild: fn(ledger.LedgerPath, config_types.ProjectionRetentionConfig) ->
    Result(ledger.RebuildReport, ledger.LedgerError),
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result = state_compact(root, dry_run, yes, rebuild_from_archives, rebuild)
  print_state_compact(result, json_output, output_line)
  Ok(Nil)
}

fn state_compact(
  root: String,
  dry_run: Bool,
  yes: Bool,
  rebuild_from_archives: Bool,
  rebuild: fn(ledger.LedgerPath, config_types.ProjectionRetentionConfig) ->
    Result(ledger.RebuildReport, ledger.LedgerError),
) -> StateCompactResult {
  case dry_run, yes, rebuild_from_archives {
    True, _, True ->
      rejected_state_compact_result(
        root,
        "rebuild_requires_confirmation",
        "--rebuild-from-archives requires --yes and cannot be combined with --dry-run",
      )
    False, False, True ->
      rejected_state_compact_result(
        root,
        "confirmation_required",
        "--rebuild-from-archives requires --yes",
      )
    True, True, False ->
      rejected_state_compact_result(
        root,
        "confirmation_conflict",
        "pass exactly one of --dry-run or --yes",
      )
    False, False, False ->
      rejected_state_compact_result(
        root,
        "confirmation_required",
        "pass --dry-run to inspect or --yes to compact",
      )
    True, False, False ->
      case load_retention_policy(root) {
        Ok(policy) -> inspect_state_compact(root, policy)
        Error(reason) ->
          rejected_state_compact_result(root, "config_invalid", reason)
      }
    False, True, False ->
      case load_retention_policy(root) {
        Ok(policy) -> apply_state_compact(root, policy)
        Error(reason) ->
          rejected_state_compact_result(root, "config_invalid", reason)
      }
    False, True, True ->
      case load_retention_policy(root) {
        Ok(policy) -> apply_archive_rebuild(root, policy, rebuild)
        Error(reason) ->
          rejected_state_compact_result(root, "config_invalid", reason)
      }
  }
}

// nolint: stringly_typed_error -- private loader reason is rendered directly by this CLI boundary
fn load_retention_policy(
  root: String,
) -> Result(config_types.ProjectionRetentionConfig, String) {
  let config_path = path.join(root, "scherzo.yaml")
  case simplifile.read(config_path) {
    Error(simplifile.Enoent) -> Ok(config.default_projection_retention_config())
    Error(file_error) ->
      Error("read workspace config: " <> simplifile.describe_error(file_error))
    Ok(contents) ->
      case yay.parse_string(contents) {
        Ok([document]) ->
          config.resolve_with_env(
            yay.document_root(document),
            config_path,
            path.env,
          )
          |> result.map(fn(effective) {
            effective.ledger_compaction.projection_retention
          })
          |> result.map_error(error.config_message)
        Ok(_) ->
          Error("workspace config must contain exactly one YAML document")
        Error(_) -> Error("workspace config contains invalid YAML")
      }
  }
}

fn inspect_state_compact(
  root: String,
  policy: config_types.ProjectionRetentionConfig,
) -> StateCompactResult {
  case compact_ledger_path(root) {
    Error(result) -> result
    Ok(ledger_path) ->
      with_state_compact_lock(ledger_path, fn() {
        case inspect_compaction_details(ledger_path) {
          Error(error) ->
            failed_state_compact_result(
              ledger_path,
              None,
              None,
              "ledger_inspect_failed",
              compact_inspect_error_message(error),
            )
          Ok(before) ->
            case
              ledger.preview_compaction(
                ledger_path,
                policy,
                local_artifacts.now_ms(),
              )
            {
              Error(error) ->
                failed_state_compact_result(
                  ledger_path,
                  Some(before),
                  Some(before.current_size_bytes > 0),
                  ledger.ledger_error_code(error),
                  ledger_error_message(error),
                )
              Ok(preview) ->
                StateCompactResult(
                  status: "dry_run",
                  workspace_root: ledger_path.workspace_root,
                  ledger_dir: ledger_path.ledger_dir,
                  current_path: ledger_path.current_path,
                  snapshot_path: ledger_path.snapshot_path,
                  archive_dir: ledger_path.archive_dir,
                  before: Some(before),
                  after: None,
                  would_archive_current: Some(before.current_size_bytes > 0),
                  reason: None,
                  message: compaction_preview_message(preview),
                )
            }
        }
      })
  }
}

fn compaction_preview_message(preview: ledger.CompactionPreview) -> String {
  let policy = preview.policy
  let report = preview.prune_report
  let families = report.families_removed
  let blockers = report.blockers
  let estimate = preview.reconstruction_estimate
  "dry-run; ledger files were not modified"
  <> retention_policy_text(policy, preview.policy_fingerprint)
  <> "; coverage_status="
  <> preview.coverage_status
  <> "; candidates="
  <> int.to_string(list.length(preview.candidate_run_ids))
  <> "; blockers="
  <> blocker_counts_text(blockers)
  <> "; families="
  <> family_counts_text(families)
  <> "; before_bytes="
  <> int.to_string(report.before_bytes)
  <> "; projected_bytes="
  <> int.to_string(report.after_bytes)
  <> "; reconstruction_raw_input_bytes="
  <> int.to_string(estimate.raw_input_bytes)
  <> "; reconstruction_estimated_output_bytes="
  <> int.to_string(estimate.estimated_output_bytes)
  <> "; reconstruction_required_memory_bytes="
  <> int.to_string(estimate.required_memory_bytes)
  <> "; reconstruction_required_disk_bytes="
  <> int.to_string(estimate.required_disk_bytes)
}

fn retention_policy_text(
  policy: config_types.ProjectionRetentionConfig,
  fingerprint: String,
) -> String {
  "; policy_fingerprint="
  <> fingerprint
  <> "; enabled="
  <> bool_to_string(policy.enabled)
  <> "; terminal_grace_ms="
  <> int.to_string(policy.terminal_grace_ms)
  <> "; scheduled_max_age_ms="
  <> int.to_string(policy.scheduled_max_age_ms)
  <> "; scheduled_last_per_job="
  <> int.to_string(policy.scheduled_last_per_job)
}

fn compaction_completed_message(report: ledger.CompactionReport) -> String {
  "ledger compaction completed"
  <> retention_policy_text(report.policy, report.policy_fingerprint)
  <> "; coverage_status="
  <> report.coverage_status
  <> "; candidates="
  <> int.to_string(list.length(report.candidate_run_ids))
  <> "; pruned="
  <> int.to_string(list.length(report.pruned_run_ids))
}

fn family_counts_text(value: retention.FamilyCounts) -> String {
  [
    int.to_string(value.workflow_runs),
    int.to_string(value.provenances),
    int.to_string(value.task_refs),
    int.to_string(value.input_manifests),
    int.to_string(value.interface_snapshots),
    int.to_string(value.output_manifests),
    int.to_string(value.repairs),
    int.to_string(value.step_attempts),
    int.to_string(value.step_recoveries),
    int.to_string(value.publication_attempts),
    int.to_string(value.control_operations),
    int.to_string(value.outbox_entries),
  ]
  |> string.join(with: ",")
}

fn blocker_counts_text(value: retention.BlockerCounts) -> String {
  [
    int.to_string(value.active),
    int.to_string(value.within_grace),
    int.to_string(value.parked),
    int.to_string(value.retained_workspace),
    int.to_string(value.marker_unavailable),
    int.to_string(value.recovery_started),
    int.to_string(value.control_in_flight),
    int.to_string(value.publication_unsettled),
    int.to_string(value.outbox_unsettled),
    int.to_string(value.malformed_association),
  ]
  |> string.join(with: ",")
}

fn apply_archive_rebuild(
  root: String,
  policy: config_types.ProjectionRetentionConfig,
  rebuild: fn(ledger.LedgerPath, config_types.ProjectionRetentionConfig) ->
    Result(ledger.RebuildReport, ledger.LedgerError),
) -> StateCompactResult {
  case compact_ledger_path(root) {
    Error(result) -> result
    Ok(ledger_path) ->
      case offline_state_mutation_guard(ledger_path.workspace_root) {
        Error(#(reason, message)) ->
          failed_state_compact_result(ledger_path, None, None, reason, message)
        Ok(Nil) ->
          with_state_compact_lock(ledger_path, fn() {
            case inspect_compaction_details(ledger_path) {
              Error(error) ->
                failed_state_compact_result(
                  ledger_path,
                  None,
                  None,
                  "ledger_inspect_failed",
                  compact_inspect_error_message(error),
                )
              Ok(before) ->
                case rebuild(ledger_path, policy) {
                  Error(error) ->
                    failed_state_compact_result(
                      ledger_path,
                      Some(before),
                      Some(before.current_size_bytes > 0),
                      ledger.ledger_error_code(error),
                      ledger_error_message(error),
                    )
                  Ok(report) ->
                    completed_archive_rebuild_result(
                      ledger_path,
                      before,
                      report,
                    )
                }
            }
          })
      }
  }
}

fn completed_archive_rebuild_result(
  ledger_path: ledger.LedgerPath,
  before: StateCompactDetails,
  report: ledger.RebuildReport,
) -> StateCompactResult {
  case inspect_compaction_details(ledger_path) {
    Error(error) ->
      StateCompactResult(
        status: "rebuilt",
        workspace_root: ledger_path.workspace_root,
        ledger_dir: ledger_path.ledger_dir,
        current_path: ledger_path.current_path,
        snapshot_path: ledger_path.snapshot_path,
        archive_dir: ledger_path.archive_dir,
        before: Some(before),
        after: None,
        would_archive_current: Some(before.current_size_bytes > 0),
        reason: Some("post_rebuild_inspect_failed"),
        message: "archive reconstruction completed; failed to inspect after details: "
          <> compact_inspect_error_message(error),
      )
    Ok(after) -> {
      let estimate = report.estimate
      StateCompactResult(
        status: "rebuilt",
        workspace_root: ledger_path.workspace_root,
        ledger_dir: ledger_path.ledger_dir,
        current_path: ledger_path.current_path,
        snapshot_path: ledger_path.snapshot_path,
        archive_dir: ledger_path.archive_dir,
        before: Some(before),
        after: Some(after),
        would_archive_current: Some(before.current_size_bytes > 0),
        reason: None,
        message: "retain-all archive reconstruction completed"
          <> "; raw_input_bytes="
          <> int.to_string(estimate.raw_input_bytes)
          <> "; estimated_output_bytes="
          <> int.to_string(estimate.estimated_output_bytes)
          <> "; required_memory_bytes="
          <> int.to_string(estimate.required_memory_bytes)
          <> "; required_disk_bytes="
          <> int.to_string(estimate.required_disk_bytes)
          <> "; restored_projection_bytes="
          <> int.to_string(report.restored_projection_bytes),
      )
    }
  }
}

fn apply_state_compact(
  root: String,
  policy: config_types.ProjectionRetentionConfig,
) -> StateCompactResult {
  case compact_ledger_path(root) {
    Error(result) -> result
    Ok(ledger_path) ->
      case offline_state_mutation_guard(ledger_path.workspace_root) {
        Error(#(reason, message)) ->
          failed_state_compact_result(ledger_path, None, None, reason, message)
        Ok(Nil) ->
          with_state_compact_lock(ledger_path, fn() {
            case inspect_compaction_details(ledger_path) {
              Error(error) ->
                failed_state_compact_result(
                  ledger_path,
                  None,
                  None,
                  "ledger_inspect_failed",
                  compact_inspect_error_message(error),
                )
              Ok(before) ->
                case
                  ledger.compact_with_retention(
                    ledger_path,
                    policy,
                    local_artifacts.now_ms,
                  )
                {
                  Error(error) ->
                    failed_state_compact_result(
                      ledger_path,
                      Some(before),
                      Some(before.current_size_bytes > 0),
                      ledger.ledger_error_code(error),
                      ledger_error_message(error),
                    )
                  Ok(report) ->
                    case inspect_compaction_details(ledger_path) {
                      Error(error) ->
                        StateCompactResult(
                          status: "compacted",
                          workspace_root: ledger_path.workspace_root,
                          ledger_dir: ledger_path.ledger_dir,
                          current_path: ledger_path.current_path,
                          snapshot_path: ledger_path.snapshot_path,
                          archive_dir: ledger_path.archive_dir,
                          before: Some(before),
                          after: None,
                          would_archive_current: Some(
                            before.current_size_bytes > 0,
                          ),
                          reason: Some("post_compaction_inspect_failed"),
                          message: compaction_completed_message(report)
                            <> "; failed to inspect after details: "
                            <> compact_inspect_error_message(error),
                        )
                      Ok(after) ->
                        StateCompactResult(
                          status: "compacted",
                          workspace_root: ledger_path.workspace_root,
                          ledger_dir: ledger_path.ledger_dir,
                          current_path: ledger_path.current_path,
                          snapshot_path: ledger_path.snapshot_path,
                          archive_dir: ledger_path.archive_dir,
                          before: Some(before),
                          after: Some(after),
                          would_archive_current: Some(
                            before.current_size_bytes > 0,
                          ),
                          reason: None,
                          message: compaction_completed_message(report),
                        )
                    }
                }
            }
          })
      }
  }
}

fn offline_state_mutation_guard(
  root: String,
) -> Result(Nil, #(String, String)) {
  let control_path = control_file.path_for_workspace(root)
  case simplifile.is_file(control_path) {
    Ok(True) ->
      Error(#(
        "daemon_control_file_present",
        "stop the daemon before mutating offline state; control file present at "
          <> control_path,
      ))
    Ok(False) | Error(simplifile.Enoent) -> Ok(Nil)
    Error(error) ->
      Error(#(
        "daemon_control_file_check_failed",
        "failed to inspect daemon control file: "
          <> simplifile.describe_error(error),
      ))
  }
}

fn inspect_compaction_details(
  ledger_path: ledger.LedgerPath,
) -> Result(StateCompactDetails, StateCompactInspectError) {
  use current_file <- result.try(file_size(ledger_path.current_path))
  use storage_stats <- result.try(
    ledger.storage_stats(ledger_path)
    |> result.map_error(CompactLedgerReadFailed),
  )
  let #(current_exists, current_size_bytes) = current_file
  let snapshot_exists = storage_stats.snapshot_size_bytes > 0
  Ok(StateCompactDetails(
    current_exists: current_exists,
    current_size_bytes: current_size_bytes,
    current_record_count: storage_stats.current.record_count,
    current_truncated_tail: storage_stats.current.truncated_tail,
    snapshot_exists: snapshot_exists,
    snapshot_size_bytes: storage_stats.snapshot_size_bytes,
    archive_segment_count: storage_stats.archive_segment_count,
  ))
}

fn file_size(
  file_path: String,
) -> Result(#(Bool, Int), StateCompactInspectError) {
  case simplifile.file_info(file_path) {
    Ok(info) -> Ok(#(True, info.size))
    Error(simplifile.Enoent) -> Ok(#(False, 0))
    Error(error) -> Error(CompactFileInfoFailed(file_path, error))
  }
}

fn compact_ledger_path(
  root: String,
) -> Result(ledger.LedgerPath, StateCompactResult) {
  case ledger.path_for_workspace_root(root) {
    Error(error) ->
      failed_state_compact_without_path(
        root,
        "ledger_path_failed",
        ledger_error_message(error),
      )
      |> Error
    Ok(ledger_path) ->
      case compact_path_safety_error(ledger_path) {
        Some(reason) ->
          unsafe_state_compact_result(ledger_path, reason) |> Error
        None -> Ok(ledger_path)
      }
  }
}

fn with_state_compact_lock(
  ledger_path: ledger.LedgerPath,
  action: fn() -> StateCompactResult,
) -> StateCompactResult {
  case instance_lock.acquire(ledger_path.workspace_root) {
    Ok(lock) -> {
      let result = action()
      instance_lock.release(lock)
      result
    }
    Error(instance_lock.LockAlreadyHeld(message)) ->
      StateCompactResult(
        status: "rejected",
        workspace_root: ledger_path.workspace_root,
        ledger_dir: ledger_path.ledger_dir,
        current_path: ledger_path.current_path,
        snapshot_path: ledger_path.snapshot_path,
        archive_dir: ledger_path.archive_dir,
        before: None,
        after: None,
        would_archive_current: None,
        reason: Some("instance_lock_held"),
        message: message,
      )
    Error(error) ->
      StateCompactResult(
        status: "rejected",
        workspace_root: ledger_path.workspace_root,
        ledger_dir: ledger_path.ledger_dir,
        current_path: ledger_path.current_path,
        snapshot_path: ledger_path.snapshot_path,
        archive_dir: ledger_path.archive_dir,
        before: None,
        after: None,
        would_archive_current: None,
        reason: Some("instance_lock_failed"),
        message: instance_lock.error_message(error),
      )
  }
}

fn compact_path_safety_error(ledger_path: ledger.LedgerPath) -> Option(String) {
  case
    [
      ledger_path.ledger_dir,
      ledger_path.current_path,
      ledger_path.snapshot_path,
      ledger_path.snapshot_path <> ".tmp",
      ledger_path.archive_dir,
    ]
    |> list.find_map(fn(candidate) {
      case
        local_artifacts.check_path_safety(ledger_path.workspace_root, candidate)
      {
        local_artifacts.PathUnsafe(reason) -> Ok(reason)
        local_artifacts.PathSafe(_) -> Error(Nil)
      }
    })
  {
    Ok(reason) -> Some(reason)
    Error(Nil) -> None
  }
}

fn compact_inspect_error_message(error: StateCompactInspectError) -> String {
  case error {
    CompactLedgerReadFailed(error) -> ledger_error_message(error)
    CompactFileInfoFailed(file_path, error) ->
      "inspect ledger file "
      <> file_path
      <> ": "
      <> simplifile.describe_error(error)
  }
}

fn rejected_state_compact_result(
  root: String,
  reason: String,
  message: String,
) -> StateCompactResult {
  case ledger.path_for_workspace_root(root) {
    Error(error) ->
      StateCompactResult(
        status: "rejected",
        workspace_root: root,
        ledger_dir: "",
        current_path: "",
        snapshot_path: "",
        archive_dir: "",
        before: None,
        after: None,
        would_archive_current: None,
        reason: Some(reason),
        message: message
          <> "; ledger path unavailable: "
          <> ledger_error_message(error),
      )
    Ok(ledger_path) ->
      StateCompactResult(
        status: "rejected",
        workspace_root: ledger_path.workspace_root,
        ledger_dir: ledger_path.ledger_dir,
        current_path: ledger_path.current_path,
        snapshot_path: ledger_path.snapshot_path,
        archive_dir: ledger_path.archive_dir,
        before: None,
        after: None,
        would_archive_current: None,
        reason: Some(reason),
        message: message,
      )
  }
}

fn unsafe_state_compact_result(
  ledger_path: ledger.LedgerPath,
  safety_reason: String,
) -> StateCompactResult {
  StateCompactResult(
    status: "rejected",
    workspace_root: ledger_path.workspace_root,
    ledger_dir: ledger_path.ledger_dir,
    current_path: ledger_path.current_path,
    snapshot_path: ledger_path.snapshot_path,
    archive_dir: ledger_path.archive_dir,
    before: None,
    after: None,
    would_archive_current: None,
    reason: Some("unsafe_state_path"),
    message: "unsafe state path: " <> safety_reason,
  )
}

fn failed_state_compact_without_path(
  root: String,
  reason: String,
  message: String,
) -> StateCompactResult {
  StateCompactResult(
    status: "failed",
    workspace_root: root,
    ledger_dir: "",
    current_path: "",
    snapshot_path: "",
    archive_dir: "",
    before: None,
    after: None,
    would_archive_current: None,
    reason: Some(reason),
    message: message,
  )
}

fn failed_state_compact_result(
  ledger_path: ledger.LedgerPath,
  before: Option(StateCompactDetails),
  would_archive_current: Option(Bool),
  reason: String,
  message: String,
) -> StateCompactResult {
  StateCompactResult(
    status: "failed",
    workspace_root: ledger_path.workspace_root,
    ledger_dir: ledger_path.ledger_dir,
    current_path: ledger_path.current_path,
    snapshot_path: ledger_path.snapshot_path,
    archive_dir: ledger_path.archive_dir,
    before: before,
    after: None,
    would_archive_current: would_archive_current,
    reason: Some(reason),
    message: message,
  )
}

pub fn run_repair_run_provenance(
  root: String,
  run_id: String,
  json_output json_output: Bool,
  dry_run dry_run: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result = state_repair_run_provenance(root, run_id, dry_run, yes)
  print_state_repair_run_provenance(result, json_output, output_line)
  Ok(Nil)
}

fn state_repair_run_provenance(
  root: String,
  run_id: String,
  dry_run: Bool,
  yes: Bool,
) -> StateRunProvenanceRepairResult {
  case ledger.path_for_workspace_root(root) {
    Error(error) ->
      rejected_state_repair_result(
        run_id,
        "ledger_path_failed",
        ledger_error_message(error),
      )
    Ok(ledger_path) ->
      case ledger.read_records(ledger_path) {
        Error(error) ->
          rejected_state_repair_result(
            run_id,
            "ledger_read_failed",
            ledger_error_message(error),
          )
        Ok(read) -> {
          let projection_state = projection.fold(read.records)
          case
            workflow_repair.inspect_run_provenance_repair(
              projection_state,
              run_id,
              workflow_repair.state_repair_explicit_mode,
            )
          {
            Error(error) ->
              rejected_state_repair_result(
                run_id,
                workflow_repair.describe_error(error),
                repair_error_message_text(error),
              )
            Ok(workflow_repair.RunProvenanceRepairAlreadyPresent(..)) ->
              StateRunProvenanceRepairResult(
                status: "already_repaired",
                run_id: run_id,
                repair_status: "already_repaired",
                repair_mode: workflow_repair.state_repair_explicit_mode,
                source_evidence: [],
                reason: None,
                message: Some("workflow run provenance is already present"),
              )
            Ok(workflow_repair.RunProvenanceRepairRequired(plan)) ->
              case
                workflow_repair.validate_run_root_for_repair(
                  run_id,
                  plan.run_root,
                  root,
                )
              {
                Error(error) ->
                  rejected_state_repair_result(
                    run_id,
                    workflow_repair.describe_error(error),
                    repair_error_message_text(error),
                  )
                Ok(Nil) ->
                  case dry_run, yes {
                    True, _ ->
                      StateRunProvenanceRepairResult(
                        status: "dry_run",
                        run_id: run_id,
                        repair_status: "would_repair",
                        repair_mode: plan.repair_mode,
                        source_evidence: plan.source_evidence,
                        reason: None,
                        message: Some("workflow run provenance can be repaired"),
                      )
                    _, True ->
                      case
                        offline_state_mutation_guard(ledger_path.workspace_root)
                      {
                        Ok(Nil) ->
                          append_state_repair_run_provenance(ledger_path, plan)
                        Error(#(reason, message)) ->
                          rejected_state_repair_result(run_id, reason, message)
                      }
                    _, _ ->
                      rejected_state_repair_result(
                        run_id,
                        "confirmation_required",
                        "pass --dry-run to inspect or --yes to repair",
                      )
                  }
              }
          }
        }
      }
  }
}

fn append_state_repair_run_provenance(
  ledger_path: ledger.LedgerPath,
  plan: workflow_repair.RunProvenanceRepairPlan,
) -> StateRunProvenanceRepairResult {
  let ledger_record =
    record.with_id(
      "workflow-run-provenance-repaired-" <> plan.run_id,
      local_artifacts.now_ms(),
      plan.record_body,
    )
  case ledger.append_idempotent(ledger_path, ledger_record, True) {
    Ok(ledger.Appended) ->
      StateRunProvenanceRepairResult(
        status: "repaired",
        run_id: plan.run_id,
        repair_status: "repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repaired"),
      )
    Ok(ledger.AlreadyRecorded(_)) ->
      StateRunProvenanceRepairResult(
        status: "already_repaired",
        run_id: plan.run_id,
        repair_status: "already_repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repair was already recorded"),
      )
    Error(error) ->
      rejected_state_repair_result(
        plan.run_id,
        "ledger_append_failed",
        append_idempotent_error_message(error),
      )
  }
}

fn repair_error_message_text(error: workflow_repair.RepairError) -> String {
  case workflow_repair.error_message(error) {
    Some(message) -> message
    None -> workflow_repair.describe_error(error)
  }
}

fn rejected_state_repair_result(
  run_id: String,
  reason: String,
  message: String,
) -> StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: "rejected",
    run_id: run_id,
    repair_status: "rejected",
    repair_mode: workflow_repair.state_repair_explicit_mode,
    source_evidence: [],
    reason: Some(reason),
    message: Some(message),
  )
}

fn print_state_repair_run_provenance(
  result: StateRunProvenanceRepairResult,
  json_output: Bool,
  output_line: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True ->
      output_line(
        result
        |> state_repair_run_provenance_to_json
        |> json.to_string,
      )
    False -> {
      output_line("state repair-run-provenance " <> result.status)
      output_line("run_id: " <> result.run_id)
      output_line("repair_status: " <> result.repair_status)
      case result.reason {
        Some(reason) -> output_line("reason: " <> reason)
        None -> Nil
      }
      case result.message {
        Some(message) -> output_line("message: " <> message)
        None -> Nil
      }
      case result.source_evidence {
        [] -> Nil
        _ -> {
          output_line("source_evidence:")
          list.each(result.source_evidence, fn(evidence) {
            output_line("  " <> evidence)
          })
        }
      }
    }
  }
}

fn state_repair_run_provenance_to_json(
  result: StateRunProvenanceRepairResult,
) -> json.Json {
  [
    #("command", json.string("state repair-run-provenance")),
    #("status", json.string(result.status)),
    #("run_id", json.string(result.run_id)),
    #("repair_status", json.string(result.repair_status)),
    #("repair_mode", json.string(result.repair_mode)),
    #("source_evidence", json.array(result.source_evidence, of: json.string)),
    #("reason", optional_string_json(result.reason)),
    #("message", optional_string_json(result.message)),
  ]
  |> json.object
}

fn print_state_compact(
  result: StateCompactResult,
  json_output: Bool,
  output_line: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True -> output_line(result |> state_compact_to_json |> json.to_string)
    False -> {
      output_line("state compact " <> result.status)
      output_line("message: " <> result.message)
      case result.reason {
        Some(reason) -> output_line("reason: " <> reason)
        None -> Nil
      }
      output_line("workspace_root: " <> result.workspace_root)
      output_line("ledger_dir: " <> result.ledger_dir)
      output_line("current_path: " <> result.current_path)
      output_line("snapshot_path: " <> result.snapshot_path)
      output_line("archive_dir: " <> result.archive_dir)
      case result.would_archive_current {
        Some(value) ->
          output_line("would_archive_current: " <> bool_to_string(value))
        None -> Nil
      }
      case result.before {
        Some(before) -> print_compact_details(before, "before", output_line)
        None -> Nil
      }
      case result.after {
        Some(after) -> print_compact_details(after, "after", output_line)
        None -> Nil
      }
    }
  }
}

fn print_compact_details(
  details: StateCompactDetails,
  suffix: String,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line(
    "current_exists_"
    <> suffix
    <> ": "
    <> bool_to_string(details.current_exists),
  )
  output_line(
    "current_size_bytes_"
    <> suffix
    <> ": "
    <> int.to_string(details.current_size_bytes),
  )
  output_line(
    "current_record_count_"
    <> suffix
    <> ": "
    <> int.to_string(details.current_record_count),
  )
  output_line(
    "current_truncated_tail_"
    <> suffix
    <> ": "
    <> bool_to_string(details.current_truncated_tail),
  )
  output_line(
    "snapshot_exists_"
    <> suffix
    <> ": "
    <> bool_to_string(details.snapshot_exists),
  )
  output_line(
    "snapshot_size_bytes_"
    <> suffix
    <> ": "
    <> int.to_string(details.snapshot_size_bytes),
  )
  output_line(
    "archive_segment_count_"
    <> suffix
    <> ": "
    <> int.to_string(details.archive_segment_count),
  )
}

fn state_compact_to_json(result: StateCompactResult) -> json.Json {
  [
    #("command", json.string("state compact")),
    #("status", json.string(result.status)),
    #("workspace_root", json.string(result.workspace_root)),
    #("ledger_dir", json.string(result.ledger_dir)),
    #("current_path", json.string(result.current_path)),
    #("snapshot_path", json.string(result.snapshot_path)),
    #("archive_dir", json.string(result.archive_dir)),
    #("before", optional_compact_details_json(result.before)),
    #("after", optional_compact_details_json(result.after)),
    #("would_archive_current", optional_bool_json(result.would_archive_current)),
    #("reason", optional_string_json(result.reason)),
    #("message", json.string(result.message)),
  ]
  |> json.object
}

fn optional_compact_details_json(
  details: Option(StateCompactDetails),
) -> json.Json {
  case details {
    Some(details) -> compact_details_to_json(details)
    None -> json.null()
  }
}

fn compact_details_to_json(details: StateCompactDetails) -> json.Json {
  json.object([
    #("current_exists", json.bool(details.current_exists)),
    #("current_size_bytes", json.int(details.current_size_bytes)),
    #("current_record_count", json.int(details.current_record_count)),
    #("current_truncated_tail", json.bool(details.current_truncated_tail)),
    #("snapshot_exists", json.bool(details.snapshot_exists)),
    #("snapshot_size_bytes", json.int(details.snapshot_size_bytes)),
    #("archive_segment_count", json.int(details.archive_segment_count)),
  ])
}

fn optional_bool_json(value: Option(Bool)) -> json.Json {
  case value {
    Some(value) -> json.bool(value)
    None -> json.null()
  }
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn append_idempotent_error_message(
  error: ledger.AppendIdempotentError,
) -> String {
  case error {
    ledger.AppendLedgerError(error) -> ledger_error_message(error)
    ledger.RecordIdConflict(record_id) ->
      "ledger record id conflict: " <> record_id
  }
}

fn print_state_status(
  status: local_artifacts.StateStatusResult,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("state: " <> state_status_name(status.status))
  output_line("message: " <> status.message)
  output_line("workspace_root: " <> status.workspace_root)
  output_line("ledger_dir: " <> status.ledger_dir)
  case status.warnings {
    [] -> Nil
    _ -> {
      output_line("warnings:")
      list.each(status.warnings, fn(warning) { output_line("  " <> warning) })
    }
  }
  case status.status {
    local_artifacts.StateUnsupported(_, _) -> {
      output_line("recovery: old_state_reset_required")
      output_line("safe actions: archive-old, discard-old, reinitialize")
    }
    _ -> output_line("recovery: -")
  }
}

fn state_status_name(status: local_artifacts.StateStatus) -> String {
  case status {
    local_artifacts.StateCurrent -> "current"
    local_artifacts.StateUnsupported(_, _) -> "unsupported"
    local_artifacts.StateCorrupt(_) -> "corrupt"
    local_artifacts.StateMissing -> "missing"
    local_artifacts.StateArchived -> "archived"
  }
}

fn print_state_mutation(
  result: local_artifacts.StateMutationResult,
  json_output: Bool,
  output_line: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True ->
      output_line(
        result |> local_artifacts.state_mutation_to_json |> json.to_string,
      )
    False -> {
      output_line(result.action <> " " <> result.status)
      output_line("message: " <> result.message)
      case result.archive_path {
        Some(path) -> output_line("archive_path: " <> path)
        None -> Nil
      }
    }
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}
