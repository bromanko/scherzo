import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/log
import scherzo/path
import scherzo/session/event
import scherzo/session/recovery as session_recovery
import scherzo/state/ledger
import scherzo/state/record
import simplifile

pub const workflow_artifact_retention_ms = 2_592_000_000

pub const pi_transcript_retention_ms = 1_209_600_000

pub const cleanup_tombstone_retention_ms = 2_592_000_000

pub type ArtifactType {
  WorkflowArtifact
  PiTranscript
  CleanupTombstone
  LedgerArchive
}

pub type SchemaStatus {
  SchemaCurrent
  SchemaUnsupported(version: Int)
  SchemaCorrupt(reason: String)
  SchemaMissing
  SchemaUnknown
}

pub type LocalArtifactMetadata {
  LocalArtifactMetadata(
    artifact_type: ArtifactType,
    id: String,
    path: String,
    owner_id: Option(String),
    terminal_at_ms: Option(Int),
    recovery_status: Option(event.RecoveryStatus),
    schema_status: SchemaStatus,
    malformed: Bool,
  )
}

pub type LocalArtifactDecision {
  LocalArtifactDecision(
    artifact_type: String,
    id: String,
    recovery_status: Option(event.RecoveryStatus),
    cleanup_phase: event.CleanupPhase,
    reason: String,
    retention_until_ms: Option(Int),
    display_path: String,
  )
}

pub type CleanupResult {
  CleanupResult(
    dry_run: Bool,
    now_ms: Int,
    roots: List(String),
    transcript_root_status: String,
    would_delete: List(LocalArtifactDecision),
    deleted: List(LocalArtifactDecision),
    retained: List(LocalArtifactDecision),
    warnings: List(String),
  )
}

pub type PathSafety {
  PathSafe(path: String)
  PathUnsafe(reason: String)
}

pub type StateStatus {
  StateCurrent
  StateUnsupported(version: Int, reason: String)
  StateCorrupt(reason: String)
  StateMissing
  StateArchived
}

pub type StateStatusResult {
  StateStatusResult(
    status: StateStatus,
    workspace_root: String,
    ledger_dir: String,
    current_path: String,
    snapshot_path: String,
    archive_dir: String,
    message: String,
  )
}

pub type StateMutationResult {
  StateMutationResult(
    action: String,
    status: String,
    workspace_root: String,
    message: String,
    archive_path: Option(String),
  )
}

pub fn artifact_type_to_string(artifact_type: ArtifactType) -> String {
  case artifact_type {
    WorkflowArtifact -> "workflow_artifact"
    PiTranscript -> "pi_transcript"
    CleanupTombstone -> "cleanup_tombstone"
    LedgerArchive -> "ledger_archive"
  }
}

pub fn classify(
  metadata: LocalArtifactMetadata,
  now_ms: Int,
) -> LocalArtifactDecision {
  let base = fn(phase, reason, retention_until) {
    LocalArtifactDecision(
      artifact_type: artifact_type_to_string(metadata.artifact_type),
      id: metadata.id,
      recovery_status: metadata.recovery_status,
      cleanup_phase: phase,
      reason: session_recovery.recovery_safe_text(reason, []),
      retention_until_ms: retention_until,
      display_path: metadata.path,
    )
  }
  case metadata.malformed {
    True ->
      base(event.Retained, "malformed artifact metadata is retained", None)
    False ->
      case metadata.owner_id {
        None -> base(event.Retained, "missing owner marker is retained", None)
        Some(_) ->
          case metadata.schema_status {
            SchemaUnsupported(version) ->
              base(
                event.Retained,
                "unsupported schema version "
                  <> int.to_string(version)
                  <> " is retained",
                None,
              )
            SchemaCorrupt(reason) ->
              base(
                event.Retained,
                "corrupt schema is retained: " <> reason,
                None,
              )
            SchemaUnknown ->
              base(event.Retained, "unknown schema is retained", None)
            SchemaMissing ->
              base(event.Retained, "missing schema marker is retained", None)
            SchemaCurrent -> classify_current_schema(metadata, now_ms, base)
          }
      }
  }
}

fn classify_current_schema(
  metadata: LocalArtifactMetadata,
  now_ms: Int,
  base: fn(event.CleanupPhase, String, Option(Int)) -> LocalArtifactDecision,
) -> LocalArtifactDecision {
  case metadata.recovery_status {
    Some(event.Interrupted) ->
      base(
        event.Retained,
        "interrupted recovery state requires operator inspection",
        None,
      )
    Some(event.Parked) ->
      base(
        event.Retained,
        "parked recovery state is retained until unparked",
        None,
      )
    Some(event.OldStateResetRequired) ->
      base(
        event.Retained,
        "old-state reset-required artifacts are retained",
        None,
      )
    Some(event.Blocked)
    | Some(event.InspectionNeeded)
    | Some(event.DriftDetected) ->
      base(event.Retained, "reserved recovery state is retained", None)
    Some(event.Resumed) | Some(event.Recovered) | Some(event.Cleanup) | None ->
      classify_by_terminal_time(metadata, now_ms, base)
  }
}

fn classify_by_terminal_time(
  metadata: LocalArtifactMetadata,
  now_ms: Int,
  base: fn(event.CleanupPhase, String, Option(Int)) -> LocalArtifactDecision,
) -> LocalArtifactDecision {
  case metadata.terminal_at_ms {
    None -> base(event.Retained, "missing terminal time is retained", None)
    Some(terminal_at_ms) -> {
      let retention_until =
        terminal_at_ms + retention_ms(metadata.artifact_type)
      case now_ms >= retention_until {
        True ->
          base(
            event.Eligible,
            "retention expired; eligible for cleanup",
            Some(retention_until),
          )
        False ->
          base(
            event.Retained,
            "retention window has not expired",
            Some(retention_until),
          )
      }
    }
  }
}

fn retention_ms(artifact_type: ArtifactType) -> Int {
  case artifact_type {
    WorkflowArtifact | LedgerArchive -> workflow_artifact_retention_ms
    PiTranscript -> pi_transcript_retention_ms
    CleanupTombstone -> cleanup_tombstone_retention_ms
  }
}

pub fn check_path_safety(
  workspace_root: String,
  candidate_path: String,
) -> PathSafety {
  let workspace_root = string.trim(workspace_root)
  let candidate_path = string.trim(candidate_path)
  case workspace_root == "" || workspace_root == "/" {
    True -> PathUnsafe("workspace root must not be empty or filesystem root")
    False ->
      case candidate_path == "" || has_parent_traversal(candidate_path) {
        True ->
          PathUnsafe(
            "candidate path must not be empty or contain parent traversal",
          )
        False ->
          case
            path.absolute(workspace_root),
            path.absolute(path.join(workspace_root, ".scherzo-state")),
            path.absolute(candidate_path)
          {
            Ok(abs_root), Ok(abs_state_root), Ok(abs_candidate) ->
              case abs_root == "/" {
                True -> PathUnsafe("workspace root resolves to filesystem root")
                False ->
                  case path.contains(abs_state_root, abs_candidate) {
                    False -> PathUnsafe("candidate path escapes .scherzo-state")
                    True -> check_symlink_safety(abs_state_root, abs_candidate)
                  }
              }
            _, _, _ -> PathUnsafe("path normalization failed")
          }
      }
  }
}

fn check_symlink_safety(state_root: String, candidate: String) -> PathSafety {
  case path_has_symlink(state_root, candidate) {
    True -> PathUnsafe("candidate path includes a symlink")
    False -> PathSafe(candidate)
  }
}

fn path_has_symlink(state_root: String, candidate: String) -> Bool {
  let state_root = trim_trailing_slash(state_root)
  let candidate = trim_trailing_slash(candidate)
  let relative = case candidate == state_root {
    True -> ""
    False -> string.drop_start(candidate, string.length(state_root) + 1)
  }
  relative
  |> string.split(on: "/")
  |> list.filter(fn(part) { part != "" })
  |> path_has_symlink_loop(state_root)
}

fn path_has_symlink_loop(parts: List(String), current: String) -> Bool {
  case parts {
    [] -> False
    [part, ..rest] -> {
      let next = path.join(current, part)
      case simplifile.is_symlink(next) {
        Ok(True) -> True
        _ -> path_has_symlink_loop(rest, next)
      }
    }
  }
}

fn has_parent_traversal(candidate_path: String) -> Bool {
  candidate_path == ".."
  || string.starts_with(candidate_path, "../")
  || string.ends_with(candidate_path, "/..")
  || string.contains(candidate_path, "/../")
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

pub fn inventory(
  workspace_root: String,
  now_ms: Int,
  dry_run: Bool,
) -> CleanupResult {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(_) ->
      CleanupResult(
        dry_run: dry_run,
        now_ms: now_ms,
        roots: [],
        transcript_root_status: "unavailable",
        would_delete: [],
        deleted: [],
        retained: [],
        warnings: ["workspace root is invalid"],
      )
    Ok(paths) -> {
      let roots = [
        paths.archive_dir,
        path.join(workspace_root, ".scherzo-state/cleanup/tombstones"),
      ]
      let #(metadata, warnings) =
        discover_candidates(workspace_root, paths, now_ms)
      let decisions = list.map(metadata, classify(_, now_ms))
      let would_delete =
        list.filter(decisions, fn(decision) {
          decision.cleanup_phase == event.Eligible
        })
      let retained =
        list.filter(decisions, fn(decision) {
          decision.cleanup_phase != event.Eligible
        })
      CleanupResult(
        dry_run: dry_run,
        now_ms: now_ms,
        roots: roots,
        transcript_root_status: "unavailable",
        would_delete: would_delete,
        deleted: [],
        retained: retained,
        warnings: [
          "pi transcript root is not available in this tree",
          ..warnings
        ],
      )
    }
  }
}

pub fn apply_cleanup(workspace_root: String, now_ms: Int) -> CleanupResult {
  let dry = inventory(workspace_root, now_ms, True)
  case root_safety_errors(workspace_root, dry.would_delete) {
    [first, ..rest] ->
      CleanupResult(
        ..dry,
        dry_run: False,
        would_delete: [],
        warnings: list.append(dry.warnings, [first, ..rest]),
      )
    [] -> {
      let applied =
        delete_decisions(workspace_root, dry.would_delete, now_ms, [], [])
      let #(deleted, warnings) = applied
      CleanupResult(
        ..dry,
        dry_run: False,
        would_delete: [],
        deleted: list.reverse(deleted),
        warnings: list.append(dry.warnings, list.reverse(warnings)),
      )
    }
  }
}

fn root_safety_errors(
  workspace_root: String,
  decisions: List(LocalArtifactDecision),
) -> List(String) {
  decisions
  |> list.filter_map(fn(decision) {
    case check_path_safety(workspace_root, decision.display_path) {
      PathSafe(_) -> Error(Nil)
      PathUnsafe(reason) ->
        Ok("cleanup aborted for " <> decision.id <> ": " <> reason)
    }
  })
}

fn delete_decisions(
  workspace_root: String,
  decisions: List(LocalArtifactDecision),
  now_ms: Int,
  deleted: List(LocalArtifactDecision),
  warnings: List(String),
) -> #(List(LocalArtifactDecision), List(String)) {
  case decisions {
    [] -> #(deleted, warnings)
    [decision, ..rest] ->
      case check_path_safety(workspace_root, decision.display_path) {
        PathUnsafe(reason) ->
          delete_decisions(workspace_root, rest, now_ms, deleted, [
            "retained " <> decision.id <> ": " <> reason,
            ..warnings
          ])
        PathSafe(path) ->
          case write_tombstone(workspace_root, decision, now_ms) {
            Error(reason) ->
              delete_decisions(workspace_root, rest, now_ms, deleted, [
                "tombstone failed for " <> decision.id <> ": " <> reason,
                ..warnings
              ])
            Ok(Nil) ->
              case simplifile.delete(path) {
                Ok(Nil) ->
                  delete_decisions(
                    workspace_root,
                    rest,
                    now_ms,
                    [
                      LocalArtifactDecision(
                        ..decision,
                        cleanup_phase: event.Deleted,
                      ),
                      ..deleted
                    ],
                    warnings,
                  )
                Error(simplifile.Enoent) ->
                  delete_decisions(workspace_root, rest, now_ms, deleted, [
                    "already deleted: " <> decision.display_path,
                    ..warnings
                  ])
                Error(error) ->
                  delete_decisions(workspace_root, rest, now_ms, deleted, [
                    "delete failed for "
                      <> decision.id
                      <> ": "
                      <> simplifile.describe_error(error),
                    ..warnings
                  ])
              }
          }
      }
  }
}

fn write_tombstone(
  workspace_root: String,
  decision: LocalArtifactDecision,
  now_ms: Int,
) -> Result(Nil, String) {
  let tombstone_dir =
    path.join(workspace_root, ".scherzo-state/cleanup/tombstones")
  let tombstone_path =
    path.join(
      tombstone_dir,
      int.to_string(now_ms) <> "-" <> safe_filename(decision.id) <> ".json",
    )
  case check_path_safety(workspace_root, tombstone_path) {
    PathUnsafe(reason) -> Error(reason)
    PathSafe(_) ->
      case simplifile.create_directory_all(tombstone_dir) {
        Error(error) -> Error(simplifile.describe_error(error))
        Ok(Nil) ->
          case
            simplifile.write(
              tombstone_path,
              json.to_string(decision_to_json(decision)) <> "\n",
            )
          {
            Ok(Nil) -> Ok(Nil)
            Error(error) -> Error(simplifile.describe_error(error))
          }
      }
  }
}

fn safe_filename(value: String) -> String {
  value
  |> string.replace(each: "/", with: "_")
  |> string.replace(each: "..", with: "_")
  |> string.replace(each: " ", with: "_")
}

fn discover_candidates(
  workspace_root: String,
  paths: ledger.LedgerPath,
  now_ms: Int,
) -> #(List(LocalArtifactMetadata), List(String)) {
  let archive_candidates =
    discover_archive_candidates(workspace_root, paths, now_ms)
  let tombstone_candidates =
    discover_tombstone_candidates(workspace_root, now_ms)
  let #(archive_metadata, archive_warnings) = archive_candidates
  let #(tombstone_metadata, tombstone_warnings) = tombstone_candidates
  #(
    list.append(archive_metadata, tombstone_metadata),
    list.append(archive_warnings, tombstone_warnings),
  )
}

fn discover_archive_candidates(
  workspace_root: String,
  paths: ledger.LedgerPath,
  _now_ms: Int,
) -> #(List(LocalArtifactMetadata), List(String)) {
  case simplifile.read_directory(paths.archive_dir) {
    Error(simplifile.Enoent) -> #([], [])
    Error(error) -> #([], [
      "read ledger archive failed: " <> simplifile.describe_error(error),
    ])
    Ok(entries) -> {
      let metadata =
        entries
        |> list.map(fn(entry) {
          let full_path = path.join(paths.archive_dir, entry)
          archive_metadata_for_path(workspace_root, entry, full_path)
        })
      #(metadata, [])
    }
  }
}

fn archive_metadata_for_path(
  workspace_root: String,
  id: String,
  full_path: String,
) -> LocalArtifactMetadata {
  case check_path_safety(workspace_root, full_path) {
    PathUnsafe(_) ->
      LocalArtifactMetadata(
        artifact_type: LedgerArchive,
        id: id,
        path: full_path,
        owner_id: Some(workspace_root),
        terminal_at_ms: None,
        recovery_status: Some(event.Cleanup),
        schema_status: SchemaCurrent,
        malformed: True,
      )
    PathSafe(_) -> {
      let terminal_at_ms = file_mtime_ms(full_path)
      LocalArtifactMetadata(
        artifact_type: LedgerArchive,
        id: id,
        path: full_path,
        owner_id: Some(workspace_root),
        terminal_at_ms: terminal_at_ms,
        recovery_status: Some(event.Cleanup),
        schema_status: SchemaCurrent,
        malformed: False,
      )
    }
  }
}

fn discover_tombstone_candidates(
  workspace_root: String,
  _now_ms: Int,
) -> #(List(LocalArtifactMetadata), List(String)) {
  let tombstone_dir =
    path.join(workspace_root, ".scherzo-state/cleanup/tombstones")
  case simplifile.read_directory(tombstone_dir) {
    Error(simplifile.Enoent) -> #([], [])
    Error(error) -> #([], [
      "read cleanup tombstones failed: " <> simplifile.describe_error(error),
    ])
    Ok(entries) -> #(
      entries
        |> list.map(fn(entry) {
          let full_path = path.join(tombstone_dir, entry)
          LocalArtifactMetadata(
            artifact_type: CleanupTombstone,
            id: entry,
            path: full_path,
            owner_id: Some(workspace_root),
            terminal_at_ms: file_mtime_ms(full_path),
            recovery_status: Some(event.Cleanup),
            schema_status: SchemaCurrent,
            malformed: False,
          )
        }),
      [],
    )
  }
}

fn file_mtime_ms(file_path: String) -> Option(Int) {
  case simplifile.file_info(file_path) {
    Ok(info) -> Some(info.mtime_seconds * 1000)
    Error(_) -> None
  }
}

pub fn cleanup_result_to_json(result: CleanupResult) -> json.Json {
  json.object([
    #("dry_run", json.bool(result.dry_run)),
    #("now_ms", json.int(result.now_ms)),
    #("roots", json.array(result.roots, of: json.string)),
    #("transcript_root_status", json.string(result.transcript_root_status)),
    #("would_delete", json.array(result.would_delete, of: decision_to_json)),
    #("deleted", json.array(result.deleted, of: decision_to_json)),
    #("retained", json.array(result.retained, of: decision_to_json)),
    #("warnings", json.array(result.warnings, of: json.string)),
  ])
}

pub fn decision_to_json(decision: LocalArtifactDecision) -> json.Json {
  json.object([
    #("artifact_type", json.string(decision.artifact_type)),
    #("id", json.string(decision.id)),
    #("recovery_status", optional_recovery_status(decision.recovery_status)),
    #(
      "cleanup_phase",
      json.string(event.cleanup_phase_to_string(decision.cleanup_phase)),
    ),
    #("reason", json.string(decision.reason)),
    #("retention_until_ms", optional_int(decision.retention_until_ms)),
    #("path", json.string(decision.display_path)),
  ])
}

fn optional_recovery_status(value: Option(event.RecoveryStatus)) -> json.Json {
  case value {
    Some(status) -> json.string(event.recovery_status_to_string(status))
    None -> json.null()
  }
}

fn optional_int(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

pub fn inspect_state(workspace_root: String) -> StateStatusResult {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(_) ->
      StateStatusResult(
        status: StateCorrupt("workspace root is invalid"),
        workspace_root: workspace_root,
        ledger_dir: "",
        current_path: "",
        snapshot_path: "",
        archive_dir: "",
        message: "workspace root is invalid",
      )
    Ok(paths) -> inspect_ledger_paths(paths)
  }
}

fn inspect_ledger_paths(paths: ledger.LedgerPath) -> StateStatusResult {
  let status = state_status_for_paths(paths)
  StateStatusResult(
    status: status,
    workspace_root: paths.workspace_root,
    ledger_dir: paths.ledger_dir,
    current_path: paths.current_path,
    snapshot_path: paths.snapshot_path,
    archive_dir: paths.archive_dir,
    message: state_status_message(status),
  )
}

fn state_status_for_paths(paths: ledger.LedgerPath) -> StateStatus {
  case simplifile.is_directory(paths.ledger_dir) {
    Ok(False) | Error(simplifile.Enoent) -> StateMissing
    Error(error) -> StateCorrupt(simplifile.describe_error(error))
    Ok(True) -> {
      case unsupported_or_corrupt_current(paths.current_path) {
        Some(status) -> status
        None ->
          case unsupported_or_corrupt_snapshot(paths.snapshot_path) {
            Some(status) -> status
            None -> StateCurrent
          }
      }
    }
  }
}

fn unsupported_or_corrupt_current(current_path: String) -> Option(StateStatus) {
  case simplifile.read(current_path) {
    Error(simplifile.Enoent) -> None
    Error(error) -> Some(StateCorrupt(simplifile.describe_error(error)))
    Ok(contents) -> {
      let lines =
        contents
        |> string.split(on: "\n")
        |> list.filter(fn(line) { string.trim(line) != "" })
      case lines {
        [] -> None
        [line, ..] ->
          case schema_version_from_json(line) {
            Ok(version) ->
              case version == record.schema_version {
                True -> None
                False ->
                  Some(StateUnsupported(
                    version,
                    "unsupported ledger schema version "
                      <> int.to_string(version),
                  ))
              }
            Error(reason) -> Some(StateCorrupt(reason))
          }
      }
    }
  }
}

fn unsupported_or_corrupt_snapshot(
  snapshot_path: String,
) -> Option(StateStatus) {
  case simplifile.read(snapshot_path) {
    Error(simplifile.Enoent) -> None
    Error(error) -> Some(StateCorrupt(simplifile.describe_error(error)))
    Ok(contents) ->
      case string.trim(contents) == "" {
        True -> None
        False ->
          case schema_version_from_json(contents) {
            Ok(version) ->
              case version == record.schema_version {
                True -> None
                False ->
                  Some(StateUnsupported(
                    version,
                    "unsupported snapshot schema version "
                      <> int.to_string(version),
                  ))
              }
            Error(reason) -> Some(StateCorrupt(reason))
          }
      }
  }
}

fn schema_version_from_json(contents: String) -> Result(Int, String) {
  case json.parse(contents, schema_version_decoder()) {
    Ok(version) -> Ok(version)
    Error(_) -> Error("malformed schema marker")
  }
}

fn schema_version_decoder() -> decode.Decoder(Int) {
  use version <- decode.field("schema_version", decode.int)
  decode.success(version)
}

fn state_status_message(status: StateStatus) -> String {
  case status {
    StateCurrent -> "local state is current"
    StateUnsupported(_, reason) -> reason
    StateCorrupt(reason) -> "local state is corrupt or malformed: " <> reason
    StateMissing -> "local state is missing"
    StateArchived -> "local state has been archived"
  }
}

pub fn archive_old_state(
  workspace_root: String,
  yes: Bool,
  now_ms: Int,
) -> StateMutationResult {
  mutate_old_state("archive-old", workspace_root, yes, now_ms)
}

pub fn discard_old_state(
  workspace_root: String,
  yes: Bool,
  now_ms: Int,
) -> StateMutationResult {
  mutate_old_state("discard-old", workspace_root, yes, now_ms)
}

fn mutate_old_state(
  action: String,
  workspace_root: String,
  yes: Bool,
  now_ms: Int,
) -> StateMutationResult {
  case yes {
    False ->
      StateMutationResult(
        action,
        "rejected",
        workspace_root,
        action <> " requires --yes",
        None,
      )
    True -> {
      let status = inspect_state(workspace_root)
      case status.status {
        StateUnsupported(_, _) ->
          case action {
            "archive-old" -> archive_ledger(status, now_ms)
            "discard-old" -> discard_ledger(status)
            _ ->
              StateMutationResult(
                action,
                "rejected",
                workspace_root,
                "unknown action",
                None,
              )
          }
        StateCorrupt(_) ->
          StateMutationResult(
            action,
            "rejected",
            workspace_root,
            "state is corrupt or malformed; inspect manually",
            None,
          )
        StateCurrent ->
          StateMutationResult(
            action,
            "rejected",
            workspace_root,
            "state is current; no old state to mutate",
            None,
          )
        StateMissing ->
          StateMutationResult(
            action,
            "rejected",
            workspace_root,
            "state is missing",
            None,
          )
        StateArchived ->
          StateMutationResult(
            action,
            "rejected",
            workspace_root,
            "state is already archived",
            None,
          )
      }
    }
  }
}

fn archive_ledger(
  status: StateStatusResult,
  now_ms: Int,
) -> StateMutationResult {
  let destination =
    path.join(
      status.workspace_root,
      ".scherzo-state/archive/old-state/" <> int.to_string(now_ms) <> "/ledger",
    )
  case
    check_path_safety(status.workspace_root, status.ledger_dir),
    check_path_safety(status.workspace_root, destination)
  {
    PathUnsafe(reason), _ | _, PathUnsafe(reason) ->
      StateMutationResult(
        "archive-old",
        "rejected",
        status.workspace_root,
        "unsafe state path: " <> reason,
        None,
      )
    PathSafe(_), PathSafe(_) ->
      case
        simplifile.create_directory_all(path.join(
          status.workspace_root,
          ".scherzo-state/archive/old-state/" <> int.to_string(now_ms),
        ))
      {
        Error(error) ->
          StateMutationResult(
            "archive-old",
            "failed",
            status.workspace_root,
            simplifile.describe_error(error),
            None,
          )
        Ok(Nil) ->
          case simplifile.rename(status.ledger_dir, destination) {
            Ok(Nil) ->
              StateMutationResult(
                "archive-old",
                "applied",
                status.workspace_root,
                "unsupported old state archived",
                Some(destination),
              )
            Error(error) ->
              StateMutationResult(
                "archive-old",
                "failed",
                status.workspace_root,
                simplifile.describe_error(error),
                None,
              )
          }
      }
  }
}

fn discard_ledger(status: StateStatusResult) -> StateMutationResult {
  case check_path_safety(status.workspace_root, status.ledger_dir) {
    PathUnsafe(reason) ->
      StateMutationResult(
        "discard-old",
        "rejected",
        status.workspace_root,
        "unsafe state path: " <> reason,
        None,
      )
    PathSafe(_) ->
      case simplifile.delete(status.ledger_dir) {
        Ok(Nil) ->
          StateMutationResult(
            "discard-old",
            "applied",
            status.workspace_root,
            "unsupported old state discarded irreversibly",
            None,
          )
        Error(error) ->
          StateMutationResult(
            "discard-old",
            "failed",
            status.workspace_root,
            simplifile.describe_error(error),
            None,
          )
      }
  }
}

pub fn reinitialize_state(
  workspace_root: String,
  yes yes: Bool,
) -> StateMutationResult {
  case yes {
    False ->
      StateMutationResult(
        "reinitialize",
        "rejected",
        workspace_root,
        "reinitialize requires --yes",
        None,
      )
    True ->
      case ledger.path_for_workspace_root(workspace_root) {
        Error(_) ->
          StateMutationResult(
            "reinitialize",
            "failed",
            workspace_root,
            "workspace root is invalid",
            None,
          )
        Ok(paths) ->
          case check_path_safety(paths.workspace_root, paths.current_path) {
            PathUnsafe(reason) ->
              StateMutationResult(
                "reinitialize",
                "rejected",
                workspace_root,
                "unsafe state path: " <> reason,
                None,
              )
            PathSafe(_) ->
              case simplifile.is_file(paths.current_path) {
                Ok(True) ->
                  StateMutationResult(
                    "reinitialize",
                    "rejected",
                    workspace_root,
                    "current ledger already exists; archive or discard old state first",
                    None,
                  )
                _ ->
                  case simplifile.create_directory_all(paths.archive_dir) {
                    Error(error) ->
                      StateMutationResult(
                        "reinitialize",
                        "failed",
                        workspace_root,
                        simplifile.describe_error(error),
                        None,
                      )
                    Ok(Nil) ->
                      case simplifile.write(paths.current_path, "") {
                        Ok(Nil) ->
                          StateMutationResult(
                            "reinitialize",
                            "applied",
                            workspace_root,
                            "empty current ledger initialized",
                            None,
                          )
                        Error(error) ->
                          StateMutationResult(
                            "reinitialize",
                            "failed",
                            workspace_root,
                            simplifile.describe_error(error),
                            None,
                          )
                      }
                  }
              }
          }
      }
  }
}

pub fn state_status_to_json(status: StateStatusResult) -> json.Json {
  let #(name, version) = case status.status {
    StateCurrent -> #("current", None)
    StateUnsupported(version, _) -> #("unsupported", Some(version))
    StateCorrupt(_) -> #("corrupt", None)
    StateMissing -> #("missing", None)
    StateArchived -> #("archived", None)
  }
  json.object([
    #("status", json.string(name)),
    #("schema_version", optional_int(version)),
    #("workspace_root", json.string(status.workspace_root)),
    #("ledger_dir", json.string(status.ledger_dir)),
    #("current_path", json.string(status.current_path)),
    #("snapshot_path", json.string(status.snapshot_path)),
    #("archive_dir", json.string(status.archive_dir)),
    #("message", json.string(status.message)),
    #(
      "recovery",
      session_recovery.old_state_reset_required(status.message)
        |> recovery_to_json_when_needed(status.status),
    ),
  ])
}

fn recovery_to_json_when_needed(
  recovery: event.RecoveryInfo,
  status: StateStatus,
) -> json.Json {
  case status {
    StateUnsupported(_, _) ->
      json.object([
        #(
          "status",
          json.string(event.recovery_status_to_string(recovery.status)),
        ),
        #("source", json.string(recovery.source)),
        #("message", optional_string(recovery.message)),
        #(
          "safe_actions",
          json.array(recovery.safe_actions, of: fn(action) {
            json.string(event.recovery_action_to_string(action))
          }),
        ),
      ])
    _ -> json.null()
  }
}

fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

pub fn state_mutation_to_json(result: StateMutationResult) -> json.Json {
  json.object([
    #("action", json.string(result.action)),
    #("status", json.string(result.status)),
    #("workspace_root", json.string(result.workspace_root)),
    #("message", json.string(result.message)),
    #("archive_path", optional_string(result.archive_path)),
  ])
}

pub fn now_ms() -> Int {
  system_time_millisecond()
}

pub fn cleanup_summary(result: CleanupResult) -> String {
  "cleanup "
  <> case result.dry_run {
    True -> "dry-run"
    False -> "apply"
  }
  <> ": would_delete="
  <> int.to_string(list.length(result.would_delete))
  <> " deleted="
  <> int.to_string(list.length(result.deleted))
  <> " retained="
  <> int.to_string(list.length(result.retained))
  <> " warnings="
  <> int.to_string(list.length(result.warnings))
}

pub fn safe_log_field(value: String, secrets: List(String)) -> String {
  log.redact("recovery", value, secrets) |> log.truncate(200)
}

@external(erlang, "scherzo_state_ffi", "system_time_millisecond")
fn system_time_millisecond() -> Int
