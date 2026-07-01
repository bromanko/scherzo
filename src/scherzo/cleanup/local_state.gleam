import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt}
import gleam/string
import scherzo/path
import scherzo/session/event
import scherzo/state/ledger
import scherzo/state/local_artifacts
import simplifile

pub type LocalStateItem {
  LocalStateItem(
    decision: local_artifacts.LocalArtifactDecision,
    status: String,
    warnings: List(String),
  )
}

pub type CleanupPage {
  CleanupPage(
    roots: List(String),
    transcript_root_status: String,
    items: List(LocalStateItem),
    warnings: List(String),
    scanned: Int,
    applied: Int,
    budget_exhausted: Bool,
    truncated: Bool,
    next_key: Option(String),
    truncated_reason: Option(String),
  )
}

type CandidateRef {
  ArchiveCandidate(id: String, full_path: String)
  TombstoneCandidate(id: String, full_path: String)
}

type CandidateStep {
  NoCandidate
  NextCandidate(
    candidate: CandidateRef,
    archive_entries: List(String),
    tombstone_entries: List(String),
  )
}

type PageState {
  PageState(
    items: List(LocalStateItem),
    scanned: Int,
    applied: Int,
    budget_exhausted: Bool,
    truncated: Bool,
    next_key: Option(String),
    truncated_reason: Option(String),
    last_key: Option(String),
  )
}

pub fn cleanup_page(
  workspace_root: String,
  now_ms: Int,
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
) -> CleanupPage {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(error) ->
      CleanupPage(
        roots: [],
        transcript_root_status: "unavailable",
        items: [],
        warnings: [
          "workspace root is invalid: " <> ledger.ledger_error_to_string(error),
        ],
        scanned: 0,
        applied: 0,
        budget_exhausted: False,
        truncated: False,
        next_key: None,
        truncated_reason: None,
      )
    Ok(paths) -> {
      let tombstone_dir =
        path.join(workspace_root, ".scherzo-state/cleanup/tombstones")
      let roots = [paths.archive_dir, tombstone_dir]
      let #(archive_entries, archive_warnings) =
        sorted_directory_entries(
          paths.archive_dir,
          "read ledger archive failed: ",
        )
      let #(tombstone_entries, tombstone_warnings) =
        sorted_directory_entries(
          tombstone_dir,
          "read cleanup tombstones failed: ",
        )
      let state =
        process_candidate_sources(
          workspace_root,
          now_ms,
          paths.archive_dir,
          tombstone_dir,
          archive_entries,
          tombstone_entries,
          after_key,
          limit,
          started_ms,
          max_runtime_ms,
          clock,
          apply,
          PageState([], 0, 0, False, False, None, None, None),
        )
      CleanupPage(
        roots: roots,
        transcript_root_status: "unavailable",
        items: list.reverse(state.items),
        warnings: [
          "pi transcript root is not available in this tree",
          ..list.append(archive_warnings, tombstone_warnings)
        ],
        scanned: state.scanned,
        applied: state.applied,
        budget_exhausted: state.budget_exhausted,
        truncated: state.truncated,
        next_key: state.next_key,
        truncated_reason: state.truncated_reason,
      )
    }
  }
}

fn sorted_directory_entries(
  directory: String,
  warning_prefix: String,
) -> #(List(String), List(String)) {
  case simplifile.read_directory(directory) {
    Error(simplifile.Enoent) -> #([], [])
    Error(error) -> #([], [warning_prefix <> simplifile.describe_error(error)])
    Ok(entries) -> #(entries |> list.sort(by: string.compare), [])
  }
}

fn process_candidate_sources(
  workspace_root: String,
  now_ms: Int,
  archive_dir: String,
  tombstone_dir: String,
  archive_entries: List(String),
  tombstone_entries: List(String),
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
  state: PageState,
) -> PageState {
  case state.truncated {
    True -> state
    False ->
      case
        next_candidate(
          archive_dir,
          tombstone_dir,
          archive_entries,
          tombstone_entries,
        )
      {
        NoCandidate -> state
        NextCandidate(candidate, next_archive_entries, next_tombstone_entries) -> {
          let next_state =
            process_candidate(
              workspace_root,
              now_ms,
              candidate,
              after_key,
              limit,
              started_ms,
              max_runtime_ms,
              clock,
              apply,
              state,
            )
          process_candidate_sources(
            workspace_root,
            now_ms,
            archive_dir,
            tombstone_dir,
            next_archive_entries,
            next_tombstone_entries,
            after_key,
            limit,
            started_ms,
            max_runtime_ms,
            clock,
            apply,
            next_state,
          )
        }
      }
  }
}

fn next_candidate(
  archive_dir: String,
  tombstone_dir: String,
  archive_entries: List(String),
  tombstone_entries: List(String),
) -> CandidateStep {
  case archive_entries, tombstone_entries {
    [], [] -> NoCandidate
    [archive_entry, ..rest_archive_entries], [] ->
      NextCandidate(
        archive_candidate(archive_dir, archive_entry),
        rest_archive_entries,
        tombstone_entries,
      )
    [], [tombstone_entry, ..rest_tombstone_entries] ->
      NextCandidate(
        tombstone_candidate(tombstone_dir, tombstone_entry),
        archive_entries,
        rest_tombstone_entries,
      )
    [archive_entry, ..rest_archive_entries],
      [tombstone_entry, ..rest_tombstone_entries]
    -> {
      let archive = archive_candidate(archive_dir, archive_entry)
      let tombstone = tombstone_candidate(tombstone_dir, tombstone_entry)
      case string.compare(candidate_key(archive), candidate_key(tombstone)) {
        Gt -> NextCandidate(tombstone, archive_entries, rest_tombstone_entries)
        _ -> NextCandidate(archive, rest_archive_entries, tombstone_entries)
      }
    }
  }
}

fn archive_candidate(archive_dir: String, entry: String) -> CandidateRef {
  ArchiveCandidate(entry, path.join(archive_dir, entry))
}

fn tombstone_candidate(tombstone_dir: String, entry: String) -> CandidateRef {
  TombstoneCandidate(entry, path.join(tombstone_dir, entry))
}

fn process_candidate(
  workspace_root: String,
  now_ms: Int,
  candidate: CandidateRef,
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
  state: PageState,
) -> PageState {
  case state.truncated || skip_candidate_for_cursor(candidate, after_key) {
    True -> state
    False ->
      case
        cleanup_page_should_truncate(
          limit,
          state.scanned,
          state.budget_exhausted,
          max_runtime_ms,
          started_ms,
          clock,
        )
      {
        Some(reason) ->
          PageState(
            ..state,
            truncated: True,
            next_key: state.last_key,
            truncated_reason: Some(reason),
          )
        None -> {
          let item = candidate_item(workspace_root, now_ms, candidate)
          let #(item, budget_exhausted) =
            apply_candidate_item_and_checkpoint_budget(
              workspace_root,
              now_ms,
              apply,
              max_runtime_ms,
              started_ms,
              clock,
              item,
            )
          PageState(
            items: [item, ..state.items],
            scanned: state.scanned + 1,
            applied: state.applied + applied_increment(apply),
            budget_exhausted: budget_exhausted,
            truncated: False,
            next_key: state.next_key,
            truncated_reason: state.truncated_reason,
            last_key: Some(candidate_key(candidate)),
          )
        }
      }
  }
}

fn apply_candidate_item_and_checkpoint_budget(
  workspace_root: String,
  now_ms: Int,
  apply: Bool,
  max_runtime_ms: Option(Int),
  started_ms: Int,
  clock: fn() -> Int,
  item: LocalStateItem,
) -> #(LocalStateItem, Bool) {
  let item = case apply {
    False -> item
    True -> apply_item(workspace_root, now_ms, item)
  }
  #(item, cleanup_runtime_budget_hit(max_runtime_ms, started_ms, clock()))
}

fn applied_increment(apply: Bool) -> Int {
  case apply {
    True -> 1
    False -> 0
  }
}

fn skip_candidate_for_cursor(
  candidate: CandidateRef,
  after_key: Option(String),
) -> Bool {
  case after_key {
    Some(last_key) -> string.compare(candidate_key(candidate), last_key) != Gt
    None -> False
  }
}

fn cleanup_page_should_truncate(
  limit: Option(Int),
  scanned: Int,
  budget_exhausted: Bool,
  max_runtime_ms: Option(Int),
  started_ms: Int,
  clock: fn() -> Int,
) -> Option(String) {
  case hit_limit(limit, scanned) {
    True -> Some("limit")
    False ->
      case
        budget_exhausted
        || cleanup_runtime_budget_hit(max_runtime_ms, started_ms, clock())
      {
        True -> Some("runtime_budget")
        False -> None
      }
  }
}

fn candidate_item(
  workspace_root: String,
  now_ms: Int,
  candidate: CandidateRef,
) -> LocalStateItem {
  let decision =
    candidate_metadata(workspace_root, candidate)
    |> local_artifacts.classify(now_ms)
  LocalStateItem(decision, status_for_decision(decision), [])
}

fn candidate_metadata(
  workspace_root: String,
  candidate: CandidateRef,
) -> local_artifacts.LocalArtifactMetadata {
  case candidate {
    ArchiveCandidate(id, full_path) ->
      archive_metadata_for_path(workspace_root, id, full_path)
    TombstoneCandidate(id, full_path) ->
      local_artifacts.LocalArtifactMetadata(
        artifact_type: local_artifacts.CleanupTombstone,
        id: id,
        path: full_path,
        owner_id: Some(workspace_root),
        terminal_at_ms: file_mtime_ms(full_path),
        recovery_status: Some(event.Cleanup),
        schema_status: local_artifacts.SchemaCurrent,
        malformed: False,
      )
  }
}

fn archive_metadata_for_path(
  workspace_root: String,
  id: String,
  full_path: String,
) -> local_artifacts.LocalArtifactMetadata {
  case local_artifacts.check_path_safety(workspace_root, full_path) {
    local_artifacts.PathUnsafe(_) ->
      local_artifacts.LocalArtifactMetadata(
        artifact_type: local_artifacts.LedgerArchive,
        id: id,
        path: full_path,
        owner_id: Some(workspace_root),
        terminal_at_ms: None,
        recovery_status: Some(event.Cleanup),
        schema_status: local_artifacts.SchemaCurrent,
        malformed: True,
      )
    local_artifacts.PathSafe(_) ->
      local_artifacts.LocalArtifactMetadata(
        artifact_type: local_artifacts.LedgerArchive,
        id: id,
        path: full_path,
        owner_id: Some(workspace_root),
        terminal_at_ms: file_mtime_ms(full_path),
        recovery_status: Some(event.Cleanup),
        schema_status: local_artifacts.SchemaCurrent,
        malformed: False,
      )
  }
}

fn apply_item(
  workspace_root: String,
  now_ms: Int,
  item: LocalStateItem,
) -> LocalStateItem {
  let LocalStateItem(decision, _, _) = item
  case decision.cleanup_phase == event.Eligible {
    True -> {
      let #(applied, warnings) =
        local_artifacts.apply_decision(workspace_root, decision, now_ms)
      case applied {
        Some(next) -> LocalStateItem(next, "deleted", warnings)
        None -> LocalStateItem(decision, "retained", warnings)
      }
    }
    False -> LocalStateItem(decision, "retained", [])
  }
}

fn status_for_decision(
  decision: local_artifacts.LocalArtifactDecision,
) -> String {
  case decision.cleanup_phase {
    event.Eligible -> "would_delete"
    event.Retained -> "retained"
    event.Deleting -> "would_delete"
    event.Deleted -> "deleted"
  }
}

fn file_mtime_ms(file_path: String) -> Option(Int) {
  case simplifile.file_info(file_path) {
    Ok(info) -> Some(info.mtime_seconds * 1000)
    Error(error) -> missing_file_mtime(file_path, error)
  }
}

fn missing_file_mtime(
  _file_path: String,
  _error: simplifile.FileError,
) -> Option(Int) {
  None
}

fn candidate_key(candidate: CandidateRef) -> String {
  case candidate {
    ArchiveCandidate(id, full_path) -> id <> ":" <> full_path
    TombstoneCandidate(id, full_path) -> id <> ":" <> full_path
  }
}

fn hit_limit(limit: Option(Int), scanned: Int) -> Bool {
  case limit {
    Some(value) -> value >= 0 && scanned >= value
    None -> False
  }
}

fn cleanup_runtime_budget_hit(
  max_runtime_ms: Option(Int),
  started_ms: Int,
  now_ms: Int,
) -> Bool {
  case max_runtime_ms {
    Some(value) -> value > 0 && now_ms - started_ms >= value
    None -> False
  }
}
