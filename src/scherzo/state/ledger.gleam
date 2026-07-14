import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/path
import scherzo/state/ledger/cache as ledger_cache
import scherzo/state/ledger/fingerprint
import scherzo/state/ledger/record_index
import scherzo/state/projection
import scherzo/state/record
import simplifile

pub type LedgerPath {
  LedgerPath(
    workspace_root: String,
    ledger_dir: String,
    current_path: String,
    snapshot_path: String,
    archive_dir: String,
  )
}

pub type LedgerFfiError {
  OpenFailed(reason: String)
  WriteFailed(reason: String)
  SyncFailed(reason: String)
  CloseFailed(reason: String)
  ReadFailed(reason: String)
  StepFailed(reason: String)
  LockFailed(reason: String)
  UnexpectedFfiFailure(function: String, detail: String)
}

pub type LedgerError {
  Io(String)
  LedgerFfiFailed(LedgerFfiError)
  UnsupportedVersion(Int)
  CorruptRecord(line: Int, reason: String)
  AggregateInvariantViolation(reason: String, run_id: String)
}

pub type ReadRecordsResult {
  ReadRecordsResult(records: List(record.LedgerRecord), truncated_tail: Bool)
}

pub type CurrentSegmentStats {
  CurrentSegmentStats(record_count: Int, byte_size: Int, truncated_tail: Bool)
}

pub type LedgerStorageStats {
  LedgerStorageStats(
    current: CurrentSegmentStats,
    snapshot_size_bytes: Int,
    archive_segment_count: Int,
  )
}

pub type CompactionReport {
  CompactionReport(
    before: LedgerStorageStats,
    after: LedgerStorageStats,
    duration_ms: Int,
  )
}

pub type CacheDiagnostics {
  CacheDiagnostics(
    hydration_count: Int,
    reload_count: Int,
    fingerprint_mismatch_count: Int,
    cache_hit_count: Int,
    duplicate_probe_count: Int,
    record_id_index_size: Int,
  )
}

type CachedLedgerState {
  CachedLedgerState(
    projection: projection.Projection,
    record_index: record_index.RecordIndex,
    fingerprint: fingerprint.LedgerFingerprint,
    diagnostics: CacheDiagnostics,
  )
}

pub type ReplayResult {
  ReplayResult(
    records: List(record.LedgerRecord),
    projection: projection.Projection,
    truncated_tail: Bool,
  )
}

pub type AppendIdempotentResult {
  Appended
  AlreadyRecorded(existing_record: record.LedgerRecord)
}

pub type AppendIdempotentError {
  AppendLedgerError(LedgerError)
  RecordIdConflict(record_id: String)
}

pub type AppendWorkstreamStartResult {
  WorkstreamStartRecordsAppended
  WorkstreamStartRecordsDuplicate(existing_run: projection.WorkstreamPhaseRun)
  WorkstreamStartRecordsConflict(existing_run: projection.WorkstreamPhaseRun)
}

pub type AppendWorkstreamStartError {
  AppendStartLedgerError(LedgerError)
  AppendStartRecordIdConflict(record_id: String)
  AppendStartInvalidQueueRecord
}

type StartQueueRecord {
  StartQueueRecord(
    workstream_id: String,
    action_id: String,
    idempotency_key: String,
  )
}

type ExistingStartDecision {
  NoExistingStart
  ExistingStartDuplicate(existing_run: projection.WorkstreamPhaseRun)
  ExistingStartConflict(existing_run: projection.WorkstreamPhaseRun)
}

type LockedStartAppendDecision {
  LockedStartAppended
  LockedStartDuplicate(existing_run: projection.WorkstreamPhaseRun)
  LockedStartConflict(existing_run: projection.WorkstreamPhaseRun)
  LockedStartRecordConflict(record_id: String)
}

type MissingRecordsDecision {
  MissingRecords(records: List(record.LedgerRecord))
  MissingRecordConflict(record_id: String)
}

type RecordPresenceDecision {
  RecordMissing
  RecordAlreadyRecorded(existing_record: record.LedgerRecord)
  RecordConflict(record_id: String)
}

type SnapshotRead {
  SnapshotRead(
    projection: projection.Projection,
    metadata: Option(record_index.RecordIndex),
  )
}

type LockedAppendDecision {
  LockedAppendAppended
  LockedAppendAlreadyRecorded(existing_record: record.LedgerRecord)
  LockedAppendConflict(record_id: String)
}

type JsonlFold(value) {
  JsonlFold(value: value, error: Option(LedgerError), truncated_tail: Bool)
}

type ParsedLine {
  ParsedRecord(record.LedgerRecord)
  EmptyTrailingLine
  TruncatedTail
}

pub fn path_for_workspace_root(
  workspace_root: String,
) -> Result(LedgerPath, LedgerError) {
  case string.trim(workspace_root) == "" {
    True -> Error(Io("workspace root must not be empty"))
    False -> {
      let state_dir = path.join(workspace_root, ".scherzo-state")
      let ledger_dir = path.join(state_dir, "ledger")
      Ok(LedgerPath(
        workspace_root: workspace_root,
        ledger_dir: ledger_dir,
        current_path: path.join(ledger_dir, "current.jsonl"),
        snapshot_path: path.join(ledger_dir, "snapshot.json"),
        archive_dir: path.join(ledger_dir, "archive"),
      ))
    }
  }
}

pub fn append(
  ledger_path: LedgerPath,
  ledger_record: record.LedgerRecord,
  fsync: Bool,
) -> Result(Nil, LedgerError) {
  append_many(ledger_path, [ledger_record], fsync)
}

pub fn append_many(
  ledger_path: LedgerPath,
  records: List(record.LedgerRecord),
  fsync: Bool,
) -> Result(Nil, LedgerError) {
  use Nil <- result.try(ensure_layout(ledger_path))
  case records {
    [] -> Ok(Nil)
    _ ->
      with_ledger_lock(ledger_path.ledger_dir, fn() {
        use cached <- result.try(ensure_cache_current_unlocked(ledger_path))
        use Nil <- result.try(validate_append_records_against_projection(
          cached.projection,
          records,
        ))
        use Nil <- result.try(append_prepared(
          ledger_path.current_path,
          records,
          fsync,
        ))
        persist_cached_append_unlocked(ledger_path, cached, records)
        Ok(Nil)
      })
  }
}

pub fn append_idempotent(
  ledger_path: LedgerPath,
  ledger_record: record.LedgerRecord,
  fsync: Bool,
) -> Result(AppendIdempotentResult, AppendIdempotentError) {
  use Nil <- result.try(
    ensure_layout(ledger_path)
    |> result.map_error(AppendLedgerError),
  )
  case
    with_ledger_lock(ledger_path.ledger_dir, fn() {
      use cached <- result.try(ensure_cache_current_unlocked(ledger_path))
      case
        ensure_record_absent_or_duplicate_unlocked(
          ledger_path,
          cached,
          ledger_record,
        )
      {
        Error(error) -> Error(error)
        Ok(RecordAlreadyRecorded(existing_record)) ->
          Ok(LockedAppendAlreadyRecorded(existing_record))
        Ok(RecordConflict(record_id)) -> Ok(LockedAppendConflict(record_id))
        Ok(RecordMissing) -> {
          use Nil <- result.try(
            validate_append_records_against_projection(cached.projection, [
              ledger_record,
            ]),
          )
          use Nil <- result.try(append_prepared(
            ledger_path.current_path,
            [ledger_record],
            fsync,
          ))
          persist_cached_append_unlocked(ledger_path, cached, [ledger_record])
          Ok(LockedAppendAppended)
        }
      }
    })
  {
    Error(error) -> Error(AppendLedgerError(error))
    Ok(LockedAppendAppended) -> Ok(Appended)
    Ok(LockedAppendAlreadyRecorded(existing)) -> Ok(AlreadyRecorded(existing))
    Ok(LockedAppendConflict(record_id)) -> Error(RecordIdConflict(record_id))
  }
}

pub fn append_workstream_start_records(
  ledger_path: LedgerPath,
  records: List(record.LedgerRecord),
  queued_record: record.LedgerRecord,
  fsync: Bool,
) -> Result(AppendWorkstreamStartResult, AppendWorkstreamStartError) {
  use queue <- result.try(queue_record_details(queued_record))
  use Nil <- result.try(
    ensure_layout(ledger_path)
    |> result.map_error(AppendStartLedgerError),
  )
  case
    with_ledger_lock(ledger_path.ledger_dir, fn() {
      use cached <- result.try(ensure_cache_current_unlocked(ledger_path))
      case existing_start_decision(cached.projection, queue) {
        ExistingStartDuplicate(existing_run) ->
          Ok(LockedStartDuplicate(existing_run))
        ExistingStartConflict(existing_run) ->
          Ok(LockedStartConflict(existing_run))
        NoExistingStart -> {
          use missing <- result.try(
            missing_records_with_cache_unlocked(
              ledger_path,
              cached,
              records,
              [],
            ),
          )
          case missing {
            MissingRecordConflict(record_id) ->
              Ok(LockedStartRecordConflict(record_id))
            MissingRecords(missing_records) -> {
              use Nil <- result.try(append_prepared(
                ledger_path.current_path,
                missing_records,
                fsync,
              ))
              persist_cached_append_unlocked(
                ledger_path,
                cached,
                missing_records,
              )
              Ok(LockedStartAppended)
            }
          }
        }
      }
    })
  {
    Error(error) -> Error(AppendStartLedgerError(error))
    Ok(LockedStartAppended) -> Ok(WorkstreamStartRecordsAppended)
    Ok(LockedStartDuplicate(existing_run)) ->
      Ok(WorkstreamStartRecordsDuplicate(existing_run))
    Ok(LockedStartConflict(existing_run)) ->
      Ok(WorkstreamStartRecordsConflict(existing_run))
    Ok(LockedStartRecordConflict(record_id)) ->
      Error(AppendStartRecordIdConflict(record_id))
  }
}

fn queue_record_details(
  ledger_record: record.LedgerRecord,
) -> Result(StartQueueRecord, AppendWorkstreamStartError) {
  case ledger_record.body {
    record.WorkstreamPhaseRunQueued(
      workstream_id,
      _,
      action_id,
      _,
      _,
      _,
      _,
      idempotency_key,
    ) ->
      Ok(StartQueueRecord(
        workstream_id: workstream_id,
        action_id: action_id,
        idempotency_key: idempotency_key,
      ))
    _ -> Error(AppendStartInvalidQueueRecord)
  }
}

fn existing_start_decision(
  projected: projection.Projection,
  queue: StartQueueRecord,
) -> ExistingStartDecision {
  case dict.get(projected.workstreams, queue.workstream_id) {
    Error(Nil) -> NoExistingStart
    Ok(workstream) -> {
      let action_runs =
        workstream.queued_phase_runs
        |> dict.values
        |> list.filter(fn(run) { run.action_id == queue.action_id })
      case
        action_runs
        |> list.find(fn(run) { run.idempotency_key != queue.idempotency_key })
      {
        Ok(existing_run) -> ExistingStartConflict(existing_run)
        Error(Nil) ->
          case action_runs |> list.first {
            Ok(existing_run) -> ExistingStartDuplicate(existing_run)
            Error(Nil) -> NoExistingStart
          }
      }
    }
  }
}

fn missing_records_with_cache_unlocked(
  ledger_path: LedgerPath,
  cached: CachedLedgerState,
  records: List(record.LedgerRecord),
  acc: List(record.LedgerRecord),
) -> Result(MissingRecordsDecision, LedgerError) {
  case records {
    [] -> Ok(MissingRecords(list.reverse(acc)))
    [ledger_record, ..rest] -> {
      use decision <- result.try(ensure_record_absent_or_duplicate_unlocked(
        ledger_path,
        cached_after_append(cached, list.reverse(acc)),
        ledger_record,
      ))
      case decision {
        RecordAlreadyRecorded(_) ->
          missing_records_with_cache_unlocked(ledger_path, cached, rest, acc)
        RecordConflict(record_id) -> Ok(MissingRecordConflict(record_id))
        RecordMissing ->
          missing_records_with_cache_unlocked(ledger_path, cached, rest, [
            ledger_record,
            ..acc
          ])
      }
    }
  }
}

pub fn read_records(
  ledger_path: LedgerPath,
) -> Result(ReadRecordsResult, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    read_records_unlocked(ledger_path)
  })
}

pub fn current_segment_stats(
  ledger_path: LedgerPath,
) -> Result(CurrentSegmentStats, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    use _snapshot_projection <- result.try(read_snapshot_projection(ledger_path))
    current_segment_stats_unlocked(ledger_path.current_path)
  })
}

pub fn storage_stats(
  ledger_path: LedgerPath,
) -> Result(LedgerStorageStats, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    use _snapshot_projection <- result.try(read_snapshot_projection(ledger_path))
    storage_stats_unlocked(ledger_path)
  })
}

pub fn replay(ledger_path: LedgerPath) -> Result(ReplayResult, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() { replay_unlocked(ledger_path) })
}

pub fn load_projection(
  ledger_path: LedgerPath,
) -> Result(projection.Projection, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    ensure_cache_current_unlocked(ledger_path)
    |> result.map(fn(cached) { cached.projection })
  })
}

pub fn cache_diagnostics(
  ledger_path: LedgerPath,
) -> Result(CacheDiagnostics, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    ensure_cache_current_unlocked(ledger_path)
    |> result.map(fn(cached) { cached.diagnostics })
  })
}

pub fn compact(ledger_path: LedgerPath) -> Result(Nil, LedgerError) {
  compact_with_report(ledger_path, fn() { 0 }) |> result.map(fn(_) { Nil })
}

pub fn compact_with_report(
  ledger_path: LedgerPath,
  now_ms: fn() -> Int,
) -> Result(CompactionReport, LedgerError) {
  use Nil <- result.try(ensure_layout(ledger_path))
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    compact_locked(ledger_path, now_ms)
  })
}

pub fn records_jsonl_byte_size(records: List(record.LedgerRecord)) -> Int {
  case records {
    [] -> 0
    _ -> {
      let contents =
        records
        |> list.map(record.to_string)
        |> string.join(with: "\n")

      bit_array.byte_size(bit_array.from_string(contents <> "\n"))
    }
  }
}

fn empty_cache_diagnostics() -> CacheDiagnostics {
  CacheDiagnostics(
    hydration_count: 0,
    reload_count: 0,
    fingerprint_mismatch_count: 0,
    cache_hit_count: 0,
    duplicate_probe_count: 0,
    record_id_index_size: 0,
  )
}

fn put_cached_state(ledger_path: LedgerPath, cached: CachedLedgerState) -> Nil {
  ledger_cache.put(ledger_path.ledger_dir, cached)
}

fn capture_fingerprint(
  ledger_path: LedgerPath,
) -> Result(fingerprint.LedgerFingerprint, LedgerError) {
  fingerprint.capture(
    ledger_path.snapshot_path,
    ledger_path.current_path,
    ledger_path.archive_dir,
  )
  |> result.map_error(Io)
}

fn update_cache_diagnostics(
  diagnostics: CacheDiagnostics,
  hydration_increment hydration_increment: Int,
  reload_increment reload_increment: Int,
  fingerprint_mismatch_increment fingerprint_mismatch_increment: Int,
  cache_hit_increment cache_hit_increment: Int,
  duplicate_probe_increment duplicate_probe_increment: Int,
  record_id_index_size record_id_index_size: Int,
) -> CacheDiagnostics {
  CacheDiagnostics(
    hydration_count: diagnostics.hydration_count + hydration_increment,
    reload_count: diagnostics.reload_count + reload_increment,
    fingerprint_mismatch_count: diagnostics.fingerprint_mismatch_count
      + fingerprint_mismatch_increment,
    cache_hit_count: diagnostics.cache_hit_count + cache_hit_increment,
    duplicate_probe_count: diagnostics.duplicate_probe_count
      + duplicate_probe_increment,
    record_id_index_size: record_id_index_size,
  )
}

fn cached_after_append(
  cached: CachedLedgerState,
  records: List(record.LedgerRecord),
) -> CachedLedgerState {
  let next_projection = projection.fold_from(cached.projection, records)
  let next_index =
    list.fold(records, cached.record_index, fn(index, ledger_record) {
      case record_index.insert(index, ledger_record, "current") {
        record_index.Inserted(updated) -> updated
        record_index.Duplicate(_) | record_index.Conflict(_) -> index
      }
    })
  CachedLedgerState(
    projection: next_projection,
    record_index: next_index,
    fingerprint: cached.fingerprint,
    diagnostics: update_cache_diagnostics(
      cached.diagnostics,
      hydration_increment: 0,
      reload_increment: 0,
      fingerprint_mismatch_increment: 0,
      cache_hit_increment: 0,
      duplicate_probe_increment: 0,
      record_id_index_size: record_index.size(next_index),
    ),
  )
}

fn persist_cached_append_unlocked(
  ledger_path: LedgerPath,
  cached: CachedLedgerState,
  records: List(record.LedgerRecord),
) -> Nil {
  let updated = cached_after_append(cached, records)
  case capture_fingerprint(ledger_path) {
    Ok(refreshed_fingerprint) ->
      put_cached_state(
        ledger_path,
        CachedLedgerState(
          projection: updated.projection,
          record_index: updated.record_index,
          fingerprint: refreshed_fingerprint,
          diagnostics: updated.diagnostics,
        ),
      )
    Error(_) -> ledger_cache.delete(ledger_path.ledger_dir)
  }
}

fn validate_append_records_against_projection(
  projected: projection.Projection,
  records: List(record.LedgerRecord),
) -> Result(Nil, LedgerError) {
  let known_runs =
    dict.keys(projected.workflow_runs)
    |> list.fold(dict.new(), fn(known, run_id) {
      dict.insert(known, run_id, True)
    })
  validate_append_records(records, known_runs)
}

fn ensure_cache_current_unlocked(
  ledger_path: LedgerPath,
) -> Result(CachedLedgerState, LedgerError) {
  let key = ledger_path.ledger_dir
  let maybe_cached: Option(CachedLedgerState) = ledger_cache.get(key)
  use current_fingerprint <- result.try(capture_fingerprint(ledger_path))
  case maybe_cached {
    Some(cached) ->
      case cached.fingerprint == current_fingerprint {
        True -> {
          let updated =
            CachedLedgerState(
              projection: cached.projection,
              record_index: cached.record_index,
              fingerprint: cached.fingerprint,
              diagnostics: update_cache_diagnostics(
                cached.diagnostics,
                hydration_increment: 0,
                reload_increment: 0,
                fingerprint_mismatch_increment: 0,
                cache_hit_increment: 1,
                duplicate_probe_increment: 0,
                record_id_index_size: record_index.size(cached.record_index),
              ),
            )
          put_cached_state(ledger_path, updated)
          Ok(updated)
        }
        False -> {
          let diagnostics =
            update_cache_diagnostics(
              cached.diagnostics,
              hydration_increment: 0,
              reload_increment: 0,
              fingerprint_mismatch_increment: 1,
              cache_hit_increment: 0,
              duplicate_probe_increment: 0,
              record_id_index_size: record_index.size(cached.record_index),
            )
          hydrate_cache_from_disk_unlocked(
            ledger_path,
            current_fingerprint,
            diagnostics,
            is_reload: True,
          )
        }
      }
    None ->
      hydrate_cache_from_disk_unlocked(
        ledger_path,
        current_fingerprint,
        empty_cache_diagnostics(),
        is_reload: False,
      )
  }
}

fn hydrate_cache_from_disk_unlocked(
  ledger_path: LedgerPath,
  current_fingerprint: fingerprint.LedgerFingerprint,
  diagnostics: CacheDiagnostics,
  is_reload is_reload: Bool,
) -> Result(CachedLedgerState, LedgerError) {
  use snapshot <- result.try(read_snapshot_state(ledger_path))
  let base_index = case snapshot.metadata {
    Some(index) -> Ok(index)
    None -> hydrate_archive_index_unlocked(ledger_path)
  }
  use hydrated_index <- result.try(base_index)
  use combined <- result.try(fold_current_segment_with_index(
    ledger_path.current_path,
    snapshot.projection,
    hydrated_index,
  ))
  let #(combined_projection, combined_index) = combined
  let diagnostics =
    update_cache_diagnostics(
      diagnostics,
      hydration_increment: 1,
      reload_increment: case is_reload {
        True -> 1
        False -> 0
      },
      fingerprint_mismatch_increment: 0,
      cache_hit_increment: 0,
      duplicate_probe_increment: 0,
      record_id_index_size: record_index.size(combined_index),
    )
  let cached =
    CachedLedgerState(
      projection: combined_projection,
      record_index: combined_index,
      fingerprint: current_fingerprint,
      diagnostics: diagnostics,
    )
  put_cached_state(ledger_path, cached)
  Ok(cached)
}

fn hydrate_archive_index_unlocked(
  ledger_path: LedgerPath,
) -> Result(record_index.RecordIndex, LedgerError) {
  use archive_paths <- result.try(archive_segment_paths(ledger_path))
  fold_segment_paths_for_index(archive_paths, record_index.new())
}

fn fold_segment_paths_for_index(
  paths: List(String),
  index: record_index.RecordIndex,
) -> Result(record_index.RecordIndex, LedgerError) {
  case paths {
    [] -> Ok(index)
    [segment_path, ..rest] -> {
      use next <- result.try(fold_segment_index(segment_path, index, "archive"))
      fold_segment_paths_for_index(rest, next)
    }
  }
}

fn replay_unlocked(
  ledger_path: LedgerPath,
) -> Result(ReplayResult, LedgerError) {
  use snapshot_projection <- result.try(read_snapshot_projection(ledger_path))
  use read <- result.try(read_records_unlocked(ledger_path))
  Ok(ReplayResult(
    records: read.records,
    projection: projection.fold_from(snapshot_projection, read.records),
    truncated_tail: read.truncated_tail,
  ))
}

fn compact_locked(
  ledger_path: LedgerPath,
  now_ms: fn() -> Int,
) -> Result(CompactionReport, LedgerError) {
  let started_at_ms = now_ms()
  use before <- result.try(storage_stats_unlocked(ledger_path))
  use cached <- result.try(ensure_cache_current_unlocked(ledger_path))
  use snapshot <- result.try(read_snapshot_state(ledger_path))
  use Nil <- result.try(write_snapshot_atomically(
    ledger_path,
    cached.projection,
    cached.record_index,
  ))
  case archive_current_segment(ledger_path, before.archive_segment_count + 1) {
    Ok(archived_nonempty_current) -> {
      use refreshed_fingerprint <- result.try(capture_fingerprint(ledger_path))
      let refreshed_cache =
        CachedLedgerState(
          projection: cached.projection,
          record_index: cached.record_index,
          fingerprint: refreshed_fingerprint,
          diagnostics: update_cache_diagnostics(
            cached.diagnostics,
            hydration_increment: 0,
            reload_increment: 1,
            fingerprint_mismatch_increment: 0,
            cache_hit_increment: 0,
            duplicate_probe_increment: 0,
            record_id_index_size: record_index.size(cached.record_index),
          ),
        )
      put_cached_state(ledger_path, refreshed_cache)
      use current <- result.try(current_segment_stats_unlocked(
        ledger_path.current_path,
      ))
      use snapshot_size_bytes <- result.try(file_size_bytes_or_zero(
        ledger_path.snapshot_path,
        "inspect ledger snapshot",
      ))
      let after =
        LedgerStorageStats(
          current: current,
          snapshot_size_bytes: snapshot_size_bytes,
          archive_segment_count: before.archive_segment_count
            + archived_segment_delta(archived_nonempty_current),
        )
      Ok(CompactionReport(
        before: before,
        after: after,
        duration_ms: now_ms() - started_at_ms,
      ))
    }
    Error(error) -> {
      ledger_cache.delete(ledger_path.ledger_dir)
      case restore_snapshot(ledger_path, snapshot) {
        Ok(Nil) -> Error(error)
        Error(restore_error) -> Error(restore_error)
      }
    }
  }
}

fn ensure_layout(ledger_path: LedgerPath) -> Result(Nil, LedgerError) {
  case simplifile.create_directory_all(ledger_path.archive_dir) {
    Ok(Nil) -> Ok(Nil)
    Error(error) -> Error(Io(file_error("create ledger directories", error)))
  }
}

fn find_record_by_id_unlocked(
  ledger_path: LedgerPath,
  record_id: String,
) -> Result(Option(record.LedgerRecord), LedgerError) {
  use current <- result.try(find_record_by_id_in_segment(
    ledger_path.current_path,
    record_id,
  ))
  case current {
    Some(_) -> Ok(current)
    None -> {
      use archive_paths <- result.try(archive_segment_paths(ledger_path))
      find_record_by_id_in_segments(archive_paths, record_id)
    }
  }
}

fn find_record_by_id_in_segments(
  paths: List(String),
  record_id: String,
) -> Result(Option(record.LedgerRecord), LedgerError) {
  case paths {
    [] -> Ok(None)
    [segment_path, ..rest] -> {
      use found <- result.try(find_record_by_id_in_segment(
        segment_path,
        record_id,
      ))
      case found {
        Some(_) -> Ok(found)
        None -> find_record_by_id_in_segments(rest, record_id)
      }
    }
  }
}

fn archive_segment_paths(
  ledger_path: LedgerPath,
) -> Result(List(String), LedgerError) {
  case simplifile.read_directory(ledger_path.archive_dir) {
    Ok(entries) ->
      entries
      |> list.filter(fn(entry) {
        string.starts_with(entry, "segment-")
        && string.ends_with(entry, ".jsonl")
      })
      |> list.sort(by: string.compare)
      |> list.map(fn(entry) { path.join(ledger_path.archive_dir, entry) })
      |> Ok
    Error(simplifile.Enoent) | Error(simplifile.Enotdir) -> Ok([])
    Error(error) ->
      Error(Io(file_error("read ledger archive directory", error)))
  }
}

fn find_record_by_id_in_segment(
  segment_path: String,
  record_id: String,
) -> Result(Option(record.LedgerRecord), LedgerError) {
  let initial = JsonlFold(value: None, error: None, truncated_tail: False)
  case fold_lines(segment_path, initial, record_id_lookup_step(record_id)) {
    Ok(JsonlFold(value: found, error: None, truncated_tail: _)) -> Ok(found)
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) -> Ok(None)
    Error(error) -> Error(LedgerFfiFailed(error))
  }
}

fn record_id_lookup_step(
  record_id: String,
) -> fn(JsonlFold(Option(record.LedgerRecord)), String, Int, Bool) ->
  JsonlFold(Option(record.LedgerRecord)) {
  fn(
    state: JsonlFold(Option(record.LedgerRecord)),
    line: String,
    line_number: Int,
    is_last: Bool,
  ) {
    case state.error {
      Some(_) -> state
      None ->
        case parse_jsonl_line(line, line_number, is_last) {
          Ok(ParsedRecord(ledger_record)) ->
            JsonlFold(
              value: select_found_record(state.value, ledger_record, record_id),
              error: None,
              truncated_tail: state.truncated_tail,
            )
          Ok(EmptyTrailingLine) -> state
          Ok(TruncatedTail) ->
            JsonlFold(value: state.value, error: None, truncated_tail: True)
          Error(error) ->
            JsonlFold(
              value: state.value,
              error: Some(error),
              truncated_tail: state.truncated_tail,
            )
        }
    }
  }
}

fn select_found_record(
  existing: Option(record.LedgerRecord),
  ledger_record: record.LedgerRecord,
  record_id: String,
) -> Option(record.LedgerRecord) {
  case existing, ledger_record.record_id == record_id {
    Some(_), _ -> existing
    None, True -> Some(ledger_record)
    None, False -> None
  }
}

fn ensure_record_absent_or_duplicate_unlocked(
  ledger_path: LedgerPath,
  cached: CachedLedgerState,
  ledger_record: record.LedgerRecord,
) -> Result(RecordPresenceDecision, LedgerError) {
  case record_index.get(cached.record_index, ledger_record.record_id) {
    Error(Nil) -> Ok(RecordMissing)
    Ok(index_entry) ->
      case index_entry.body_sha256 == record_index.body_sha256(ledger_record) {
        False -> Ok(RecordConflict(ledger_record.record_id))
        True -> {
          let probed =
            CachedLedgerState(
              projection: cached.projection,
              record_index: cached.record_index,
              fingerprint: cached.fingerprint,
              diagnostics: update_cache_diagnostics(
                cached.diagnostics,
                hydration_increment: 0,
                reload_increment: 0,
                fingerprint_mismatch_increment: 0,
                cache_hit_increment: 0,
                duplicate_probe_increment: 1,
                record_id_index_size: record_index.size(cached.record_index),
              ),
            )
          put_cached_state(ledger_path, probed)
          use existing <- result.try(find_record_by_id_unlocked(
            ledger_path,
            ledger_record.record_id,
          ))
          case existing {
            Some(existing_record) ->
              case existing_record.body == ledger_record.body {
                True -> Ok(RecordAlreadyRecorded(existing_record))
                False -> Ok(RecordConflict(ledger_record.record_id))
              }
            None -> Ok(RecordMissing)
          }
        }
      }
  }
}

fn validate_append_records(
  records: List(record.LedgerRecord),
  known_runs: dict.Dict(String, Bool),
) -> Result(Nil, LedgerError) {
  case records {
    [] -> Ok(Nil)
    [ledger_record, ..rest] ->
      case append_record_workflow_requirement(ledger_record.body) {
        AddWorkflowRun(run_id) ->
          validate_append_records(rest, dict.insert(known_runs, run_id, True))
        RequireKnownWorkflowRun(reason, run_id) ->
          case dict.has_key(known_runs, run_id) {
            True -> validate_append_records(rest, known_runs)
            False -> Error(AggregateInvariantViolation(reason, run_id))
          }
        NoWorkflowRunRequirement -> validate_append_records(rest, known_runs)
      }
  }
}

type AppendRecordWorkflowRequirement {
  AddWorkflowRun(run_id: String)
  RequireKnownWorkflowRun(reason: String, run_id: String)
  NoWorkflowRunRequirement
}

fn append_record_workflow_requirement(
  body: record.RecordBody,
) -> AppendRecordWorkflowRequirement {
  case body {
    record.WorkflowRunStarted(run_id, _, _, _, _, _, _, _)
    | record.WorkflowRunStartedWithTask(run_id, _, _, _, _, _, _, _, _) ->
      AddWorkflowRun(run_id)
    record.WorkflowRunFinished(run_id, _, _, _, _, _)
    | record.WorkflowRunFinishedWithTask(run_id, _, _, _, _, _, _)
    | record.WorkflowRunInterrupted(run_id, _, _, _)
    | record.WorkflowRunSuperseded(run_id, _, _, _, _) ->
      RequireKnownWorkflowRun("unknown_workflow_run", run_id)
    record.StepAttemptPrepared(run_id, _, _, _, _, _, _, _, _)
    | record.StepAttemptStarted(run_id, _, _, _, _, _, _)
    | record.StepAttemptContinuationStarted(run_id, _, _, _, _)
    | record.StepAttemptPiSessionRecorded(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptPiSessionRecordedWithTask(
        run_id,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
      )
    | record.StepAttemptFinished(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptInterrupted(run_id, _, _, _, _)
    | record.StepAttemptSuperseded(run_id, _, _, _, _, _) ->
      RequireKnownWorkflowRun(
        "orphan_step_attempt_without_workflow_run",
        run_id,
      )
    _ -> NoWorkflowRunRequirement
  }
}

fn append_prepared(
  current_path: String,
  records: List(record.LedgerRecord),
  fsync: Bool,
) -> Result(Nil, LedgerError) {
  case records {
    [] -> Ok(Nil)
    _ -> {
      let contents =
        records
        |> list.map(record.to_string)
        |> string.join(with: "\n")

      append_lines(current_path, contents <> "\n", fsync)
      |> result.map_error(fn(error) { LedgerFfiFailed(error) })
    }
  }
}

fn storage_stats_unlocked(
  ledger_path: LedgerPath,
) -> Result(LedgerStorageStats, LedgerError) {
  use current <- result.try(current_segment_stats_unlocked(
    ledger_path.current_path,
  ))
  use snapshot_size_bytes <- result.try(file_size_bytes_or_zero(
    ledger_path.snapshot_path,
    "inspect ledger snapshot",
  ))
  use archive_segment_count <- result.try(archive_segment_count_unlocked(
    ledger_path.archive_dir,
  ))
  Ok(LedgerStorageStats(
    current: current,
    snapshot_size_bytes: snapshot_size_bytes,
    archive_segment_count: archive_segment_count,
  ))
}

fn file_size_bytes_or_zero(
  file_path: String,
  operation: String,
) -> Result(Int, LedgerError) {
  case simplifile.file_info(file_path) {
    Ok(info) -> Ok(info.size)
    Error(simplifile.Enoent) -> Ok(0)
    Error(error) -> Error(Io(file_error(operation, error)))
  }
}

fn archive_segment_count_unlocked(
  archive_dir: String,
) -> Result(Int, LedgerError) {
  case simplifile.read_directory(archive_dir) {
    Ok(entries) ->
      entries
      |> list.filter(fn(entry) {
        string.starts_with(entry, "segment-")
        && string.ends_with(entry, ".jsonl")
      })
      |> list.length
      |> Ok
    Error(simplifile.Enoent) -> Ok(0)
    Error(error) ->
      Error(Io(file_error("read ledger archive directory", error)))
  }
}

fn read_records_unlocked(
  ledger_path: LedgerPath,
) -> Result(ReadRecordsResult, LedgerError) {
  let initial = JsonlFold(value: [], error: None, truncated_tail: False)
  case fold_lines(ledger_path.current_path, initial, record_fold_step) {
    Ok(JsonlFold(value: records, error: None, truncated_tail: truncated_tail)) ->
      Ok(ReadRecordsResult(
        records: list.reverse(records),
        truncated_tail: truncated_tail,
      ))
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) ->
      Ok(ReadRecordsResult(records: [], truncated_tail: False))
    Error(error) -> Error(LedgerFfiFailed(error))
  }
}

fn current_segment_stats_unlocked(
  current_path: String,
) -> Result(CurrentSegmentStats, LedgerError) {
  let byte_size_result = case simplifile.file_info(current_path) {
    Ok(info) -> Ok(Some(info.size))
    Error(simplifile.Enoent) -> Ok(None)
    Error(error) -> Error(Io(file_error("inspect current ledger", error)))
  }
  use byte_size <- result.try(byte_size_result)
  let initial = JsonlFold(value: 0, error: None, truncated_tail: False)
  case fold_lines(current_path, initial, current_segment_stats_fold_step) {
    Ok(JsonlFold(value: count, error: None, truncated_tail: truncated_tail)) ->
      Ok(CurrentSegmentStats(
        record_count: count,
        byte_size: byte_size |> option.unwrap(0),
        truncated_tail: truncated_tail,
      ))
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) ->
      Ok(CurrentSegmentStats(
        record_count: 0,
        byte_size: 0,
        truncated_tail: False,
      ))
    Error(error) -> Error(LedgerFfiFailed(error))
  }
}

fn record_fold_step(
  state: JsonlFold(List(record.LedgerRecord)),
  line: String,
  line_number: Int,
  is_last: Bool,
) -> JsonlFold(List(record.LedgerRecord)) {
  case state.error {
    Some(_) -> state
    None ->
      case parse_jsonl_line(line, line_number, is_last) {
        Ok(ParsedRecord(ledger_record)) ->
          JsonlFold(
            value: [ledger_record, ..state.value],
            error: None,
            truncated_tail: state.truncated_tail,
          )
        Ok(EmptyTrailingLine) -> state
        Ok(TruncatedTail) ->
          JsonlFold(value: state.value, error: None, truncated_tail: True)
        Error(error) ->
          JsonlFold(
            value: state.value,
            error: Some(error),
            truncated_tail: state.truncated_tail,
          )
      }
  }
}

fn current_segment_stats_fold_step(
  state: JsonlFold(Int),
  line: String,
  line_number: Int,
  is_last: Bool,
) -> JsonlFold(Int) {
  case state.error {
    Some(_) -> state
    None ->
      case parse_jsonl_line(line, line_number, is_last) {
        Ok(ParsedRecord(_)) ->
          JsonlFold(
            value: state.value + 1,
            error: None,
            truncated_tail: state.truncated_tail,
          )
        Ok(EmptyTrailingLine) -> state
        Ok(TruncatedTail) ->
          JsonlFold(value: state.value, error: None, truncated_tail: True)
        Error(error) ->
          JsonlFold(
            value: state.value,
            error: Some(error),
            truncated_tail: state.truncated_tail,
          )
      }
  }
}

fn fold_current_segment_with_index(
  current_path: String,
  snapshot_projection: projection.Projection,
  snapshot_index: record_index.RecordIndex,
) -> Result(#(projection.Projection, record_index.RecordIndex), LedgerError) {
  let initial =
    JsonlFold(
      value: #(snapshot_projection, snapshot_index),
      error: None,
      truncated_tail: False,
    )
  case
    fold_lines(current_path, initial, projection_and_index_fold_step("current"))
  {
    Ok(JsonlFold(value: combined, error: None, truncated_tail: _)) ->
      Ok(combined)
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) -> Ok(#(snapshot_projection, snapshot_index))
    Error(error) -> Error(LedgerFfiFailed(error))
  }
}

fn fold_segment_index(
  segment_path: String,
  initial_index: record_index.RecordIndex,
  storage: String,
) -> Result(record_index.RecordIndex, LedgerError) {
  let initial =
    JsonlFold(value: initial_index, error: None, truncated_tail: False)
  case fold_lines(segment_path, initial, record_index_fold_step(storage)) {
    Ok(JsonlFold(value: index, error: None, truncated_tail: _)) -> Ok(index)
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) -> Ok(initial_index)
    Error(error) -> Error(LedgerFfiFailed(error))
  }
}

fn projection_and_index_fold_step(
  storage: String,
) -> fn(
  JsonlFold(#(projection.Projection, record_index.RecordIndex)),
  String,
  Int,
  Bool,
) -> JsonlFold(#(projection.Projection, record_index.RecordIndex)) {
  fn(
    state: JsonlFold(#(projection.Projection, record_index.RecordIndex)),
    line: String,
    line_number: Int,
    is_last: Bool,
  ) {
    case state.error {
      Some(_) -> state
      None ->
        case parse_jsonl_line(line, line_number, is_last) {
          Ok(ParsedRecord(ledger_record)) -> {
            let #(projected, index) = state.value
            case record_index.insert(index, ledger_record, storage) {
              record_index.Inserted(_) | record_index.Duplicate(_) ->
                JsonlFold(
                  value: #(
                    projection.apply(projected, ledger_record),
                    insert_index_or_keep(index, ledger_record, storage),
                  ),
                  error: None,
                  truncated_tail: state.truncated_tail,
                )
              record_index.Conflict(_) ->
                JsonlFold(
                  value: state.value,
                  error: Some(CorruptRecord(
                    line_number,
                    "conflicting record bodies for record id "
                      <> ledger_record.record_id,
                  )),
                  truncated_tail: state.truncated_tail,
                )
            }
          }
          Ok(EmptyTrailingLine) -> state
          Ok(TruncatedTail) ->
            JsonlFold(value: state.value, error: None, truncated_tail: True)
          Error(error) ->
            JsonlFold(
              value: state.value,
              error: Some(error),
              truncated_tail: state.truncated_tail,
            )
        }
    }
  }
}

fn record_index_fold_step(
  storage: String,
) -> fn(JsonlFold(record_index.RecordIndex), String, Int, Bool) ->
  JsonlFold(record_index.RecordIndex) {
  fn(
    state: JsonlFold(record_index.RecordIndex),
    line: String,
    line_number: Int,
    is_last: Bool,
  ) {
    case state.error {
      Some(_) -> state
      None ->
        case parse_jsonl_line(line, line_number, is_last) {
          Ok(ParsedRecord(ledger_record)) ->
            case record_index.insert(state.value, ledger_record, storage) {
              record_index.Inserted(next_index) ->
                JsonlFold(
                  value: next_index,
                  error: None,
                  truncated_tail: state.truncated_tail,
                )
              record_index.Duplicate(_) -> state
              record_index.Conflict(_) ->
                JsonlFold(
                  value: state.value,
                  error: Some(CorruptRecord(
                    line_number,
                    "conflicting record bodies for record id "
                      <> ledger_record.record_id,
                  )),
                  truncated_tail: state.truncated_tail,
                )
            }
          Ok(EmptyTrailingLine) -> state
          Ok(TruncatedTail) ->
            JsonlFold(value: state.value, error: None, truncated_tail: True)
          Error(error) ->
            JsonlFold(
              value: state.value,
              error: Some(error),
              truncated_tail: state.truncated_tail,
            )
        }
    }
  }
}

fn insert_index_or_keep(
  index: record_index.RecordIndex,
  ledger_record: record.LedgerRecord,
  storage: String,
) -> record_index.RecordIndex {
  case record_index.insert(index, ledger_record, storage) {
    record_index.Inserted(next_index) -> next_index
    record_index.Duplicate(_) | record_index.Conflict(_) -> index
  }
}

fn parse_jsonl_line(
  line: String,
  line_number: Int,
  is_last: Bool,
) -> Result(ParsedLine, LedgerError) {
  case string.trim(line) == "" {
    True ->
      case is_last {
        True -> Ok(EmptyTrailingLine)
        False -> Error(CorruptRecord(line_number, "empty JSONL line"))
      }
    False ->
      case record.decode_string(line) {
        Ok(ledger_record) -> Ok(ParsedRecord(ledger_record))
        Error(record.MalformedJson(_)) ->
          case is_last {
            True -> Ok(TruncatedTail)
            False ->
              Error(record_decode_error(
                line_number,
                record.MalformedJson("malformed JSON"),
              ))
          }
        Error(error) -> Error(record_decode_error(line_number, error))
      }
  }
}

fn record_decode_error(
  line_number: Int,
  error: record.DecodeError,
) -> LedgerError {
  case error {
    record.UnsupportedVersion(version) -> UnsupportedVersion(version)
    other -> CorruptRecord(line_number, record.describe_error(other))
  }
}

fn read_snapshot_projection(
  ledger_path: LedgerPath,
) -> Result(projection.Projection, LedgerError) {
  read_snapshot_state(ledger_path)
  |> result.map(fn(snapshot) { snapshot.projection })
}

fn read_snapshot_state(
  ledger_path: LedgerPath,
) -> Result(SnapshotRead, LedgerError) {
  case simplifile.read(ledger_path.snapshot_path) {
    Ok(contents) -> {
      let projection_result = case projection.decode_string(contents) {
        Ok(snapshot_projection) -> Ok(snapshot_projection)
        Error(projection.UnsupportedSnapshotVersion(version)) ->
          Error(UnsupportedVersion(version))
        Error(error) ->
          Error(CorruptRecord(0, projection.describe_decode_error(error)))
      }
      use snapshot_projection <- result.try(projection_result)
      let metadata = case record_index.decode_snapshot_metadata(contents) {
        Ok(index) -> index
        Error(_) -> None
      }
      Ok(SnapshotRead(projection: snapshot_projection, metadata: metadata))
    }
    Error(simplifile.Enoent) ->
      Ok(SnapshotRead(projection: projection.new(), metadata: None))
    Error(error) -> Error(Io(file_error("read ledger snapshot", error)))
  }
}

fn write_snapshot_atomically(
  ledger_path: LedgerPath,
  snapshot_projection: projection.Projection,
  snapshot_index: record_index.RecordIndex,
) -> Result(Nil, LedgerError) {
  projection.to_json_with_extra_fields(snapshot_projection, [
    #(
      record_index.snapshot_metadata_field,
      record_index.snapshot_metadata_json(snapshot_index),
    ),
  ])
  |> json.to_string
  |> write_snapshot_contents_atomically(ledger_path)
}

fn write_snapshot_contents_atomically(
  contents: String,
  ledger_path: LedgerPath,
) -> Result(Nil, LedgerError) {
  let temp_path = ledger_path.snapshot_path <> ".tmp"
  case simplifile.write(temp_path, contents <> "\n") {
    Error(error) ->
      Error(Io(file_error("write temporary ledger snapshot", error)))
    Ok(Nil) ->
      case simplifile.rename(temp_path, ledger_path.snapshot_path) {
        Ok(Nil) -> Ok(Nil)
        Error(error) -> Error(Io(file_error("rename ledger snapshot", error)))
      }
  }
}

fn restore_snapshot(
  ledger_path: LedgerPath,
  snapshot: SnapshotRead,
) -> Result(Nil, LedgerError) {
  let contents = case snapshot.metadata {
    Some(index) ->
      projection.to_json_with_extra_fields(snapshot.projection, [
        #(
          record_index.snapshot_metadata_field,
          record_index.snapshot_metadata_json(index),
        ),
      ])
      |> json.to_string
    None -> projection.to_string(snapshot.projection)
  }
  write_snapshot_contents_atomically(contents, ledger_path)
}

fn archived_segment_delta(archived_nonempty_current: Bool) -> Int {
  case archived_nonempty_current {
    True -> 1
    False -> 0
  }
}

fn archive_current_segment(
  ledger_path: LedgerPath,
  next_archive_segment_number: Int,
) -> Result(Bool, LedgerError) {
  case simplifile.file_info(ledger_path.current_path) {
    Error(simplifile.Enoent) ->
      simplifile.write(ledger_path.current_path, "")
      |> map_io("create empty current ledger")
      |> result.map(fn(_) { False })
    Error(error) ->
      Error(Io(file_error("inspect current ledger for archive", error)))
    Ok(info) ->
      case info.size == 0 {
        True ->
          simplifile.write(ledger_path.current_path, "")
          |> map_io("truncate current ledger")
          |> result.map(fn(_) { False })
        False -> {
          let archive_path =
            archive_path_for_segment_number(
              ledger_path,
              next_archive_segment_number,
            )
          case simplifile.rename(ledger_path.current_path, archive_path) {
            Error(error) ->
              Error(Io(file_error("archive current ledger", error)))
            Ok(Nil) ->
              simplifile.write(ledger_path.current_path, "")
              |> map_io("create new current ledger")
              |> result.map(fn(_) { True })
          }
        }
      }
  }
}

fn archive_path_for_segment_number(
  ledger_path: LedgerPath,
  segment_number: Int,
) -> String {
  path.join(
    ledger_path.archive_dir,
    "segment-" <> int.to_string(segment_number) <> ".jsonl",
  )
}

fn map_io(
  result: Result(Nil, simplifile.FileError),
  operation: String,
) -> Result(Nil, LedgerError) {
  case result {
    Ok(Nil) -> Ok(Nil)
    Error(error) -> Error(Io(file_error(operation, error)))
  }
}

fn file_error(operation: String, error: simplifile.FileError) -> String {
  operation <> ": " <> simplifile.describe_error(error)
}

fn append_lines(
  path: String,
  contents: String,
  fsync: Bool,
) -> Result(Nil, LedgerFfiError) {
  ffi_append_lines(path, contents, fsync)
  |> result.map_error(fn(error) { raw_ledger_error("append_lines", error) })
}

fn fold_lines(
  path: String,
  initial: a,
  step: fn(a, String, Int, Bool) -> a,
) -> Result(a, LedgerFfiError) {
  ffi_fold_lines(path, initial, step)
  |> result.map_error(fn(error) { raw_ledger_error("fold_lines", error) })
}

pub fn ledger_error_code(error: LedgerError) -> String {
  case error {
    Io(_) -> "io"
    LedgerFfiFailed(_) -> "ledger_ffi_failed"
    UnsupportedVersion(_) -> "unsupported_version"
    CorruptRecord(_, _) -> "corrupt_record"
    AggregateInvariantViolation(reason, _) -> reason
  }
}

pub fn ledger_error_to_string(error: LedgerError) -> String {
  case error {
    Io(message) -> message
    LedgerFfiFailed(error) -> ledger_ffi_error_to_string(error)
    UnsupportedVersion(version) ->
      "unsupported ledger schema version " <> int.to_string(version)
    CorruptRecord(line, reason) ->
      "corrupt ledger record at line " <> int.to_string(line) <> ": " <> reason
    AggregateInvariantViolation(reason, run_id) ->
      reason <> ": workflow run " <> run_id <> " is not present in local state"
  }
}

pub fn ledger_ffi_error_to_string(error: LedgerFfiError) -> String {
  case error {
    OpenFailed(reason) -> "open failed: " <> reason
    WriteFailed(reason) -> "write failed: " <> reason
    SyncFailed(reason) -> "sync failed: " <> reason
    CloseFailed(reason) -> "close failed: " <> reason
    ReadFailed(reason) -> "read failed: " <> reason
    StepFailed(reason) -> "step callback failed: " <> reason
    LockFailed(reason) -> "ledger lock failed: " <> reason
    UnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
  }
}

fn raw_ledger_error(function: String, error: String) -> LedgerFfiError {
  let #(tag, detail) = split_tag(error)
  case tag {
    "open" -> OpenFailed(detail)
    "write" -> WriteFailed(detail)
    "sync" -> SyncFailed(detail)
    "close" -> CloseFailed(detail)
    "read" -> ReadFailed(detail)
    "step" -> StepFailed(detail)
    "lock" -> LockFailed(detail)
    "unexpected_ffi_failure" -> UnexpectedFfiFailure(function, detail)
    _ -> UnexpectedFfiFailure(function, error)
  }
}

fn split_tag(error: String) -> #(String, String) {
  case string.split_once(error, on: ":") {
    Ok(#(tag, detail)) -> #(tag, detail)
    Error(Nil) -> #(error, "")
  }
}

@external(erlang, "scherzo_state_ffi", "append_lines")
fn ffi_append_lines(
  path: String,
  contents: String,
  fsync: Bool,
) -> Result(Nil, String)

@external(erlang, "scherzo_state_ffi", "fold_lines")
fn ffi_fold_lines(
  path: String,
  initial: a,
  step: fn(a, String, Int, Bool) -> a,
) -> Result(a, String)

@external(erlang, "scherzo_state_ffi", "with_ledger_lock")
fn with_ledger_lock(
  ledger_key: String,
  operation: fn() -> Result(a, LedgerError),
) -> Result(a, LedgerError)
