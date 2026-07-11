import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/path
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
        use Nil <- result.try(validate_append_batch_unlocked(
          ledger_path,
          records,
        ))
        append_prepared(ledger_path.current_path, records, fsync)
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
      use existing <- result.try(find_record_by_id_unlocked(
        ledger_path,
        ledger_record.record_id,
      ))
      case existing {
        Some(existing_record) ->
          case existing_record.body == ledger_record.body {
            True -> Ok(LockedAppendAlreadyRecorded(existing_record))
            False -> Ok(LockedAppendConflict(ledger_record.record_id))
          }
        None -> {
          use Nil <- result.try(
            validate_append_batch_unlocked(ledger_path, [ledger_record]),
          )
          append_prepared(ledger_path.current_path, [ledger_record], fsync)
          |> result.map(fn(_) { LockedAppendAppended })
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
      use projected <- result.try(load_projection_unlocked(ledger_path))
      case existing_start_decision(projected, queue) {
        ExistingStartDuplicate(existing_run) ->
          Ok(LockedStartDuplicate(existing_run))
        ExistingStartConflict(existing_run) ->
          Ok(LockedStartConflict(existing_run))
        NoExistingStart -> {
          use missing <- result.try(
            missing_records_unlocked(ledger_path, records, []),
          )
          case missing {
            MissingRecordConflict(record_id) ->
              Ok(LockedStartRecordConflict(record_id))
            MissingRecords(missing_records) ->
              append_prepared(ledger_path.current_path, missing_records, fsync)
              |> result.map(fn(_) { LockedStartAppended })
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

fn missing_records_unlocked(
  ledger_path: LedgerPath,
  records: List(record.LedgerRecord),
  acc: List(record.LedgerRecord),
) -> Result(MissingRecordsDecision, LedgerError) {
  case records {
    [] -> Ok(MissingRecords(list.reverse(acc)))
    [ledger_record, ..rest] -> {
      use existing <- result.try(find_record_by_id_unlocked(
        ledger_path,
        ledger_record.record_id,
      ))
      case existing {
        Some(existing_record) ->
          case existing_record.body == ledger_record.body {
            True -> missing_records_unlocked(ledger_path, rest, acc)
            False -> Ok(MissingRecordConflict(ledger_record.record_id))
          }
        None ->
          missing_records_unlocked(ledger_path, rest, [ledger_record, ..acc])
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
    use _snapshot_projection <- result.try(read_snapshot(ledger_path))
    current_segment_stats_unlocked(ledger_path.current_path)
  })
}

pub fn storage_stats(
  ledger_path: LedgerPath,
) -> Result(LedgerStorageStats, LedgerError) {
  with_ledger_lock(ledger_path.ledger_dir, fn() {
    use _snapshot_projection <- result.try(read_snapshot(ledger_path))
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
    load_projection_unlocked(ledger_path)
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

fn replay_unlocked(
  ledger_path: LedgerPath,
) -> Result(ReplayResult, LedgerError) {
  use snapshot_projection <- result.try(read_snapshot(ledger_path))
  use read <- result.try(read_records_unlocked(ledger_path))
  Ok(ReplayResult(
    records: read.records,
    projection: projection.fold_from(snapshot_projection, read.records),
    truncated_tail: read.truncated_tail,
  ))
}

fn load_projection_unlocked(
  ledger_path: LedgerPath,
) -> Result(projection.Projection, LedgerError) {
  use snapshot_projection <- result.try(read_snapshot(ledger_path))
  fold_current_segment_streaming(ledger_path.current_path, snapshot_projection)
}

fn compact_locked(
  ledger_path: LedgerPath,
  now_ms: fn() -> Int,
) -> Result(CompactionReport, LedgerError) {
  let started_at_ms = now_ms()
  use before <- result.try(storage_stats_unlocked(ledger_path))
  use snapshot_projection <- result.try(read_snapshot(ledger_path))
  use compacted_projection <- result.try(fold_current_segment_streaming(
    ledger_path.current_path,
    snapshot_projection,
  ))
  use Nil <- result.try(write_snapshot_atomically(
    ledger_path,
    compacted_projection,
  ))
  case archive_current_segment(ledger_path, before.archive_segment_count + 1) {
    Ok(archived_nonempty_current) -> {
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
    Error(error) ->
      case restore_snapshot(ledger_path, snapshot_projection) {
        Ok(Nil) -> Error(error)
        Error(restore_error) -> Error(restore_error)
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

fn validate_append_batch_unlocked(
  ledger_path: LedgerPath,
  records: List(record.LedgerRecord),
) -> Result(Nil, LedgerError) {
  use projected <- result.try(load_projection_unlocked(ledger_path))
  let known_runs =
    dict.keys(projected.workflow_runs)
    |> list.fold(dict.new(), fn(known, run_id) {
      dict.insert(known, run_id, True)
    })
  validate_append_records(records, known_runs)
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

fn fold_current_segment_streaming(
  current_path: String,
  snapshot_projection: projection.Projection,
) -> Result(projection.Projection, LedgerError) {
  let initial =
    JsonlFold(value: snapshot_projection, error: None, truncated_tail: False)
  case fold_lines(current_path, initial, projection_fold_step) {
    Ok(JsonlFold(value: folded, error: None, truncated_tail: _)) -> Ok(folded)
    Ok(JsonlFold(value: _, error: Some(error), truncated_tail: _)) ->
      Error(error)
    Error(OpenFailed("enoent")) -> Ok(snapshot_projection)
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

fn projection_fold_step(
  state: JsonlFold(projection.Projection),
  line: String,
  line_number: Int,
  is_last: Bool,
) -> JsonlFold(projection.Projection) {
  case state.error {
    Some(_) -> state
    None ->
      case parse_jsonl_line(line, line_number, is_last) {
        Ok(ParsedRecord(ledger_record)) ->
          JsonlFold(
            value: projection.apply(state.value, ledger_record),
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

fn read_snapshot(
  ledger_path: LedgerPath,
) -> Result(projection.Projection, LedgerError) {
  case simplifile.read(ledger_path.snapshot_path) {
    Ok(contents) ->
      case projection.decode_string(contents) {
        Ok(snapshot_projection) -> Ok(snapshot_projection)
        Error(projection.UnsupportedSnapshotVersion(version)) ->
          Error(UnsupportedVersion(version))
        Error(error) ->
          Error(CorruptRecord(0, projection.describe_decode_error(error)))
      }
    Error(simplifile.Enoent) -> Ok(projection.new())
    Error(error) -> Error(Io(file_error("read ledger snapshot", error)))
  }
}

fn write_snapshot_atomically(
  ledger_path: LedgerPath,
  snapshot_projection: projection.Projection,
) -> Result(Nil, LedgerError) {
  let temp_path = ledger_path.snapshot_path <> ".tmp"
  case
    simplifile.write(
      temp_path,
      projection.to_string(snapshot_projection) <> "\n",
    )
  {
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
  snapshot_projection: projection.Projection,
) -> Result(Nil, LedgerError) {
  write_snapshot_atomically(ledger_path, snapshot_projection)
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
