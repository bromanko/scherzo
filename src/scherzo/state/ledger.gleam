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
}

pub type ReadRecordsResult {
  ReadRecordsResult(records: List(record.LedgerRecord), truncated_tail: Bool)
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
        None ->
          append_prepared(ledger_path.current_path, [ledger_record], fsync)
          |> result.map(fn(_) { LockedAppendAppended })
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
  use Nil <- result.try(ensure_layout(ledger_path))
  with_ledger_lock(ledger_path.ledger_dir, fn() { compact_locked(ledger_path) })
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

fn compact_locked(ledger_path: LedgerPath) -> Result(Nil, LedgerError) {
  use snapshot_projection <- result.try(read_snapshot(ledger_path))
  use compacted_projection <- result.try(fold_current_segment_streaming(
    ledger_path.current_path,
    snapshot_projection,
  ))
  use Nil <- result.try(write_snapshot_atomically(
    ledger_path,
    compacted_projection,
  ))
  archive_current_segment(ledger_path)
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

fn archive_current_segment(
  ledger_path: LedgerPath,
) -> Result(Nil, LedgerError) {
  case simplifile.file_info(ledger_path.current_path) {
    Error(simplifile.Enoent) ->
      simplifile.write(ledger_path.current_path, "")
      |> map_io("create empty current ledger")
    Error(error) ->
      Error(Io(file_error("inspect current ledger for archive", error)))
    Ok(info) ->
      case info.size == 0 {
        True ->
          simplifile.write(ledger_path.current_path, "")
          |> map_io("truncate current ledger")
        False -> {
          use archive_path <- result.try(next_archive_path(ledger_path))
          case simplifile.rename(ledger_path.current_path, archive_path) {
            Error(error) ->
              Error(Io(file_error("archive current ledger", error)))
            Ok(Nil) ->
              simplifile.write(ledger_path.current_path, "")
              |> map_io("create new current ledger")
          }
        }
      }
  }
}

fn next_archive_path(ledger_path: LedgerPath) -> Result(String, LedgerError) {
  case simplifile.read_directory(ledger_path.archive_dir) {
    Ok(entries) ->
      Ok(path.join(
        ledger_path.archive_dir,
        "segment-" <> int.to_string(list.length(entries) + 1) <> ".jsonl",
      ))
    Error(error) ->
      Error(Io(file_error("read ledger archive directory", error)))
  }
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
