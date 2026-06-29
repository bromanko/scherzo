import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string
import scherzo/control/query/cursor
import scherzo/control/query/types
import scherzo/state/projection
import scherzo/state/record

const outbox_snapshot_timeout_ms = 1000

pub fn execute_list(
  get_outbox get_outbox: fn(Int) ->
    Result(List(#(String, projection.OutboxStatus)), Nil),
  query query: types.OutboxListQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  use offset <- try_query(decode_cursor(query.cursor))
  case get_outbox(outbox_snapshot_timeout_ms) {
    Ok(entries) -> {
      let filtered =
        entries
        |> list.map(record_from_entry)
        |> list.filter(keeping: fn(record) { matches_query(record, query) })
        |> list.sort(by: compare_records)
      let limit = bounded_outbox_limit(query.limit)
      let page_items = filtered |> list.drop(offset) |> list.take(limit)
      let has_more = list.length(filtered) > offset + list.length(page_items)
      let next_cursor = case has_more {
        True -> Some(cursor.encode_offset(offset + list.length(page_items)))
        False -> None
      }
      Ok(
        types.OutboxListResponse(types.OutboxListDto(
          items: page_items,
          page: types.PageDto(next_cursor: next_cursor, has_more: has_more),
        )),
      )
    }
    Error(Nil) -> timeout_error("daemon outbox query timed out")
  }
}

pub fn execute_show(
  get_outbox get_outbox: fn(Int) ->
    Result(List(#(String, projection.OutboxStatus)), Nil),
  query query: types.OutboxShowQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  case get_outbox(outbox_snapshot_timeout_ms) {
    Ok(entries) ->
      case
        list.find(entries, fn(entry) {
          let #(outbox_id, _) = entry
          outbox_id == query.outbox_id
        })
      {
        Ok(entry) -> Ok(types.OutboxShowResponse(record_from_entry(entry)))
        Error(Nil) ->
          Error(types.QueryError(types.QueryNotFound, "outbox record not found"))
      }
    Error(Nil) -> timeout_error("daemon outbox query timed out")
  }
}

fn record_from_entry(
  entry: #(String, projection.OutboxStatus),
) -> types.OutboxRecordDto {
  let #(outbox_id, status) = entry
  case status {
    projection.OutboxPending(issue_id, outbox_kind, dedupe_key, pending_at_ms) ->
      types.OutboxRecordDto(
        outbox_id: outbox_id,
        kind: outbox_kind,
        status: types.OutboxPendingStatus,
        task_ref: issue_task_ref(issue_id),
        dedupe_key: Some(dedupe_key),
        attempt_count: None,
        next_attempt_at_ms: None,
        last_error_code: None,
        pending_at_ms: Some(pending_at_ms),
        attempted_at_ms: None,
        failed_at_ms: None,
        completed_at_ms: None,
        has_payload: False,
      )
    projection.OutboxPendingV2(
      issue_id,
      outbox_kind,
      dedupe_key,
      _,
      pending_at_ms,
    ) ->
      types.OutboxRecordDto(
        outbox_id: outbox_id,
        kind: outbox_kind,
        status: types.OutboxPendingStatus,
        task_ref: issue_task_ref(issue_id),
        dedupe_key: Some(dedupe_key),
        attempt_count: None,
        next_attempt_at_ms: None,
        last_error_code: None,
        pending_at_ms: Some(pending_at_ms),
        attempted_at_ms: None,
        failed_at_ms: None,
        completed_at_ms: None,
        has_payload: True,
      )
    projection.OutboxPendingV2WithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      _,
      pending_at_ms,
    ) ->
      types.OutboxRecordDto(
        outbox_id: outbox_id,
        kind: outbox_kind,
        status: types.OutboxPendingStatus,
        task_ref: task_ref_to_dto(task_ref),
        dedupe_key: Some(dedupe_key),
        attempt_count: None,
        next_attempt_at_ms: None,
        last_error_code: None,
        pending_at_ms: Some(pending_at_ms),
        attempted_at_ms: None,
        failed_at_ms: None,
        completed_at_ms: None,
        has_payload: True,
      )
    projection.OutboxAttempted(
      issue_id,
      outbox_kind,
      dedupe_key,
      _,
      attempt_count,
      attempted_at_ms,
    ) ->
      attempted_record(
        outbox_id,
        issue_task_ref(issue_id),
        outbox_kind,
        dedupe_key,
        attempt_count,
        attempted_at_ms,
      )
    projection.OutboxAttemptedWithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      _,
      attempt_count,
      attempted_at_ms,
    ) ->
      attempted_record(
        outbox_id,
        task_ref_to_dto(task_ref),
        outbox_kind,
        dedupe_key,
        attempt_count,
        attempted_at_ms,
      )
    projection.OutboxRetryScheduled(
      issue_id,
      outbox_kind,
      dedupe_key,
      _,
      error_code,
      attempt_count,
      next_attempt_at_ms,
      failed_at_ms,
    ) ->
      retryable_record(
        outbox_id,
        issue_task_ref(issue_id),
        outbox_kind,
        dedupe_key,
        error_code,
        attempt_count,
        next_attempt_at_ms,
        failed_at_ms,
      )
    projection.OutboxRetryScheduledWithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      _,
      error_code,
      attempt_count,
      next_attempt_at_ms,
      failed_at_ms,
    ) ->
      retryable_record(
        outbox_id,
        task_ref_to_dto(task_ref),
        outbox_kind,
        dedupe_key,
        error_code,
        attempt_count,
        next_attempt_at_ms,
        failed_at_ms,
      )
    projection.OutboxCompleted(issue_id, outbox_kind, completed_at_ms) ->
      completed_record(
        outbox_id,
        issue_task_ref(issue_id),
        outbox_kind,
        completed_at_ms,
      )
    projection.OutboxCompletedWithTask(task_ref, outbox_kind, completed_at_ms) ->
      completed_record(
        outbox_id,
        task_ref_to_dto(task_ref),
        outbox_kind,
        completed_at_ms,
      )
    projection.OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms) ->
      failed_record(
        outbox_id,
        issue_task_ref(issue_id),
        outbox_kind,
        error_code,
        failed_at_ms,
      )
    projection.OutboxFailedWithTask(
      task_ref,
      outbox_kind,
      error_code,
      failed_at_ms,
    ) ->
      failed_record(
        outbox_id,
        task_ref_to_dto(task_ref),
        outbox_kind,
        error_code,
        failed_at_ms,
      )
    projection.OutboxPermanentlyFailed(
      issue_id,
      outbox_kind,
      error_code,
      attempt_count,
      failed_at_ms,
    ) ->
      permanent_record(
        outbox_id,
        issue_task_ref(issue_id),
        outbox_kind,
        error_code,
        attempt_count,
        failed_at_ms,
      )
    projection.OutboxPermanentlyFailedWithTask(
      task_ref,
      outbox_kind,
      error_code,
      attempt_count,
      failed_at_ms,
    ) ->
      permanent_record(
        outbox_id,
        task_ref_to_dto(task_ref),
        outbox_kind,
        error_code,
        attempt_count,
        failed_at_ms,
      )
  }
}

fn attempted_record(
  outbox_id: String,
  task_ref: types.OutboxTaskRefDto,
  outbox_kind: String,
  dedupe_key: String,
  attempt_count: Int,
  attempted_at_ms: Int,
) -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: outbox_kind,
    status: types.OutboxInFlightStatus,
    task_ref: task_ref,
    dedupe_key: Some(dedupe_key),
    attempt_count: Some(attempt_count),
    next_attempt_at_ms: None,
    last_error_code: None,
    pending_at_ms: None,
    attempted_at_ms: Some(attempted_at_ms),
    failed_at_ms: None,
    completed_at_ms: None,
    has_payload: True,
  )
}

fn retryable_record(
  outbox_id: String,
  task_ref: types.OutboxTaskRefDto,
  outbox_kind: String,
  dedupe_key: String,
  error_code: String,
  attempt_count: Int,
  next_attempt_at_ms: Int,
  failed_at_ms: Int,
) -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: outbox_kind,
    status: types.OutboxRetryableStatus,
    task_ref: task_ref,
    dedupe_key: Some(dedupe_key),
    attempt_count: Some(attempt_count),
    next_attempt_at_ms: Some(next_attempt_at_ms),
    last_error_code: Some(error_code),
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: Some(failed_at_ms),
    completed_at_ms: None,
    has_payload: True,
  )
}

fn completed_record(
  outbox_id: String,
  task_ref: types.OutboxTaskRefDto,
  outbox_kind: String,
  completed_at_ms: Int,
) -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: outbox_kind,
    status: types.OutboxCompletedStatus,
    task_ref: task_ref,
    dedupe_key: None,
    attempt_count: None,
    next_attempt_at_ms: None,
    last_error_code: None,
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: None,
    completed_at_ms: Some(completed_at_ms),
    has_payload: False,
  )
}

fn failed_record(
  outbox_id: String,
  task_ref: types.OutboxTaskRefDto,
  outbox_kind: String,
  error_code: String,
  failed_at_ms: Int,
) -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: outbox_kind,
    status: types.OutboxFailedStatus,
    task_ref: task_ref,
    dedupe_key: None,
    attempt_count: None,
    next_attempt_at_ms: None,
    last_error_code: Some(error_code),
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: Some(failed_at_ms),
    completed_at_ms: None,
    has_payload: False,
  )
}

fn permanent_record(
  outbox_id: String,
  task_ref: types.OutboxTaskRefDto,
  outbox_kind: String,
  error_code: String,
  attempt_count: Int,
  failed_at_ms: Int,
) -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: outbox_kind,
    status: types.OutboxPermanentStatus,
    task_ref: task_ref,
    dedupe_key: None,
    attempt_count: Some(attempt_count),
    next_attempt_at_ms: None,
    last_error_code: Some(error_code),
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: Some(failed_at_ms),
    completed_at_ms: None,
    has_payload: False,
  )
}

fn issue_task_ref(issue_id: String) -> types.OutboxTaskRefDto {
  task_ref_to_dto(record.linear_task_ref_fields(issue_id, None, None))
}

fn task_ref_to_dto(task_ref: record.TaskRefFields) -> types.OutboxTaskRefDto {
  types.OutboxTaskRefDto(
    provider: task_ref.task_backend_kind,
    id: task_ref.task_remote_id,
    display_id: task_ref.task_key,
    url: task_ref.task_url,
  )
}

fn matches_query(
  record: types.OutboxRecordDto,
  query: types.OutboxListQuery,
) -> Bool {
  matches_status(record.status, query.statuses)
  && matches_kind(record.kind, query.kinds)
}

fn matches_status(
  status: types.OutboxRecordStatus,
  statuses: List(types.OutboxRecordStatus),
) -> Bool {
  case statuses {
    [] -> True
    _ -> list.contains(statuses, status)
  }
}

fn matches_kind(kind: String, kinds: List(String)) -> Bool {
  case kinds {
    [] -> True
    _ -> list.contains(kinds, kind)
  }
}

fn compare_records(
  left: types.OutboxRecordDto,
  right: types.OutboxRecordDto,
) -> order.Order {
  string.compare(left.outbox_id, right.outbox_id)
}

fn bounded_outbox_limit(limit: Int) -> Int {
  case limit < 1 {
    True -> 1
    False ->
      case limit > 100 {
        True -> 100
        False -> limit
      }
  }
}

fn decode_cursor(
  cursor_value: Option(String),
) -> Result(Int, types.QueryError) {
  case cursor_value {
    Some(cursor_value) -> cursor.decode_offset(cursor_value)
    None -> Ok(0)
  }
}

fn timeout_error(
  message: String,
) -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryTimeout, message))
}

fn try_query(
  result: Result(a, types.QueryError),
  next: fn(a) -> Result(b, types.QueryError),
) -> Result(b, types.QueryError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
