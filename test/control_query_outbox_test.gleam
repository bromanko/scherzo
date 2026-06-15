import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/query/codec
import scherzo/control/query/outbox
import scherzo/control/query/types
import scherzo/state/projection
import scherzo/state/record
import test_async

fn entries() -> List(#(String, projection.OutboxStatus)) {
  [
    #(
      "outbox-pending",
      projection.OutboxPendingV2WithTask(
        record.linear_task_ref_fields(
          "issue-pending",
          Some("LIV-1"),
          Some("https://linear.example/LIV-1"),
        ),
        "linear_comment",
        "dedupe-pending",
        "{\"type\":\"linear_comment\",\"body\":\"raw-secret\"}",
        1000,
      ),
    ),
    #(
      "outbox-in-flight",
      projection.OutboxAttempted(
        "issue-in-flight",
        "linear_comment",
        "dedupe-in-flight",
        "{\"type\":\"linear_comment\",\"body\":\"raw-secret\"}",
        1,
        1100,
      ),
    ),
    #(
      "outbox-retryable",
      projection.OutboxRetryScheduled(
        "issue-retryable",
        "linear_comment",
        "dedupe-retryable",
        "{\"type\":\"linear_comment\",\"body\":\"raw-secret\"}",
        "rate_limited",
        2,
        2000,
        1200,
      ),
    ),
    #(
      "outbox-completed",
      projection.OutboxCompleted("issue-completed", "linear_comment", 1300),
    ),
    #(
      "outbox-permanent",
      projection.OutboxPermanentlyFailed(
        "issue-permanent",
        "linear_comment",
        "invalid_payload",
        3,
        1400,
      ),
    ),
  ]
}

pub fn outbox_list_filters_paginates_and_omits_payload_bodies_test() {
  let assert Ok(types.OutboxListResponse(page)) =
    outbox.execute_list(
      get_outbox: fn(_) { Ok(entries()) },
      query: types.OutboxListQuery(
        statuses: [types.OutboxRetryableStatus, types.OutboxPermanentStatus],
        kinds: ["linear_comment"],
        limit: 1,
        cursor: None,
      ),
    )

  let assert [first] = page.items
  assert first.outbox_id == "outbox-permanent"
  assert first.status == types.OutboxPermanentStatus
  assert first.task_ref.id == "issue-permanent"
  assert first.last_error_code == Some("invalid_payload")
  assert page.page.has_more
  let assert Some(next_cursor) = page.page.next_cursor
  assert next_cursor == "cursor:1"

  let assert Ok(types.OutboxListResponse(second_page)) =
    outbox.execute_list(
      get_outbox: fn(_) { Ok(entries()) },
      query: types.OutboxListQuery(
        statuses: [types.OutboxRetryableStatus, types.OutboxPermanentStatus],
        kinds: ["linear_comment"],
        limit: 1,
        cursor: Some(next_cursor),
      ),
    )
  let assert [second] = second_page.items
  assert second.outbox_id == "outbox-retryable"
  assert second.next_attempt_at_ms == Some(2000)
  assert second.has_payload
  assert second_page.page.has_more == False

  let encoded = codec.response_to_string(types.OutboxListResponse(second_page))
  assert string.contains(encoded, "\"has_payload\":true")
  assert !string.contains(encoded, "payload_json")
  assert !string.contains(encoded, "raw-secret")
}

pub fn outbox_show_returns_single_record_and_not_found_test() {
  let assert Ok(types.OutboxShowResponse(record)) =
    outbox.execute_show(
      get_outbox: fn(_) { Ok(entries()) },
      query: types.OutboxShowQuery(outbox_id: "outbox-retryable"),
    )

  assert record.outbox_id == "outbox-retryable"
  assert record.status == types.OutboxRetryableStatus
  assert record.dedupe_key == Some("dedupe-retryable")
  assert record.attempt_count == Some(2)
  assert record.last_error_code == Some("rate_limited")

  let assert Error(types.QueryError(code: code, message: message)) =
    outbox.execute_show(
      get_outbox: fn(_) { Ok(entries()) },
      query: types.OutboxShowQuery(outbox_id: "missing"),
    )
  assert code == types.QueryNotFound
  assert message == "outbox record not found"
}

pub fn outbox_list_invalid_cursor_is_validated_before_snapshot_test() {
  let calls = process.new_subject()
  let assert Error(types.QueryError(code: code, message: message)) =
    outbox.execute_list(
      get_outbox: fn(_) {
        process.send(calls, Nil)
        Error(Nil)
      },
      query: types.OutboxListQuery(
        statuses: [],
        kinds: [],
        limit: 50,
        cursor: Some("not-a-cursor"),
      ),
    )

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
  test_async.assert_no_extra_message(calls)
}

pub fn outbox_query_times_out_when_snapshot_unavailable_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    outbox.execute_list(
      get_outbox: fn(_) { Error(Nil) },
      query: types.default_outbox_list_query(),
    )

  assert code == types.QueryTimeout
  assert message == "daemon outbox query timed out"
}
