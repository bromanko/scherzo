import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import scherzo/domain
import scherzo/session/event
import scherzo/session/json as session_json

pub fn session_summary_serializes_exact_required_fields_test() {
  let summary =
    event.SessionSummary(
      session_id: "ABC-123-run-1",
      issue_id: "issue-1",
      issue_identifier: "ABC-123",
      issue_title: "Fix tests",
      workspace_path: "test/tmp/workspaces/ABC-123",
      pi_session_id: None,
      status: event.Preparing,
      current_turn: 0,
      started_at_ms: 10,
      last_event_at_ms: 10,
      token_totals: domain.TokenTotals(
        input: 1,
        output: 2,
        cache_read: 3,
        cache_write: 4,
        total: 10,
      ),
    )

  let assert Ok(decoded) =
    json.parse(session_json.summary_to_string(summary), summary_decoder())

  assert decoded.session_id == "ABC-123-run-1"
  assert decoded.issue_id == "issue-1"
  assert decoded.issue_identifier == "ABC-123"
  assert decoded.workspace_path == "test/tmp/workspaces/ABC-123"
  assert decoded.status == "preparing"
  assert decoded.current_turn == 0
  assert decoded.tokens_total == 10
}

pub fn event_page_serializes_cursor_and_truncation_test() {
  let payload = event.empty_payload(event.Lifecycle, "worker_started")
  let page =
    event.EventPage(
      events: [
        event.SessionEvent(
          cursor: 1,
          at_ms: 100,
          session_id: "session-1",
          issue_id: "issue-1",
          payload: payload,
        ),
        event.SessionEvent(
          cursor: 2,
          at_ms: 101,
          session_id: "session-1",
          issue_id: "issue-1",
          payload: event.EventPayload(..payload, name: "worker_exited"),
        ),
      ],
      next_cursor: 2,
      truncated: True,
    )

  let assert Ok(decoded) =
    json.parse(session_json.page_to_string(page), page_decoder())

  assert decoded.cursors == [1, 2]
  assert decoded.next_cursor == 2
  assert decoded.truncated == True
}

pub fn event_payload_has_no_cursor_or_timestamp_test() {
  let payload =
    event.EventPayload(
      kind: event.Pi,
      name: "message_update",
      turn: Some(1),
      pi_type: Some("message_update"),
      message: Some("hello"),
      request_id: None,
      method: None,
      tool_name: None,
      tool_input: None,
      tool_output: None,
      tool_status: None,
      tokens: domain.zero_token_totals(),
      raw_json: None,
    )

  let encoded = session_json.payload_to_string(payload)

  assert !string.contains(encoded, "cursor")
  assert !string.contains(encoded, "at_ms")
  assert string.contains(encoded, "message_update")
}

type SummaryJson {
  SummaryJson(
    session_id: String,
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
    status: String,
    current_turn: Int,
    tokens_total: Int,
  )
}

fn summary_decoder() -> decode.Decoder(SummaryJson) {
  use session_id <- decode.field("session_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use status <- decode.field("status", decode.string)
  use current_turn <- decode.field("current_turn", decode.int)
  use tokens_total <- decode.field("tokens", decode.at(["total"], decode.int))
  decode.success(SummaryJson(
    session_id: session_id,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    workspace_path: workspace_path,
    status: status,
    current_turn: current_turn,
    tokens_total: tokens_total,
  ))
}

type PageJson {
  PageJson(cursors: List(Int), next_cursor: Int, truncated: Bool)
}

fn page_decoder() -> decode.Decoder(PageJson) {
  use cursors <- decode.field(
    "events",
    decode.list(of: decode.at(["cursor"], decode.int)),
  )
  use next_cursor <- decode.field("next_cursor", decode.int)
  use truncated <- decode.field("truncated", decode.bool)
  decode.success(PageJson(
    cursors: cursors,
    next_cursor: next_cursor,
    truncated: truncated,
  ))
}
