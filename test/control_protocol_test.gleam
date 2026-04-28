import gleam/option.{None}
import gleam/string
import scherzo/control/protocol
import scherzo/domain
import scherzo/session/event

pub fn decode_ping_request_requires_token_test() {
  let assert Ok(protocol.Ping("1", "secret")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"ping\",\"id\":\"1\",\"token\":\"secret\"}",
    )

  let assert Error(protocol.RequestError(id: "1", code: code, message: _)) =
    protocol.decode_request("{\"version\":1,\"type\":\"ping\",\"id\":\"1\"}")
  assert code == "invalid_request"
}

pub fn unknown_command_returns_stable_error_test() {
  let assert Error(err) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"delete_everything\",\"id\":\"9\",\"token\":\"secret\"}",
    )
  assert err.code == "unknown_command"

  let encoded =
    protocol.request_error_response(err) |> protocol.response_to_string
  assert string.contains(encoded, "unknown_command")
  assert string.contains(encoded, "\"ok\":false")
  assert string.contains(encoded, "\"id\":\"9\"")
}

pub fn decode_request_rejects_empty_session_id_test() {
  let assert Error(protocol.RequestError(id: "2", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_session\",\"id\":\"2\",\"token\":\"secret\",\"session_id\":\"\"}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "3", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_events\",\"id\":\"3\",\"token\":\"secret\",\"session_id\":\"\",\"after\":0,\"limit\":10}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "6", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"stream_events\",\"id\":\"6\",\"token\":\"secret\",\"session_id\":\"\",\"after\":0}",
    )
  assert code == "invalid_request"
}

pub fn decode_request_rejects_negative_after_test() {
  let assert Error(protocol.RequestError(id: "4", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_events\",\"id\":\"4\",\"token\":\"secret\",\"session_id\":\"session-1\",\"after\":-1,\"limit\":10}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "5", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"stream_events\",\"id\":\"5\",\"token\":\"secret\",\"session_id\":\"session-1\",\"after\":-1}",
    )
  assert code == "invalid_request"
}

pub fn encode_events_response_contains_cursor_and_session_test() {
  let page =
    event.EventPage(
      events: [
        event.SessionEvent(
          cursor: 7,
          at_ms: 100,
          session_id: "session-1",
          issue_id: "issue-1",
          payload: event.EventPayload(
            kind: event.Lifecycle,
            name: "worker_started",
            turn: None,
            pi_type: None,
            message: None,
            request_id: None,
            method: None,
            tool_name: None,
            tokens: domain.zero_token_totals(),
            raw_json: None,
          ),
        ),
      ],
      next_cursor: 7,
      truncated: False,
    )

  let encoded =
    protocol.success_response("events-1", protocol.event_page_data(page))
    |> protocol.response_to_string

  assert string.contains(encoded, "\"id\":\"events-1\"")
  assert string.contains(encoded, "\"session_id\":\"session-1\"")
  assert string.contains(encoded, "\"cursor\":7")
  assert string.contains(encoded, "\"next_cursor\":7")
  assert string.contains(encoded, "\"truncated\":false")
  assert string.contains(encoded, "\"ok\":true")

  let assert Ok(decoded) = protocol.decode_get_events_response(encoded)
  assert decoded.next_cursor == 7
  assert decoded.truncated == False
}
