import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/session/event
import scherzo/session/reason

pub fn summary_to_string(summary: event.SessionSummary) -> String {
  summary_to_json(summary) |> json.to_string
}

pub fn event_to_string(stored_event: event.SessionEvent) -> String {
  event_to_json(stored_event) |> json.to_string
}

pub fn payload_to_string(payload: event.EventPayload) -> String {
  payload_to_json(payload) |> json.to_string
}

pub fn page_to_string(page: event.EventPage) -> String {
  page_to_json(page) |> json.to_string
}

pub fn summary_to_json(summary: event.SessionSummary) -> json.Json {
  json.object([
    #("session_id", json.string(summary.session_id)),
    #("issue_id", json.string(summary.issue_id)),
    #("issue_identifier", json.string(summary.issue_identifier)),
    #("issue_title", json.string(summary.issue_title)),
    #("workspace_path", json.string(summary.workspace_path)),
    #("pi_session_id", optional_string(summary.pi_session_id)),
    #("status", json.string(event.status_to_string(summary.status))),
    #("exit_reason", optional_exit_reason(event.exit_reason(summary.status))),
    #("current_turn", json.int(summary.current_turn)),
    #("started_at_ms", json.int(summary.started_at_ms)),
    #("last_event_at_ms", json.int(summary.last_event_at_ms)),
    #("tokens", tokens_to_json(summary.token_totals)),
  ])
}

pub fn event_to_json(stored_event: event.SessionEvent) -> json.Json {
  json.object([
    #("cursor", json.int(stored_event.cursor)),
    #("at_ms", json.int(stored_event.at_ms)),
    #("session_id", json.string(stored_event.session_id)),
    #("issue_id", json.string(stored_event.issue_id)),
    ..payload_entries(stored_event.payload)
  ])
}

pub fn payload_to_json(payload: event.EventPayload) -> json.Json {
  json.object(payload_entries(payload))
}

pub fn page_to_json(page: event.EventPage) -> json.Json {
  json.object([
    #("events", json.array(page.events, of: event_to_json)),
    #("next_cursor", json.int(page.next_cursor)),
    #("truncated", json.bool(page.truncated)),
  ])
}

fn payload_entries(payload: event.EventPayload) -> List(#(String, json.Json)) {
  [
    #("kind", json.string(event.kind_to_string(payload.kind))),
    #("name", json.string(event.name_to_string(payload.name))),
    #("turn", optional_int(payload.turn)),
    #("pi_type", optional_string(payload.pi_type)),
    #("message", optional_string(payload.message)),
    #("request_id", optional_string(payload.request_id)),
    #("method", optional_string(payload.method)),
    #("tool_name", optional_string(payload.tool_name)),
    #("tool_input", optional_string(payload.tool_input)),
    #("tool_output", optional_string(payload.tool_output)),
    #("tool_status", optional_string(payload.tool_status)),
    #("tokens", tokens_to_json(payload.tokens)),
    #("raw_json", optional_raw_json(payload.raw_json)),
  ]
}

fn tokens_to_json(tokens: domain.TokenTotals) -> json.Json {
  json.object([
    #("input", json.int(tokens.input)),
    #("output", json.int(tokens.output)),
    #("cache_read", json.int(tokens.cache_read)),
    #("cache_write", json.int(tokens.cache_write)),
    #("total", json.int(tokens.total)),
  ])
}

fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn optional_exit_reason(value: Option(reason.WorkerExitReason)) -> json.Json {
  case value {
    Some(value) -> json.string(reason.to_string(value))
    None -> json.null()
  }
}

fn optional_int(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn optional_raw_json(value: Option(event.RedactedRawJson)) -> json.Json {
  case value {
    Some(event.RedactedRawJson(value: raw, truncated: truncated)) ->
      json.object([
        #("value", json.string(raw)),
        #("truncated", json.bool(truncated)),
      ])
    None -> json.null()
  }
}
