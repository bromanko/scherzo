import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/session/event
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

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
    #("display_name", json.string(summary.display_name)),
    #("issue_id", json.string(summary.issue_id)),
    #("issue_identifier", json.string(summary.issue_identifier)),
    #("issue_title", json.string(summary.issue_title)),
    #("workspace_path", json.string(summary.workspace_path)),
    #("pi_session_id", optional_string(summary.pi_session_id)),
    #("status", json.string(event.status_to_string(summary.status))),
    #("exit_reason", optional_exit_reason(event.exit_reason(summary.status))),
    #("recovery", optional_recovery(summary.recovery)),
    #("current_turn", json.int(summary.current_turn)),
    #("current_turn_status", optional_turn_status(summary.current_turn_status)),
    #(
      "current_turn_started_at_ms",
      optional_int(summary.current_turn_started_at_ms),
    ),
    #(
      "last_turn_finished_at_ms",
      optional_int(summary.last_turn_finished_at_ms),
    ),
    #("last_turn_duration_ms", optional_int(summary.last_turn_duration_ms)),
    #("last_turn_token_delta", tokens_to_json(summary.last_turn_token_delta)),
    #("last_turn_reason", optional_turn_reason(summary.last_turn_reason)),
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
  let payload = sanitize_turn_payload_for_json(payload)
  [
    #("kind", json.string(event.kind_to_string(payload.kind))),
    #("name", json.string(event.name_to_string(payload.name))),
    #("turn", optional_int(payload.turn)),
    #("pi_type", optional_string(payload.pi_type)),
    #("message", optional_string(payload.message)),
    #("recovery", optional_recovery(payload.recovery)),
    #("request_id", optional_string(payload.request_id)),
    #("method", optional_string(payload.method)),
    #("tool_name", optional_string(payload.tool_name)),
    #("tool_input", optional_string(payload.tool_input)),
    #("tool_output", optional_string(payload.tool_output)),
    #("tool_status", optional_string(payload.tool_status)),
    #("tokens", tokens_to_json(payload.tokens)),
    #("turn_status", optional_turn_status(payload.turn_status)),
    #("turn_started_at_ms", optional_int(payload.turn_started_at_ms)),
    #("turn_finished_at_ms", optional_int(payload.turn_finished_at_ms)),
    #("turn_duration_ms", optional_int(payload.turn_duration_ms)),
    #("token_delta", tokens_to_json(payload.token_delta)),
    #("reason", optional_turn_reason(payload.reason)),
    #("raw_json", optional_raw_json(payload.raw_json)),
  ]
}

fn sanitize_turn_payload_for_json(
  payload: event.EventPayload,
) -> event.EventPayload {
  case payload.kind {
    event.Turn ->
      event.EventPayload(
        ..payload,
        pi_type: None,
        message: None,
        request_id: None,
        method: None,
        tool_name: None,
        tool_input: None,
        tool_output: None,
        tool_status: None,
        raw_json: None,
      )
    _ -> payload
  }
}

fn tokens_to_json(tokens: session_tokens.TokenTotals) -> json.Json {
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

pub fn recovery_to_json(recovery: event.RecoveryInfo) -> json.Json {
  json.object([
    #("status", json.string(event.recovery_status_to_string(recovery.status))),
    #("source", json.string(recovery.source)),
    #("message", optional_string(recovery.message)),
    #(
      "safe_actions",
      json.array(recovery.safe_actions, of: fn(action) {
        json.string(event.recovery_action_to_string(action))
      }),
    ),
    #("workflow_run_id", optional_string(recovery.workflow_run_id)),
    #("workflow_step_id", optional_string(recovery.workflow_step_id)),
    #("current_pi_session_id", optional_string(recovery.current_pi_session_id)),
    #(
      "previous_pi_session_id",
      optional_string(recovery.previous_pi_session_id),
    ),
    #("park_reason", optional_string(recovery.park_reason)),
    #("park_release_policy", optional_string(recovery.park_release_policy)),
    #("parked_at_ms", optional_int(recovery.parked_at_ms)),
    #("drift_kind", optional_string(recovery.drift_kind)),
    #("retention_until_ms", optional_int(recovery.retention_until_ms)),
    #("cleanup_eligible_at_ms", optional_int(recovery.cleanup_eligible_at_ms)),
    #("cleanup_phase", optional_cleanup_phase(recovery.cleanup_phase)),
  ])
}

fn optional_recovery(value: Option(event.RecoveryInfo)) -> json.Json {
  case value {
    Some(recovery) -> recovery_to_json(recovery)
    None -> json.null()
  }
}

fn optional_exit_reason(value: Option(reason.WorkerExitReason)) -> json.Json {
  case value {
    Some(value) -> json.string(reason.to_string(value))
    None -> json.null()
  }
}

fn optional_turn_status(value: Option(turn_telemetry.TurnStatus)) -> json.Json {
  case value {
    Some(value) -> json.string(turn_telemetry.status_to_string(value))
    None -> json.null()
  }
}

fn optional_turn_reason(value: Option(turn_telemetry.TurnReason)) -> json.Json {
  case value {
    Some(value) -> json.string(turn_telemetry.reason_to_string(value))
    None -> json.null()
  }
}

fn optional_cleanup_phase(value: Option(event.CleanupPhase)) -> json.Json {
  case value {
    Some(value) -> json.string(event.cleanup_phase_to_string(value))
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
