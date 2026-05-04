import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/session/event
import scherzo/session/json as session_json
import scherzo/session/recovery as session_recovery
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

pub fn session_summary_serializes_exact_required_fields_test() {
  let summary =
    event.SessionSummary(
      session_id: "ABC-123-run-1",
      display_name: "ABC-123-fancy-narwhal",
      issue_id: "issue-1",
      issue_identifier: "ABC-123",
      issue_title: "Fix tests",
      workspace_path: "test/tmp/workspaces/ABC-123",
      pi_session_id: None,
      status: event.Preparing,
      recovery: None,
      current_turn: 0,
      current_turn_status: None,
      current_turn_started_at_ms: None,
      last_turn_finished_at_ms: None,
      last_turn_duration_ms: None,
      last_turn_token_delta: session_tokens.zero_token_totals(),
      last_turn_reason: None,
      started_at_ms: 10,
      last_event_at_ms: 10,
      token_totals: session_tokens.TokenTotals(
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
  assert decoded.display_name == "ABC-123-fancy-narwhal"
  assert decoded.issue_id == "issue-1"
  assert decoded.issue_identifier == "ABC-123"
  assert decoded.workspace_path == "test/tmp/workspaces/ABC-123"
  assert decoded.status == "preparing"
  assert decoded.current_turn == 0
  assert decoded.tokens_total == 10
}

pub fn recovery_status_strings_roundtrip_and_reserve_future_names_test() {
  assert event.recovery_status_to_string(event.Recovered) == "recovered"
  assert event.recovery_status_to_string(event.Interrupted) == "interrupted"
  assert event.recovery_status_to_string(event.Resumed) == "resumed"
  assert event.recovery_status_to_string(event.InspectionNeeded)
    == "inspection_needed"
  assert event.recovery_status_to_string(event.Blocked) == "blocked"
  assert event.recovery_status_to_string(event.Parked) == "parked"
  assert event.recovery_status_to_string(event.Cleanup) == "cleanup"
  assert event.recovery_status_to_string(event.DriftDetected)
    == "drift_detected"
  assert event.recovery_status_to_string(event.OldStateResetRequired)
    == "old_state_reset_required"
  assert event.recovery_status_from_string("interrupted")
    == Some(event.Interrupted)
  assert event.recovery_status_from_string("drift_detected")
    == Some(event.DriftDetected)
  assert event.recovery_status_from_string("unknown") == None
}

pub fn recovery_cleanup_phase_and_action_strings_roundtrip_test() {
  assert event.cleanup_phase_to_string(event.Retained) == "retained"
  assert event.cleanup_phase_to_string(event.Eligible) == "eligible"
  assert event.cleanup_phase_to_string(event.Deleting) == "deleting"
  assert event.cleanup_phase_to_string(event.Deleted) == "deleted"
  assert event.cleanup_phase_from_string("eligible") == Some(event.Eligible)
  assert event.cleanup_phase_from_string("wat") == None

  assert event.recovery_action_to_string(event.Inspect) == "inspect"
  assert event.recovery_action_to_string(event.ViewEvents) == "view_events"
  assert event.recovery_action_to_string(event.CleanupDryRunAction)
    == "cleanup_dry_run"
  assert event.recovery_action_to_string(event.ArchiveOldState)
    == "archive_old_state"
  assert event.recovery_action_from_string("cleanup_dry_run")
    == Some(event.CleanupDryRunAction)
  assert event.recovery_action_from_string("unknown") == None
}

pub fn lifecycle_recovery_event_names_roundtrip_test() {
  assert event.lifecycle_name_to_string(event.RecoveryDetected)
    == "recovery_detected"
  assert event.lifecycle_name_to_string(event.RecoveryInterrupted)
    == "recovery_interrupted"
  assert event.lifecycle_name_to_string(event.RecoveryParked)
    == "recovery_parked"
  assert event.lifecycle_name_to_string(event.RecoveryCleanup)
    == "recovery_cleanup"
  assert event.lifecycle_name_to_string(event.OldStateResetRequiredEvent)
    == "old_state_reset_required"
  assert event.lifecycle_name_to_string(event.CleanupDryRun)
    == "cleanup_dry_run"
  assert event.lifecycle_name_from_string("cleanup_completed")
    == Some(event.CleanupCompleted)
}

pub fn recovery_safe_text_redacts_and_bounds_messages_test() {
  let long = "secret-value " <> string.repeat("a", times: 250)
  let safe = session_recovery.recovery_safe_text(long, ["secret-value"])
  assert !string.contains(safe, "secret-value")
  assert string.contains(safe, "[REDACTED]")
  assert string.length(safe) <= 203
}

pub fn event_page_serializes_cursor_and_truncation_test() {
  let payload =
    event.empty_payload(
      event.Lifecycle,
      event.LifecycleName(event.WorkerStarted),
    )
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
          payload: event.EventPayload(
            ..payload,
            name: event.LifecycleName(event.WorkerExited),
          ),
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
      ..event.empty_payload(event.Pi, event.PiName(pi_event.MessageUpdate)),
      turn: Some(1),
      pi_type: Some("message_update"),
      message: Some("hello"),
    )

  let encoded = session_json.payload_to_string(payload)

  assert !string.contains(encoded, "cursor")
  assert !string.contains(encoded, "\"at_ms\":")
  assert string.contains(encoded, "message_update")
}

pub fn summary_json_includes_bounded_turn_fields_test() {
  let summary =
    event.SessionSummary(
      session_id: "session-1",
      display_name: "session-1",
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      issue_title: "Turn telemetry",
      workspace_path: "test/tmp/workspaces/ABC-1",
      pi_session_id: None,
      status: event.Running,
      recovery: None,
      current_turn: 2,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
      last_turn_finished_at_ms: None,
      last_turn_duration_ms: None,
      last_turn_token_delta: session_tokens.zero_token_totals(),
      last_turn_reason: None,
      started_at_ms: 900,
      last_event_at_ms: 1000,
      token_totals: session_tokens.zero_token_totals(),
    )

  let encoded = session_json.summary_to_string(summary)

  assert string.contains(encoded, "\"current_turn\":2")
  assert string.contains(encoded, "\"current_turn_status\":\"running\"")
  assert string.contains(encoded, "\"current_turn_started_at_ms\":1000")
  assert !string.contains(encoded, "raw_json")
}

pub fn turn_event_json_strips_sensitive_generic_fields_test() {
  let payload =
    event.EventPayload(
      ..event.empty_payload(
        event.Turn,
        event.TurnName(turn_telemetry.EventStarted),
      ),
      turn: Some(2),
      message: Some("SECRET_PROMPT"),
      tool_input: Some("tool_input_value"),
      tool_output: Some("full transcript"),
      tool_status: Some("secret status"),
      raw_json: Some(event.RedactedRawJson(
        value: "{\"secret\":true}",
        truncated: False,
      )),
    )

  let encoded = session_json.payload_to_string(payload)

  assert string.contains(encoded, "\"kind\":\"turn\"")
  assert string.contains(encoded, "\"name\":\"turn_started\"")
  assert string.contains(encoded, "\"turn\":2")
  assert !string.contains(encoded, "SECRET_PROMPT")
  assert !string.contains(encoded, "full transcript")
  assert !string.contains(encoded, "tool_input_value")
  assert !string.contains(encoded, "secret status")
  assert !string.contains(encoded, "{\"secret\":true}")
}

type SummaryJson {
  SummaryJson(
    session_id: String,
    display_name: String,
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
  use display_name <- decode.field("display_name", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use status <- decode.field("status", decode.string)
  use current_turn <- decode.field("current_turn", decode.int)
  use tokens_total <- decode.field("tokens", decode.at(["total"], decode.int))
  decode.success(SummaryJson(
    session_id: session_id,
    display_name: display_name,
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
