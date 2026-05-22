import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/log
import scherzo/session/event
import scherzo/state/projection
import scherzo/state/recovery as state_recovery

pub const max_recovery_text_chars = 200

pub fn recovery_safe_text(value: String, secrets: List(String)) -> String {
  log.redact("recovery_message", value, secrets)
  |> log.truncate(max_recovery_text_chars)
}

pub fn safe_actions_for_status(
  status: event.RecoveryStatus,
) -> List(event.RecoveryAction) {
  case status {
    event.Recovered -> [event.Inspect, event.ViewEvents]
    event.Interrupted -> [
      event.Inspect,
      event.ViewEvents,
      event.Retry,
      event.Park,
    ]
    event.Resumed -> [event.Inspect, event.ViewEvents]
    event.InspectionNeeded -> [
      event.Inspect,
      event.ViewEvents,
      event.Retry,
      event.Park,
    ]
    event.Blocked -> [event.Inspect, event.ViewEvents, event.Park]
    event.Parked -> [event.Inspect, event.ViewEvents, event.Unpark]
    event.Cleanup -> [event.Inspect, event.CleanupDryRunAction]
    event.DriftDetected -> [
      event.Inspect,
      event.ViewEvents,
      event.Retry,
      event.Park,
    ]
    event.OldStateResetRequired -> [
      event.Inspect,
      event.ArchiveOldState,
      event.DiscardOldState,
      event.ReinitializeState,
    ]
  }
}

pub fn base_info(
  status: event.RecoveryStatus,
  source: String,
  message: Option(String),
  secrets: List(String),
) -> event.RecoveryInfo {
  event.RecoveryInfo(
    status: status,
    source: source,
    message: sanitize_message(message, secrets),
    safe_actions: safe_actions_for_status(status),
    workflow_run_id: None,
    workflow_step_id: None,
    current_pi_session_id: None,
    previous_pi_session_id: None,
    park_reason: None,
    park_release_policy: None,
    parked_at_ms: None,
    drift_kind: None,
    retention_until_ms: None,
    cleanup_eligible_at_ms: None,
    cleanup_phase: None,
  )
}

pub fn recovered(
  source: String,
  message: Option(String),
) -> event.RecoveryInfo {
  base_info(event.Recovered, source, message, [])
}

pub fn interrupted_run(
  run_id: String,
  status: projection.RunStatus,
  current_pi_session_id: Option(String),
) -> Option(event.RecoveryInfo) {
  case status {
    projection.RunRunning(_, _, _, _) ->
      Some(
        event.RecoveryInfo(
          ..base_info(
            event.Interrupted,
            "projection.run_running",
            Some("daemon_restart"),
            [],
          ),
          workflow_run_id: Some(run_id),
          current_pi_session_id: current_pi_session_id,
        ),
      )
    projection.RunInterrupted(_, reason, _) ->
      Some(
        event.RecoveryInfo(
          ..base_info(
            event.Interrupted,
            "projection.run_interrupted",
            Some(reason),
            [],
          ),
          workflow_run_id: Some(run_id),
          current_pi_session_id: current_pi_session_id,
        ),
      )
    projection.RunFinished(..) -> None
  }
}

pub fn parked_issue(parked: projection.ParkedIssue) -> event.RecoveryInfo {
  event.RecoveryInfo(
    ..base_info(
      event.Parked,
      "projection.parked_issue",
      Some(parked.reason),
      [],
    ),
    park_reason: Some(recovery_safe_text(parked.reason, [])),
    park_release_policy: Some(parked.release_policy),
    parked_at_ms: Some(parked.parked_at_ms),
    drift_kind: drift_kind_from_park_reason(parked.reason),
  )
}

fn drift_kind_from_park_reason(reason: String) -> Option(String) {
  case string.starts_with(reason, "workflow_definition_drift") {
    True -> Some("workflow_definition")
    False ->
      case string.starts_with(reason, "issue_content_drift") {
        True -> Some("issue_content")
        False ->
          case string.starts_with(reason, "issue_state_drift") {
            True -> Some("issue_state")
            False -> None
          }
      }
  }
}

pub fn cleanup_request(
  request: state_recovery.CleanupRequest,
) -> event.RecoveryInfo {
  let state_recovery.CleanupRequest(
    issue_id: _,
    issue_identifier: _,
    workspace_path: workspace_path,
  ) = request
  event.RecoveryInfo(
    ..base_info(
      event.Cleanup,
      "recovery.cleanup_request",
      Some("terminal interrupted run has workspace cleanup pending"),
      [],
    ),
    cleanup_phase: Some(event.Retained),
    park_reason: None,
    retention_until_ms: None,
    cleanup_eligible_at_ms: None,
    workflow_run_id: None,
    message: Some(
      recovery_safe_text("workspace cleanup pending: " <> workspace_path, []),
    ),
  )
}

pub fn cleanup_metadata(
  source: String,
  message: String,
  phase: event.CleanupPhase,
  retention_until_ms: Option(Int),
  cleanup_eligible_at_ms: Option(Int),
) -> event.RecoveryInfo {
  event.RecoveryInfo(
    ..base_info(event.Cleanup, source, Some(message), []),
    cleanup_phase: Some(phase),
    retention_until_ms: retention_until_ms,
    cleanup_eligible_at_ms: cleanup_eligible_at_ms,
  )
}

pub fn old_state_reset_required(message: String) -> event.RecoveryInfo {
  base_info(
    event.OldStateResetRequired,
    "ledger.unsupported_version",
    Some(message),
    [],
  )
}

fn sanitize_message(
  value: Option(String),
  secrets: List(String),
) -> Option(String) {
  case value {
    Some(value) -> Some(recovery_safe_text(value, secrets))
    None -> None
  }
}
