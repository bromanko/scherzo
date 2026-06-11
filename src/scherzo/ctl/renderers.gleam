import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command as control_command
import scherzo/control/query/types as query_types
import scherzo/ctl/workflow_recovery_history
import scherzo/session/event
import scherzo/session/reason as session_reason
import scherzo/turn_telemetry

const ps_session_width = 20

const ps_issue_width = 6

const ps_turn_width = 14

const ps_status_width = 11

const ps_recovery_width = 8

pub fn print_sessions_table(
  sessions: List(event.SessionSummary),
  now_ms: Int,
  line output_line: fn(String) -> Nil,
) -> Nil {
  output_line(ps_table_row(
    "SESSION",
    "ISSUE",
    "TURN",
    "STATUS",
    "RECOVERY",
    "LAST EVENT",
  ))
  list.each(sessions, fn(summary) {
    output_line(ps_table_row(
      ellipsize_middle(summary.display_name, ps_session_width),
      ellipsize_middle(summary.issue_identifier, ps_issue_width),
      ellipsize_middle(turn_summary_text(summary), ps_turn_width),
      ps_status_to_string(summary.status),
      ps_recovery_to_string(summary.recovery),
      format_last_event_age(now_ms, summary.last_event_at_ms),
    ))
  })
}

pub fn print_session(
  summary: event.SessionSummary,
  line output_line: fn(String) -> Nil,
) -> Nil {
  output_line("display_name: " <> summary.display_name)
  output_line("session_id: " <> summary.session_id)
  output_line(
    "issue: " <> summary.issue_identifier <> " " <> summary.issue_title,
  )
  output_line("status: " <> event.status_to_string(summary.status))
  output_line("turn: " <> turn_summary_text(summary))
  print_optional_int(
    "turn_started_at_ms",
    summary.current_turn_started_at_ms,
    output_line,
  )
  print_optional_int(
    "last_turn_finished_at_ms",
    summary.last_turn_finished_at_ms,
    output_line,
  )
  print_optional_int(
    "last_turn_duration_ms",
    summary.last_turn_duration_ms,
    output_line,
  )
  print_token_delta(summary, output_line)
  print_token_total(summary, output_line)
  print_optional_reason(summary.last_turn_reason, output_line)
  output_line("workspace: " <> summary.workspace_path)
  output_line("last_event_at_ms: " <> int.to_string(summary.last_event_at_ms))
  print_recovery_section(summary.recovery, output_line)
}

pub fn print_workflow_recovery_history(
  workspace_root: String,
  summary: event.SessionSummary,
  line output_line: fn(String) -> Nil,
) -> Nil {
  case workflow_recovery_history.load(workspace_root, summary) {
    Ok(history) ->
      workflow_recovery_history.render(history)
      |> list.each(output_line)
    Error(error) ->
      output_line(
        "workflow_step_recovery_history: unavailable ("
        <> workflow_recovery_history.describe_load_error(error)
        <> ")",
      )
  }
}

pub fn print_query_status(
  status: query_types.StatusDto,
  line output_line: fn(String) -> Nil,
) -> Nil {
  output_line("daemon_id: " <> status.daemon_id)
  output_line("boot_id: " <> status.boot_id)
  output_line("dispatch_paused: " <> bool_string(status.dispatch_paused))
  output_line("ui_server_enabled: " <> bool_string(status.ui_server_enabled))
  output_line(
    "supported_queries: " <> string.join(status.supported_queries, with: ", "),
  )
}

pub fn print_query_metrics(
  metrics: query_types.OperationalMetricsDto,
  line output_line: fn(String) -> Nil,
) -> Nil {
  output_line("daemon_id: " <> metrics.daemon_id)
  output_line("boot_id: " <> metrics.boot_id)
  print_int("schema_version", metrics.schema_version, output_line)
  print_int("sampled_at_ms", metrics.sampled_at_ms, output_line)
  output_line("dispatch_paused: " <> bool_string(metrics.dispatch_paused))
  output_line("ui_server_enabled: " <> bool_string(metrics.ui_server_enabled))
  output_line("remote_client_status: " <> metrics.remote_client_status)
  print_int("workflow_count", metrics.workflow_count, output_line)
  print_int("scheduled_job_count", metrics.scheduled_job_count, output_line)
  print_int("active_sessions", metrics.active_sessions, output_line)
  print_int("running_workers", metrics.running_workers, output_line)
  print_int(
    "running_scheduled_workers",
    metrics.running_scheduled_workers,
    output_line,
  )
  print_int("queued_claims", metrics.queued_claims, output_line)
  print_int(
    "pending_dispatch_validations",
    metrics.pending_dispatch_validations,
    output_line,
  )
  print_int(
    "pending_review_lane_preflights",
    metrics.pending_review_lane_preflights,
    output_line,
  )
  print_int("claimed_tasks", metrics.claimed_tasks, output_line)
  print_int("retry_tasks", metrics.retry_tasks, output_line)
  print_int("parked_tasks", metrics.parked_tasks, output_line)
  print_int("completed_tasks", metrics.completed_tasks, output_line)
  print_int("poll_generation", metrics.poll_generation, output_line)
  output_line("poll_in_flight: " <> bool_string(metrics.poll_in_flight))
  output_line("poll_timer_active: " <> bool_string(metrics.poll_timer_active))
  print_int("retry_timer_count", metrics.retry_timer_count, output_line)
  print_int(
    "retry_refresh_in_flight_count",
    metrics.retry_refresh_in_flight_count,
    output_line,
  )
  print_int("scheduled_due_count", metrics.scheduled_due_count, output_line)
  print_int(
    "scheduled_next_due_count",
    metrics.scheduled_next_due_count,
    output_line,
  )
  print_int(
    "scheduled_pending_count",
    metrics.scheduled_pending_count,
    output_line,
  )
  print_int("scheduled_retry_count", metrics.scheduled_retry_count, output_line)
  print_int(
    "scheduled_report_retry_count",
    metrics.scheduled_report_retry_count,
    output_line,
  )
  print_int(
    "scheduled_retry_timer_count",
    metrics.scheduled_retry_timer_count,
    output_line,
  )
  print_int(
    "scheduled_report_retry_timer_count",
    metrics.scheduled_report_retry_timer_count,
    output_line,
  )
  print_int("token_input", metrics.token_totals.input, output_line)
  print_int("token_output", metrics.token_totals.output, output_line)
  print_int("token_cache_read", metrics.token_totals.cache_read, output_line)
  print_int("token_cache_write", metrics.token_totals.cache_write, output_line)
  print_int("token_total", metrics.token_totals.total, output_line)
}

pub fn print_command_result(
  result: control_command.CommandResult,
  line output_line: fn(String) -> Nil,
) -> Nil {
  let target = case result.target {
    Some(target) -> " target=" <> target
    None -> ""
  }
  let reason = case control_command.status_reason(result.status) {
    Some(reason) -> " reason=" <> reason
    None -> ""
  }
  let message = case result.message {
    Some(message) -> " " <> message
    None -> ""
  }
  output_line(
    result.command
    <> " "
    <> control_command.status_to_string(result.status)
    <> target
    <> reason
    <> message,
  )
}

fn turn_summary_text(summary: event.SessionSummary) -> String {
  let base = "turn " <> int.to_string(summary.current_turn)
  let with_status = case summary.current_turn_status {
    Some(status) -> base <> " " <> turn_telemetry.status_to_string(status)
    None -> base
  }
  let with_duration = case summary.last_turn_duration_ms {
    Some(duration) -> with_status <> " " <> format_duration(duration)
    None -> with_status
  }
  case summary.last_turn_token_delta.total > 0 {
    True ->
      with_duration
      <> " +"
      <> int.to_string(summary.last_turn_token_delta.total)
      <> " tok"
    False -> with_duration
  }
}

fn format_duration(duration_ms: Int) -> String {
  case duration_ms < 1000 {
    True -> int.to_string(duration_ms) <> "ms"
    False -> {
      let tenths = duration_ms / 100
      let whole = tenths / 10
      let decimal = tenths - whole * 10
      int.to_string(whole) <> "." <> int.to_string(decimal) <> "s"
    }
  }
}

fn print_optional_int(
  label: String,
  value: Option(Int),
  output_line: fn(String) -> Nil,
) -> Nil {
  case value {
    Some(value) -> output_line(label <> ": " <> int.to_string(value))
    None -> Nil
  }
}

fn print_token_delta(
  summary: event.SessionSummary,
  output_line: fn(String) -> Nil,
) -> Nil {
  case summary.last_turn_token_delta.total > 0 {
    True ->
      output_line(
        "last_turn_token_delta: "
        <> int.to_string(summary.last_turn_token_delta.total),
      )
    False -> Nil
  }
}

fn print_token_total(
  summary: event.SessionSummary,
  output_line: fn(String) -> Nil,
) -> Nil {
  case summary.token_totals.total > 0 {
    True ->
      output_line("token_total: " <> int.to_string(summary.token_totals.total))
    False -> Nil
  }
}

fn print_optional_reason(
  reason: Option(turn_telemetry.TurnReason),
  output_line: fn(String) -> Nil,
) -> Nil {
  case reason {
    Some(reason) ->
      output_line(
        "last_turn_reason: " <> turn_telemetry.reason_to_string(reason),
      )
    None -> Nil
  }
}

fn ps_status_to_string(status: event.SessionStatus) -> String {
  case status {
    event.Exited(reason) -> ps_exit_reason_to_string(reason)
    _ -> event.status_to_string(status)
  }
}

fn ps_recovery_to_string(recovery: Option(event.RecoveryInfo)) -> String {
  case recovery {
    Some(recovery) -> event.recovery_status_to_string(recovery.status)
    None -> "-"
  }
}

fn ps_exit_reason_to_string(reason: session_reason.WorkerExitReason) -> String {
  case reason {
    session_reason.Normal -> "success"
    session_reason.Failed -> "failed"
    session_reason.WorkerDown -> "worker_down"
    session_reason.OperatorAbort -> "operator_abort"
    session_reason.OperatorStopAfterCurrentTurn -> "op_stop_after"
    session_reason.Stopped -> "stopped"
  }
}

fn ps_table_row(
  session_name: String,
  issue: String,
  turn: String,
  status: String,
  recovery: String,
  last_event: String,
) -> String {
  pad_right(session_name, ps_session_width)
  <> "  "
  <> pad_right(issue, ps_issue_width)
  <> "  "
  <> pad_left(turn, ps_turn_width)
  <> "  "
  <> pad_right(status, ps_status_width)
  <> "  "
  <> pad_right(recovery, ps_recovery_width)
  <> "  "
  <> last_event
}

fn format_last_event_age(now_ms: Int, event_ms: Int) -> String {
  let age_ms = case now_ms - event_ms < 0 {
    True -> 0
    False -> now_ms - event_ms
  }
  let seconds = age_ms / 1000
  case seconds < 60 {
    True -> int.to_string(seconds) <> "s ago"
    False -> {
      let minutes = seconds / 60
      case minutes < 60 {
        True -> int.to_string(minutes) <> "m ago"
        False -> {
          let hours = minutes / 60
          case hours < 24 {
            True -> int.to_string(hours) <> "h ago"
            False -> int.to_string(hours / 24) <> "d ago"
          }
        }
      }
    }
  }
}

fn ellipsize_middle(value: String, max_width: Int) -> String {
  case string.length(value) <= max_width {
    True -> value
    False ->
      case max_width <= 0 {
        True -> ""
        False ->
          case max_width == 1 {
            True -> "…"
            False -> {
              let available_width = max_width - 1
              let prefix_width = available_width / 2
              let suffix_width = available_width - prefix_width
              string.slice(value, 0, prefix_width)
              <> "…"
              <> string.slice(
                value,
                string.length(value) - suffix_width,
                suffix_width,
              )
            }
          }
      }
  }
}

fn pad_right(value: String, width: Int) -> String {
  let padding = width - string.length(value)
  case padding > 0 {
    True -> value <> string.repeat(" ", times: padding)
    False -> value
  }
}

fn pad_left(value: String, width: Int) -> String {
  let padding = width - string.length(value)
  case padding > 0 {
    True -> string.repeat(" ", times: padding) <> value
    False -> value
  }
}

fn print_recovery_section(
  recovery: Option(event.RecoveryInfo),
  output_line: fn(String) -> Nil,
) -> Nil {
  case recovery {
    None -> output_line("recovery: -")
    Some(recovery) -> {
      output_line("recovery:")
      output_line(
        "  status: " <> event.recovery_status_to_string(recovery.status),
      )
      output_line("  source: " <> recovery.source)
      case recovery.message {
        Some(message) -> output_line("  reason: " <> message)
        None -> Nil
      }
      let actions =
        recovery.safe_actions
        |> list.map(event.recovery_action_to_string)
        |> string.join(with: ", ")
      output_line("  safe_actions: " <> actions)
      print_optional(
        "  current_pi_session_id",
        recovery.current_pi_session_id,
        output_line,
      )
      print_optional("  workflow_run_id", recovery.workflow_run_id, output_line)
      print_optional(
        "  workflow_step_id",
        recovery.workflow_step_id,
        output_line,
      )
      print_optional(
        "  previous_pi_session_id",
        recovery.previous_pi_session_id,
        output_line,
      )
      print_optional("  park_reason", recovery.park_reason, output_line)
      print_optional(
        "  park_release_policy",
        recovery.park_release_policy,
        output_line,
      )
      print_optional_int("  parked_at_ms", recovery.parked_at_ms, output_line)
      print_optional("  drift_kind", recovery.drift_kind, output_line)
      print_optional_int(
        "  retention_until_ms",
        recovery.retention_until_ms,
        output_line,
      )
      print_optional_int(
        "  cleanup_eligible_at_ms",
        recovery.cleanup_eligible_at_ms,
        output_line,
      )
      case recovery.cleanup_phase {
        Some(phase) ->
          output_line(
            "  cleanup_phase: " <> event.cleanup_phase_to_string(phase),
          )
        None -> Nil
      }
    }
  }
}

fn print_optional(
  label: String,
  value: Option(String),
  output_line: fn(String) -> Nil,
) -> Nil {
  case value {
    Some(value) -> output_line(label <> ": " <> value)
    None -> Nil
  }
}

fn print_int(label: String, value: Int, output_line: fn(String) -> Nil) -> Nil {
  output_line(label <> ": " <> int.to_string(value))
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
