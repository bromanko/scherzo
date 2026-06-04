import gleam/option.{type Option, None, Some}
import scherzo/control/query/types as query_types
import scherzo/session/tokens as session_tokens

pub type RemoteClientStatus {
  Disabled
  Starting
  Connected
  Retrying(error_code: String)
  Stopped
}

pub type RuntimeCounts {
  RuntimeCounts(
    workflow_count: Int,
    scheduled_job_count: Int,
    active_sessions: Int,
    running_workers: Int,
    running_scheduled_workers: Int,
    queued_claims: Int,
    pending_dispatch_validations: Int,
    claimed_tasks: Int,
    retry_tasks: Int,
    parked_tasks: Int,
    completed_tasks: Int,
    poll_generation: Int,
    poll_in_flight: Bool,
    poll_timer_active: Bool,
    retry_timer_count: Int,
    retry_refresh_in_flight_count: Int,
    scheduled_due_count: Int,
    scheduled_pending_count: Int,
    scheduled_retry_count: Int,
    scheduled_report_retry_count: Int,
    scheduled_retry_timer_count: Int,
    scheduled_report_retry_timer_count: Int,
  )
}

pub type Snapshot {
  Snapshot(
    daemon_id: String,
    boot_id: String,
    sampled_at_ms: Int,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    remote_client_status: RemoteClientStatus,
    counts: RuntimeCounts,
    token_totals: session_tokens.TokenTotals,
    remote_client_error_code: Option(String),
  )
}

pub opaque type ReadModel {
  ReadModel(
    daemon_id: String,
    boot_id: String,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    remote_client_status: RemoteClientStatus,
    counts: RuntimeCounts,
    token_totals: session_tokens.TokenTotals,
  )
}

pub fn new(
  daemon_id daemon_id: String,
  boot_id boot_id: String,
  ui_server_enabled ui_server_enabled: Bool,
) -> ReadModel {
  ReadModel(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: False,
    ui_server_enabled: ui_server_enabled,
    remote_client_status: case ui_server_enabled {
      True -> Starting
      False -> Disabled
    },
    counts: empty_runtime_counts(),
    token_totals: session_tokens.zero_token_totals(),
  )
}

pub fn empty_runtime_counts() -> RuntimeCounts {
  RuntimeCounts(
    workflow_count: 0,
    scheduled_job_count: 0,
    active_sessions: 0,
    running_workers: 0,
    running_scheduled_workers: 0,
    queued_claims: 0,
    pending_dispatch_validations: 0,
    claimed_tasks: 0,
    retry_tasks: 0,
    parked_tasks: 0,
    completed_tasks: 0,
    poll_generation: 0,
    poll_in_flight: False,
    poll_timer_active: False,
    retry_timer_count: 0,
    retry_refresh_in_flight_count: 0,
    scheduled_due_count: 0,
    scheduled_pending_count: 0,
    scheduled_retry_count: 0,
    scheduled_report_retry_count: 0,
    scheduled_retry_timer_count: 0,
    scheduled_report_retry_timer_count: 0,
  )
}

pub fn update_counts(
  read_model: ReadModel,
  counts counts: RuntimeCounts,
) -> ReadModel {
  ReadModel(..read_model, counts: counts)
}

pub fn update_dispatch_paused(
  read_model: ReadModel,
  dispatch_paused dispatch_paused: Bool,
) -> ReadModel {
  ReadModel(..read_model, dispatch_paused: dispatch_paused)
}

pub fn update_remote_client_status(
  read_model: ReadModel,
  status status: RemoteClientStatus,
) -> ReadModel {
  ReadModel(..read_model, remote_client_status: status)
}

pub fn update_token_totals(
  read_model: ReadModel,
  token_totals token_totals: session_tokens.TokenTotals,
) -> ReadModel {
  ReadModel(..read_model, token_totals: token_totals)
}

pub fn snapshot(
  read_model: ReadModel,
  sampled_at_ms sampled_at_ms: Int,
) -> Snapshot {
  Snapshot(
    daemon_id: read_model.daemon_id,
    boot_id: read_model.boot_id,
    sampled_at_ms: sampled_at_ms,
    dispatch_paused: read_model.dispatch_paused,
    ui_server_enabled: read_model.ui_server_enabled,
    remote_client_status: read_model.remote_client_status,
    counts: read_model.counts,
    token_totals: read_model.token_totals,
    remote_client_error_code: remote_client_error_code(
      read_model.remote_client_status,
    ),
  )
}

pub fn status_source(snapshot: Snapshot) -> query_types.StatusSource {
  query_types.StatusSource(
    ..query_types.default_status_source(
      daemon_id: snapshot.daemon_id,
      boot_id: snapshot.boot_id,
    ),
    dispatch_paused: snapshot.dispatch_paused,
    ui_server_enabled: snapshot.ui_server_enabled,
  )
}

pub fn metrics_source(
  snapshot: Snapshot,
) -> query_types.OperationalMetricsSource {
  let RuntimeCounts(
    workflow_count: workflow_count,
    scheduled_job_count: scheduled_job_count,
    active_sessions: active_sessions,
    running_workers: running_workers,
    running_scheduled_workers: running_scheduled_workers,
    queued_claims: queued_claims,
    pending_dispatch_validations: pending_dispatch_validations,
    claimed_tasks: claimed_tasks,
    retry_tasks: retry_tasks,
    parked_tasks: parked_tasks,
    completed_tasks: completed_tasks,
    poll_generation: poll_generation,
    poll_in_flight: poll_in_flight,
    poll_timer_active: poll_timer_active,
    retry_timer_count: retry_timer_count,
    retry_refresh_in_flight_count: retry_refresh_in_flight_count,
    scheduled_due_count: scheduled_due_count,
    scheduled_pending_count: scheduled_pending_count,
    scheduled_retry_count: scheduled_retry_count,
    scheduled_report_retry_count: scheduled_report_retry_count,
    scheduled_retry_timer_count: scheduled_retry_timer_count,
    scheduled_report_retry_timer_count: scheduled_report_retry_timer_count,
  ) = snapshot.counts

  query_types.OperationalMetricsSource(
    ..query_types.default_operational_metrics_source(
      daemon_id: snapshot.daemon_id,
      boot_id: snapshot.boot_id,
    ),
    sampled_at_ms: snapshot.sampled_at_ms,
    dispatch_paused: snapshot.dispatch_paused,
    ui_server_enabled: snapshot.ui_server_enabled,
    remote_client_status: remote_client_status_label(
      snapshot.remote_client_status,
    ),
    workflow_count: workflow_count,
    scheduled_job_count: scheduled_job_count,
    active_sessions: active_sessions,
    running_workers: running_workers,
    running_scheduled_workers: running_scheduled_workers,
    queued_claims: queued_claims,
    pending_dispatch_validations: pending_dispatch_validations,
    claimed_tasks: claimed_tasks,
    retry_tasks: retry_tasks,
    parked_tasks: parked_tasks,
    completed_tasks: completed_tasks,
    poll_generation: poll_generation,
    poll_in_flight: poll_in_flight,
    poll_timer_active: poll_timer_active,
    retry_timer_count: retry_timer_count,
    retry_refresh_in_flight_count: retry_refresh_in_flight_count,
    scheduled_due_count: scheduled_due_count,
    scheduled_pending_count: scheduled_pending_count,
    scheduled_retry_count: scheduled_retry_count,
    scheduled_report_retry_count: scheduled_report_retry_count,
    scheduled_retry_timer_count: scheduled_retry_timer_count,
    scheduled_report_retry_timer_count: scheduled_report_retry_timer_count,
    aggregate_tokens: snapshot.token_totals,
  )
}

pub fn remote_client_status_label(status: RemoteClientStatus) -> String {
  case status {
    Disabled -> "disabled"
    Starting -> "starting"
    Connected -> "connected"
    Retrying(_) -> "retrying"
    Stopped -> "stopped"
  }
}

fn remote_client_error_code(status: RemoteClientStatus) -> Option(String) {
  case status {
    Retrying(error_code) -> Some(error_code)
    Disabled | Starting | Connected | Stopped -> None
  }
}
