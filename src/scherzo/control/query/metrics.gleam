import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/daemon_identity
import scherzo/session/tokens as session_tokens

pub type RuntimeMetrics {
  RuntimeMetrics(
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
    aggregate_tokens: session_tokens.TokenTotals,
  )
}

pub fn empty_runtime_metrics() -> RuntimeMetrics {
  RuntimeMetrics(
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
    aggregate_tokens: session_tokens.zero_token_totals(),
  )
}

pub fn execute_status(
  ui_server_enabled ui_server_enabled: Bool,
  identity identity: daemon_identity.DaemonIdentity,
  get_dispatch_paused get_dispatch_paused: fn(Int) -> Result(Bool, Nil),
) -> Result(types.QueryResponse, types.QueryError) {
  case get_dispatch_paused(100) {
    Ok(dispatch_paused) ->
      Ok(
        types.StatusResponse(
          dto.status_from_source(
            types.StatusSource(
              daemon_id: identity.daemon_id,
              boot_id: identity.boot_id,
              dispatch_paused: dispatch_paused,
              ui_server_enabled: ui_server_enabled,
              supported_queries: types.supported_queries(),
              local_control_token: "",
              enrollment_token: "",
              tracker_payload: "",
              workflow_internals: [],
            ),
          ),
        ),
      )
    Error(Nil) -> timeout_error("daemon status query timed out")
  }
}

pub fn execute_metrics(
  ui_server_enabled ui_server_enabled: Bool,
  identity identity: daemon_identity.DaemonIdentity,
  sampled_at_ms sampled_at_ms: Int,
  get_dispatch_paused get_dispatch_paused: fn(Int) -> Result(Bool, Nil),
  get_runtime_metrics get_runtime_metrics: fn(Int) ->
    Result(RuntimeMetrics, Nil),
) -> Result(types.QueryResponse, types.QueryError) {
  case get_dispatch_paused(100), get_runtime_metrics(100) {
    Ok(dispatch_paused), Ok(runtime) ->
      Ok(
        types.MetricsResponse(from_runtime(
          daemon_id: identity.daemon_id,
          boot_id: identity.boot_id,
          sampled_at_ms: sampled_at_ms,
          dispatch_paused: dispatch_paused,
          ui_server_enabled: ui_server_enabled,
          runtime: runtime,
        )),
      )
    _, _ -> timeout_error("daemon metrics query timed out")
  }
}

pub fn from_runtime(
  daemon_id daemon_id: String,
  boot_id boot_id: String,
  sampled_at_ms sampled_at_ms: Int,
  dispatch_paused dispatch_paused: Bool,
  ui_server_enabled ui_server_enabled: Bool,
  runtime runtime: RuntimeMetrics,
) -> types.OperationalMetricsDto {
  types.OperationalMetricsSource(
    ..types.default_operational_metrics_source(
      daemon_id: daemon_id,
      boot_id: boot_id,
    ),
    sampled_at_ms: sampled_at_ms,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    remote_client_status: remote_client_status(ui_server_enabled),
    workflow_count: runtime.workflow_count,
    scheduled_job_count: runtime.scheduled_job_count,
    active_sessions: runtime.active_sessions,
    running_workers: runtime.running_workers,
    running_scheduled_workers: runtime.running_scheduled_workers,
    queued_claims: runtime.queued_claims,
    pending_dispatch_validations: runtime.pending_dispatch_validations,
    claimed_tasks: runtime.claimed_tasks,
    retry_tasks: runtime.retry_tasks,
    parked_tasks: runtime.parked_tasks,
    completed_tasks: runtime.completed_tasks,
    poll_generation: runtime.poll_generation,
    poll_in_flight: runtime.poll_in_flight,
    poll_timer_active: runtime.poll_timer_active,
    retry_timer_count: runtime.retry_timer_count,
    retry_refresh_in_flight_count: runtime.retry_refresh_in_flight_count,
    scheduled_due_count: runtime.scheduled_due_count,
    scheduled_pending_count: runtime.scheduled_pending_count,
    scheduled_retry_count: runtime.scheduled_retry_count,
    scheduled_report_retry_count: runtime.scheduled_report_retry_count,
    scheduled_retry_timer_count: runtime.scheduled_retry_timer_count,
    scheduled_report_retry_timer_count: runtime.scheduled_report_retry_timer_count,
    aggregate_tokens: runtime.aggregate_tokens,
  )
  |> dto.operational_metrics_from_source
}

fn timeout_error(
  message: String,
) -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryTimeout, message))
}

fn remote_client_status(ui_server_enabled: Bool) -> String {
  case ui_server_enabled {
    True -> "unknown"
    False -> "disabled"
  }
}
