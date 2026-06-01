import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import scherzo/control/query/types

pub fn status_from_source(source: types.StatusSource) -> types.StatusDto {
  let types.StatusSource(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
    ..,
  ) = source

  types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: case list.is_empty(supported_queries) {
      True -> types.supported_queries()
      False -> supported_queries
    },
  )
}

pub fn operational_metrics_from_source(
  source: types.OperationalMetricsSource,
) -> types.OperationalMetricsDto {
  let types.OperationalMetricsSource(
    daemon_id: daemon_id,
    boot_id: boot_id,
    sampled_at_ms: sampled_at_ms,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    remote_client_status: remote_client_status,
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
    aggregate_tokens: aggregate_tokens,
  ) = source

  types.OperationalMetricsDto(
    schema_version: types.operational_metrics_schema_version,
    daemon_id: daemon_id,
    boot_id: boot_id,
    sampled_at_ms: sampled_at_ms,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    remote_client_status: remote_client_status,
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
    token_totals: types.TokenTotalsDto(
      input: aggregate_tokens.input,
      output: aggregate_tokens.output,
      cache_read: aggregate_tokens.cache_read,
      cache_write: aggregate_tokens.cache_write,
      total: aggregate_tokens.total,
    ),
  )
}

pub fn status_to_json(status: types.StatusDto) -> json.Json {
  let types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
  ) = status

  json.object([
    #("daemon_id", json.string(daemon_id)),
    #("boot_id", json.string(boot_id)),
    #("dispatch_paused", json.bool(dispatch_paused)),
    #("ui_server_enabled", json.bool(ui_server_enabled)),
    #("supported_queries", json.array(supported_queries, of: json.string)),
  ])
}

pub fn operational_metrics_to_json(
  metrics: types.OperationalMetricsDto,
) -> json.Json {
  json.object([
    #("schema_version", json.int(metrics.schema_version)),
    #("daemon_id", json.string(metrics.daemon_id)),
    #("boot_id", json.string(metrics.boot_id)),
    #("sampled_at_ms", json.int(metrics.sampled_at_ms)),
    #("dispatch_paused", json.bool(metrics.dispatch_paused)),
    #("ui_server_enabled", json.bool(metrics.ui_server_enabled)),
    #("remote_client_status", json.string(metrics.remote_client_status)),
    #("workflow_count", json.int(metrics.workflow_count)),
    #("scheduled_job_count", json.int(metrics.scheduled_job_count)),
    #("active_sessions", json.int(metrics.active_sessions)),
    #("running_workers", json.int(metrics.running_workers)),
    #("running_scheduled_workers", json.int(metrics.running_scheduled_workers)),
    #("queued_claims", json.int(metrics.queued_claims)),
    #(
      "pending_dispatch_validations",
      json.int(metrics.pending_dispatch_validations),
    ),
    #("claimed_tasks", json.int(metrics.claimed_tasks)),
    #("retry_tasks", json.int(metrics.retry_tasks)),
    #("parked_tasks", json.int(metrics.parked_tasks)),
    #("completed_tasks", json.int(metrics.completed_tasks)),
    #("poll_generation", json.int(metrics.poll_generation)),
    #("poll_in_flight", json.bool(metrics.poll_in_flight)),
    #("poll_timer_active", json.bool(metrics.poll_timer_active)),
    #("retry_timer_count", json.int(metrics.retry_timer_count)),
    #(
      "retry_refresh_in_flight_count",
      json.int(metrics.retry_refresh_in_flight_count),
    ),
    #("scheduled_due_count", json.int(metrics.scheduled_due_count)),
    #("scheduled_pending_count", json.int(metrics.scheduled_pending_count)),
    #("scheduled_retry_count", json.int(metrics.scheduled_retry_count)),
    #(
      "scheduled_report_retry_count",
      json.int(metrics.scheduled_report_retry_count),
    ),
    #(
      "scheduled_retry_timer_count",
      json.int(metrics.scheduled_retry_timer_count),
    ),
    #(
      "scheduled_report_retry_timer_count",
      json.int(metrics.scheduled_report_retry_timer_count),
    ),
    #("token_totals", token_totals_to_json(metrics.token_totals)),
  ])
}

pub fn decode_status_dynamic(
  value: Dynamic,
) -> Result(types.StatusDto, types.QueryError) {
  case decode.run(value, status_decoder()) {
    Ok(status) -> Ok(status)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid status query payload",
      ))
  }
}

pub fn decode_operational_metrics_dynamic(
  value: Dynamic,
) -> Result(types.OperationalMetricsDto, types.QueryError) {
  case decode.run(value, operational_metrics_decoder()) {
    Ok(metrics) -> validate_operational_metrics_schema(metrics)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid metrics query payload",
      ))
  }
}

fn status_decoder() -> decode.Decoder(types.StatusDto) {
  use daemon_id <- decode.field("daemon_id", decode.string)
  use boot_id <- decode.field("boot_id", decode.string)
  use dispatch_paused <- decode.field("dispatch_paused", decode.bool)
  use ui_server_enabled <- decode.field("ui_server_enabled", decode.bool)
  use supported_queries <- decode.field(
    "supported_queries",
    decode.list(decode.string),
  )
  decode.success(types.StatusDto(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    supported_queries: supported_queries,
  ))
}

fn operational_metrics_decoder() -> decode.Decoder(types.OperationalMetricsDto) {
  use schema_version <- decode.field("schema_version", decode.int)
  use daemon_id <- decode.field("daemon_id", decode.string)
  use boot_id <- decode.field("boot_id", decode.string)
  use sampled_at_ms <- decode.field("sampled_at_ms", decode.int)
  use dispatch_paused <- decode.field("dispatch_paused", decode.bool)
  use ui_server_enabled <- decode.field("ui_server_enabled", decode.bool)
  use remote_client_status <- decode.field(
    "remote_client_status",
    decode.string,
  )
  use workflow_count <- decode.field("workflow_count", decode.int)
  use scheduled_job_count <- decode.field("scheduled_job_count", decode.int)
  use active_sessions <- decode.field("active_sessions", decode.int)
  use running_workers <- decode.field("running_workers", decode.int)
  use running_scheduled_workers <- decode.field(
    "running_scheduled_workers",
    decode.int,
  )
  use queued_claims <- decode.field("queued_claims", decode.int)
  use pending_dispatch_validations <- decode.field(
    "pending_dispatch_validations",
    decode.int,
  )
  use claimed_tasks <- decode.field("claimed_tasks", decode.int)
  use retry_tasks <- decode.field("retry_tasks", decode.int)
  use parked_tasks <- decode.field("parked_tasks", decode.int)
  use completed_tasks <- decode.field("completed_tasks", decode.int)
  use poll_generation <- decode.field("poll_generation", decode.int)
  use poll_in_flight <- decode.field("poll_in_flight", decode.bool)
  use poll_timer_active <- decode.field("poll_timer_active", decode.bool)
  use retry_timer_count <- decode.field("retry_timer_count", decode.int)
  use retry_refresh_in_flight_count <- decode.field(
    "retry_refresh_in_flight_count",
    decode.int,
  )
  use scheduled_due_count <- decode.field("scheduled_due_count", decode.int)
  use scheduled_pending_count <- decode.field(
    "scheduled_pending_count",
    decode.int,
  )
  use scheduled_retry_count <- decode.field("scheduled_retry_count", decode.int)
  use scheduled_report_retry_count <- decode.field(
    "scheduled_report_retry_count",
    decode.int,
  )
  use scheduled_retry_timer_count <- decode.field(
    "scheduled_retry_timer_count",
    decode.int,
  )
  use scheduled_report_retry_timer_count <- decode.field(
    "scheduled_report_retry_timer_count",
    decode.int,
  )
  use token_totals <- decode.field("token_totals", token_totals_decoder())
  decode.success(types.OperationalMetricsDto(
    schema_version: schema_version,
    daemon_id: daemon_id,
    boot_id: boot_id,
    sampled_at_ms: sampled_at_ms,
    dispatch_paused: dispatch_paused,
    ui_server_enabled: ui_server_enabled,
    remote_client_status: remote_client_status,
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
    token_totals: token_totals,
  ))
}

fn token_totals_decoder() -> decode.Decoder(types.TokenTotalsDto) {
  use input <- decode.field("input", decode.int)
  use output <- decode.field("output", decode.int)
  use cache_read <- decode.field("cache_read", decode.int)
  use cache_write <- decode.field("cache_write", decode.int)
  use total <- decode.field("total", decode.int)
  decode.success(types.TokenTotalsDto(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  ))
}

fn token_totals_to_json(tokens: types.TokenTotalsDto) -> json.Json {
  json.object([
    #("input", json.int(tokens.input)),
    #("output", json.int(tokens.output)),
    #("cache_read", json.int(tokens.cache_read)),
    #("cache_write", json.int(tokens.cache_write)),
    #("total", json.int(tokens.total)),
  ])
}

fn validate_operational_metrics_schema(
  metrics: types.OperationalMetricsDto,
) -> Result(types.OperationalMetricsDto, types.QueryError) {
  case metrics.schema_version == types.operational_metrics_schema_version {
    True -> Ok(metrics)
    False ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "unsupported metrics schema version",
      ))
  }
}
