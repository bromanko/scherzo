import birl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/control/query/types
import scherzo/task

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
    pending_review_lane_preflights: pending_review_lane_preflights,
    claimed_tasks: claimed_tasks,
    retry_tasks: retry_tasks,
    parked_tasks: parked_tasks,
    completed_tasks: completed_tasks,
    pending_outbox_count: pending_outbox_count,
    in_flight_outbox_count: in_flight_outbox_count,
    retryable_outbox_count: retryable_outbox_count,
    permanent_outbox_count: permanent_outbox_count,
    poll_generation: poll_generation,
    poll_in_flight: poll_in_flight,
    poll_timer_active: poll_timer_active,
    retry_timer_count: retry_timer_count,
    retry_refresh_in_flight_count: retry_refresh_in_flight_count,
    lifecycle_projection_failed: lifecycle_projection_failed,
    scheduled_due_count: scheduled_due_count,
    scheduled_next_due_count: scheduled_next_due_count,
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
    pending_review_lane_preflights: pending_review_lane_preflights,
    claimed_tasks: claimed_tasks,
    retry_tasks: retry_tasks,
    parked_tasks: parked_tasks,
    completed_tasks: completed_tasks,
    pending_outbox_count: pending_outbox_count,
    in_flight_outbox_count: in_flight_outbox_count,
    retryable_outbox_count: retryable_outbox_count,
    permanent_outbox_count: permanent_outbox_count,
    poll_generation: poll_generation,
    poll_in_flight: poll_in_flight,
    poll_timer_active: poll_timer_active,
    retry_timer_count: retry_timer_count,
    retry_refresh_in_flight_count: retry_refresh_in_flight_count,
    lifecycle_projection_failed: lifecycle_projection_failed,
    scheduled_due_count: scheduled_due_count,
    scheduled_next_due_count: scheduled_next_due_count,
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
    #(
      "pending_review_lane_preflights",
      json.int(metrics.pending_review_lane_preflights),
    ),
    #("claimed_tasks", json.int(metrics.claimed_tasks)),
    #("retry_tasks", json.int(metrics.retry_tasks)),
    #("parked_tasks", json.int(metrics.parked_tasks)),
    #("completed_tasks", json.int(metrics.completed_tasks)),
    #("pending_outbox_count", json.int(metrics.pending_outbox_count)),
    #("in_flight_outbox_count", json.int(metrics.in_flight_outbox_count)),
    #("retryable_outbox_count", json.int(metrics.retryable_outbox_count)),
    #("permanent_outbox_count", json.int(metrics.permanent_outbox_count)),
    #("poll_generation", json.int(metrics.poll_generation)),
    #("poll_in_flight", json.bool(metrics.poll_in_flight)),
    #("poll_timer_active", json.bool(metrics.poll_timer_active)),
    #("retry_timer_count", json.int(metrics.retry_timer_count)),
    #(
      "retry_refresh_in_flight_count",
      json.int(metrics.retry_refresh_in_flight_count),
    ),
    #(
      "lifecycle_projection_failed",
      json.bool(metrics.lifecycle_projection_failed),
    ),
    #("scheduled_due_count", json.int(metrics.scheduled_due_count)),
    #("scheduled_next_due_count", json.int(metrics.scheduled_next_due_count)),
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

pub fn claim_list_to_json(claims: types.ClaimListDto) -> json.Json {
  json.object([
    #("sampled_at_ms", json.int(claims.sampled_at_ms)),
    #("items", json.array(claims.items, of: claim_to_json)),
  ])
}

pub fn decode_claim_list_dynamic(
  value: Dynamic,
) -> Result(types.ClaimListDto, types.QueryError) {
  case decode.run(value, claim_list_decoder()) {
    Ok(claims) -> Ok(claims)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid claim list query payload",
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
  use pending_review_lane_preflights <- decode.optional_field(
    "pending_review_lane_preflights",
    0,
    decode.int,
  )
  use claimed_tasks <- decode.field("claimed_tasks", decode.int)
  use retry_tasks <- decode.field("retry_tasks", decode.int)
  use parked_tasks <- decode.field("parked_tasks", decode.int)
  use completed_tasks <- decode.field("completed_tasks", decode.int)
  use pending_outbox_count <- decode.field("pending_outbox_count", decode.int)
  use in_flight_outbox_count <- decode.field(
    "in_flight_outbox_count",
    decode.int,
  )
  use retryable_outbox_count <- decode.field(
    "retryable_outbox_count",
    decode.int,
  )
  use permanent_outbox_count <- decode.field(
    "permanent_outbox_count",
    decode.int,
  )
  use poll_generation <- decode.field("poll_generation", decode.int)
  use poll_in_flight <- decode.field("poll_in_flight", decode.bool)
  use poll_timer_active <- decode.field("poll_timer_active", decode.bool)
  use retry_timer_count <- decode.field("retry_timer_count", decode.int)
  use retry_refresh_in_flight_count <- decode.field(
    "retry_refresh_in_flight_count",
    decode.int,
  )
  use lifecycle_projection_failed <- decode.optional_field(
    "lifecycle_projection_failed",
    False,
    decode.bool,
  )
  use scheduled_due_count <- decode.field("scheduled_due_count", decode.int)
  use scheduled_next_due_count <- decode.optional_field(
    "scheduled_next_due_count",
    scheduled_due_count,
    decode.int,
  )
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
    pending_review_lane_preflights: pending_review_lane_preflights,
    claimed_tasks: claimed_tasks,
    retry_tasks: retry_tasks,
    parked_tasks: parked_tasks,
    completed_tasks: completed_tasks,
    pending_outbox_count: pending_outbox_count,
    in_flight_outbox_count: in_flight_outbox_count,
    retryable_outbox_count: retryable_outbox_count,
    permanent_outbox_count: permanent_outbox_count,
    poll_generation: poll_generation,
    poll_in_flight: poll_in_flight,
    poll_timer_active: poll_timer_active,
    retry_timer_count: retry_timer_count,
    retry_refresh_in_flight_count: retry_refresh_in_flight_count,
    lifecycle_projection_failed: lifecycle_projection_failed,
    scheduled_due_count: scheduled_due_count,
    scheduled_next_due_count: scheduled_next_due_count,
    scheduled_pending_count: scheduled_pending_count,
    scheduled_retry_count: scheduled_retry_count,
    scheduled_report_retry_count: scheduled_report_retry_count,
    scheduled_retry_timer_count: scheduled_retry_timer_count,
    scheduled_report_retry_timer_count: scheduled_report_retry_timer_count,
    token_totals: token_totals,
  ))
}

fn claim_to_json(claim: types.ClaimDto) -> json.Json {
  json.object([
    #("task_identity", json.string(claim.task_identity)),
    #("issue_id", json.nullable(claim.issue_id, of: json.string)),
    #(
      "issue_identifier",
      json.nullable(claim.issue_identifier, of: json.string),
    ),
    #("run_id", json.nullable(claim.run_id, of: json.string)),
    #("session_id", json.nullable(claim.session_id, of: json.string)),
    #("age_ms", json.nullable(claim.age_ms, of: json.int)),
    #("holder", json.string(claim.holder)),
  ])
}

fn claim_list_decoder() -> decode.Decoder(types.ClaimListDto) {
  use sampled_at_ms <- decode.field("sampled_at_ms", decode.int)
  use items <- decode.field("items", decode.list(claim_decoder()))
  decode.success(types.ClaimListDto(sampled_at_ms: sampled_at_ms, items: items))
}

fn claim_decoder() -> decode.Decoder(types.ClaimDto) {
  use task_identity <- decode.field("task_identity", decode.string)
  use issue_id <- decode.optional_field(
    "issue_id",
    None,
    decode.optional(decode.string),
  )
  use issue_identifier <- decode.optional_field(
    "issue_identifier",
    None,
    decode.optional(decode.string),
  )
  use run_id <- decode.optional_field(
    "run_id",
    None,
    decode.optional(decode.string),
  )
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use age_ms <- decode.optional_field(
    "age_ms",
    None,
    decode.optional(decode.int),
  )
  use holder <- decode.field("holder", decode.string)
  decode.success(types.ClaimDto(
    task_identity: task_identity,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    run_id: run_id,
    session_id: session_id,
    age_ms: age_ms,
    holder: holder,
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

pub fn task_summary_from_task(item: task.Task) -> types.TaskSummaryDto {
  let task.Task(
    ref: ref,
    title: title,
    state: state,
    priority: priority,
    labels: labels,
    created_at: created_at,
    updated_at: updated_at,
    ..,
  ) = item
  let task.TaskRef(
    backend_kind: provider,
    remote_id: remote_id,
    key: display_id,
    url: url,
  ) = ref

  types.TaskSummaryDto(
    id: provider <> ":" <> remote_id,
    source: types.TaskSourceDto(
      provider: provider,
      id: remote_id,
      display_id: display_id,
      url: url,
    ),
    title: title,
    state: state.category,
    priority: option_map(priority, priority_to_dto),
    labels: list.map(labels, label_to_dto),
    created_at: option_map(created_at, birl.to_iso8601),
    updated_at: option_map(updated_at, birl.to_iso8601),
  )
}

pub fn task_detail_from_task(item: task.Task) -> types.TaskDetailDto {
  types.TaskDetailDto(
    summary: task_summary_from_task(item),
    description: types.TaskDescriptionDto(
      format: "markdown",
      body: option_with_default(item.description, ""),
    ),
  )
}

pub fn task_list_to_json(tasks: types.TaskListDto) -> json.Json {
  json.object([
    #("items", json.array(tasks.items, of: task_summary_to_json)),
    #("page", page_to_json(tasks.page)),
  ])
}

pub fn task_detail_to_json(detail: types.TaskDetailDto) -> json.Json {
  let types.TaskDetailDto(summary: summary, description: description) = detail
  let types.TaskSummaryDto(
    id: id,
    source: source,
    title: title,
    state: state,
    priority: priority,
    labels: labels,
    created_at: created_at,
    updated_at: updated_at,
  ) = summary

  json.object([
    #("id", json.string(id)),
    #("source", source_to_json(source)),
    #("title", json.string(title)),
    #("state", json.string(task.state_category_to_string(state))),
    #("priority", json.nullable(priority, of: priority_to_json)),
    #("labels", json.array(labels, of: label_dto_to_json)),
    #("created_at", json.nullable(created_at, of: json.string)),
    #("updated_at", json.nullable(updated_at, of: json.string)),
    #("description", description_to_json(description)),
  ])
}

pub fn outbox_list_to_json(outbox: types.OutboxListDto) -> json.Json {
  json.object([
    #("items", json.array(outbox.items, of: outbox_record_to_json)),
    #("page", page_to_json(outbox.page)),
  ])
}

pub fn operation_status_to_json(
  operation: types.OperationStatusDto,
) -> json.Json {
  json.object([
    #("operation_id", json.string(operation.operation_id)),
    #("kind", json.string(operation.kind)),
    #("command", json.string(operation.command)),
    #("target", json.string(operation.target)),
    #("run_id", json.nullable(operation.run_id, of: json.string)),
    #("issue_id", json.nullable(operation.issue_id, of: json.string)),
    #(
      "issue_identifier",
      json.nullable(operation.issue_identifier, of: json.string),
    ),
    #(
      "requested_step_id",
      json.nullable(operation.requested_step_id, of: json.string),
    ),
    #(
      "publication_id",
      json.nullable(operation.publication_id, of: json.string),
    ),
    #("status", json.string(operation.status)),
    #("reason", json.nullable(operation.reason, of: json.string)),
    #("message", json.nullable(operation.message, of: json.string)),
    #("queued_at_ms", json.int(operation.queued_at_ms)),
    #("started_at_ms", json.nullable(operation.started_at_ms, of: json.int)),
    #("finished_at_ms", json.nullable(operation.finished_at_ms, of: json.int)),
  ])
}

pub fn outbox_record_to_json(record: types.OutboxRecordDto) -> json.Json {
  json.object([
    #("outbox_id", json.string(record.outbox_id)),
    #("kind", json.string(record.kind)),
    #("status", json.string(types.outbox_status_to_string(record.status))),
    #("task_ref", outbox_task_ref_to_json(record.task_ref)),
    #("dedupe_key", json.nullable(record.dedupe_key, of: json.string)),
    #("attempt_count", json.nullable(record.attempt_count, of: json.int)),
    #(
      "next_attempt_at_ms",
      json.nullable(record.next_attempt_at_ms, of: json.int),
    ),
    #("last_error_code", json.nullable(record.last_error_code, of: json.string)),
    #("pending_at_ms", json.nullable(record.pending_at_ms, of: json.int)),
    #("attempted_at_ms", json.nullable(record.attempted_at_ms, of: json.int)),
    #("failed_at_ms", json.nullable(record.failed_at_ms, of: json.int)),
    #("completed_at_ms", json.nullable(record.completed_at_ms, of: json.int)),
    #("has_payload", json.bool(record.has_payload)),
  ])
}

pub fn decode_task_list_dynamic(
  value: Dynamic,
) -> Result(types.TaskListDto, types.QueryError) {
  case decode.run(value, task_list_decoder()) {
    Ok(tasks) -> Ok(tasks)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid task list query payload",
      ))
  }
}

pub fn decode_task_detail_dynamic(
  value: Dynamic,
) -> Result(types.TaskDetailDto, types.QueryError) {
  case decode.run(value, task_detail_decoder()) {
    Ok(task) -> Ok(task)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid task detail query payload",
      ))
  }
}

pub fn decode_operation_status_dynamic(
  value: Dynamic,
) -> Result(types.OperationStatusDto, types.QueryError) {
  case decode.run(value, operation_status_decoder()) {
    Ok(operation) -> Ok(operation)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid operation status query payload",
      ))
  }
}

pub fn decode_outbox_list_dynamic(
  value: Dynamic,
) -> Result(types.OutboxListDto, types.QueryError) {
  case decode.run(value, outbox_list_decoder()) {
    Ok(outbox) -> Ok(outbox)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid outbox list query payload",
      ))
  }
}

pub fn decode_outbox_record_dynamic(
  value: Dynamic,
) -> Result(types.OutboxRecordDto, types.QueryError) {
  case decode.run(value, outbox_record_decoder()) {
    Ok(record) -> Ok(record)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid outbox record query payload",
      ))
  }
}

fn task_summary_to_json(summary: types.TaskSummaryDto) -> json.Json {
  let types.TaskSummaryDto(
    id: id,
    source: source,
    title: title,
    state: state,
    priority: priority,
    labels: labels,
    created_at: created_at,
    updated_at: updated_at,
  ) = summary

  json.object([
    #("id", json.string(id)),
    #("source", source_to_json(source)),
    #("title", json.string(title)),
    #("state", json.string(task.state_category_to_string(state))),
    #("priority", json.nullable(priority, of: priority_to_json)),
    #("labels", json.array(labels, of: label_dto_to_json)),
    #("created_at", json.nullable(created_at, of: json.string)),
    #("updated_at", json.nullable(updated_at, of: json.string)),
  ])
}

fn source_to_json(source: types.TaskSourceDto) -> json.Json {
  json.object([
    #("provider", json.string(source.provider)),
    #("id", json.string(source.id)),
    #("display_id", json.nullable(source.display_id, of: json.string)),
    #("url", json.nullable(source.url, of: json.string)),
  ])
}

fn outbox_task_ref_to_json(task_ref: types.OutboxTaskRefDto) -> json.Json {
  json.object([
    #("provider", json.string(task_ref.provider)),
    #("id", json.string(task_ref.id)),
    #("display_id", json.nullable(task_ref.display_id, of: json.string)),
    #("url", json.nullable(task_ref.url, of: json.string)),
  ])
}

fn priority_to_json(priority: types.TaskPriorityDto) -> json.Json {
  json.object([
    #("value", json.int(priority.value)),
    #("label", json.string(priority.label)),
  ])
}

fn label_dto_to_json(label: types.TaskLabelDto) -> json.Json {
  json.object([
    #("id", json.nullable(label.id, of: json.string)),
    #("name", json.string(label.name)),
  ])
}

fn description_to_json(description: types.TaskDescriptionDto) -> json.Json {
  json.object([
    #("format", json.string(description.format)),
    #("body", json.string(description.body)),
  ])
}

fn page_to_json(page: types.PageDto) -> json.Json {
  json.object([
    #("next_cursor", json.nullable(page.next_cursor, of: json.string)),
    #("has_more", json.bool(page.has_more)),
  ])
}

fn priority_to_dto(value: Int) -> types.TaskPriorityDto {
  types.TaskPriorityDto(value: value, label: priority_label(value))
}

fn priority_label(value: Int) -> String {
  case value {
    0 -> "No priority"
    1 -> "Urgent"
    2 -> "High"
    3 -> "Medium"
    4 -> "Low"
    _ -> "Priority " <> int.to_string(value)
  }
}

fn label_to_dto(label: task.TaskLabel) -> types.TaskLabelDto {
  types.TaskLabelDto(id: label.id, name: label.name)
}

fn task_list_decoder() -> decode.Decoder(types.TaskListDto) {
  use items <- decode.field("items", decode.list(task_summary_decoder()))
  use page <- decode.field("page", page_decoder())
  decode.success(types.TaskListDto(items: items, page: page))
}

fn task_detail_decoder() -> decode.Decoder(types.TaskDetailDto) {
  use summary <- decode.then(task_summary_decoder())
  use description <- decode.field("description", description_decoder())
  decode.success(types.TaskDetailDto(summary: summary, description: description))
}

fn outbox_list_decoder() -> decode.Decoder(types.OutboxListDto) {
  use items <- decode.field("items", decode.list(outbox_record_decoder()))
  use page <- decode.field("page", page_decoder())
  decode.success(types.OutboxListDto(items: items, page: page))
}

fn operation_status_decoder() -> decode.Decoder(types.OperationStatusDto) {
  use operation_id <- decode.field("operation_id", decode.string)
  use kind <- decode.field("kind", decode.string)
  use command <- decode.field("command", decode.string)
  use target <- decode.field("target", decode.string)
  use run_id <- decode.field("run_id", decode.optional(decode.string))
  use issue_id <- decode.field("issue_id", decode.optional(decode.string))
  use issue_identifier <- decode.field(
    "issue_identifier",
    decode.optional(decode.string),
  )
  use requested_step_id <- decode.field(
    "requested_step_id",
    decode.optional(decode.string),
  )
  use publication_id <- decode.optional_field(
    "publication_id",
    None,
    decode.optional(decode.string),
  )
  use status <- decode.field("status", decode.string)
  use reason <- decode.field("reason", decode.optional(decode.string))
  use message <- decode.field("message", decode.optional(decode.string))
  use queued_at_ms <- decode.field("queued_at_ms", decode.int)
  use started_at_ms <- decode.field(
    "started_at_ms",
    decode.optional(decode.int),
  )
  use finished_at_ms <- decode.field(
    "finished_at_ms",
    decode.optional(decode.int),
  )
  decode.success(types.OperationStatusDto(
    operation_id: operation_id,
    kind: kind,
    command: command,
    target: target,
    run_id: run_id,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    requested_step_id: requested_step_id,
    publication_id: publication_id,
    status: status,
    reason: reason,
    message: message,
    queued_at_ms: queued_at_ms,
    started_at_ms: started_at_ms,
    finished_at_ms: finished_at_ms,
  ))
}

fn outbox_record_decoder() -> decode.Decoder(types.OutboxRecordDto) {
  use outbox_id <- decode.field("outbox_id", decode.string)
  use kind <- decode.field("kind", decode.string)
  use status <- decode.field("status", outbox_status_decoder())
  use task_ref <- decode.field("task_ref", outbox_task_ref_decoder())
  use dedupe_key <- decode.field("dedupe_key", decode.optional(decode.string))
  use attempt_count <- decode.field(
    "attempt_count",
    decode.optional(decode.int),
  )
  use next_attempt_at_ms <- decode.field(
    "next_attempt_at_ms",
    decode.optional(decode.int),
  )
  use last_error_code <- decode.field(
    "last_error_code",
    decode.optional(decode.string),
  )
  use pending_at_ms <- decode.field(
    "pending_at_ms",
    decode.optional(decode.int),
  )
  use attempted_at_ms <- decode.field(
    "attempted_at_ms",
    decode.optional(decode.int),
  )
  use failed_at_ms <- decode.field("failed_at_ms", decode.optional(decode.int))
  use completed_at_ms <- decode.field(
    "completed_at_ms",
    decode.optional(decode.int),
  )
  use has_payload <- decode.field("has_payload", decode.bool)
  decode.success(types.OutboxRecordDto(
    outbox_id: outbox_id,
    kind: kind,
    status: status,
    task_ref: task_ref,
    dedupe_key: dedupe_key,
    attempt_count: attempt_count,
    next_attempt_at_ms: next_attempt_at_ms,
    last_error_code: last_error_code,
    pending_at_ms: pending_at_ms,
    attempted_at_ms: attempted_at_ms,
    failed_at_ms: failed_at_ms,
    completed_at_ms: completed_at_ms,
    has_payload: has_payload,
  ))
}

fn outbox_task_ref_decoder() -> decode.Decoder(types.OutboxTaskRefDto) {
  use provider <- decode.field("provider", decode.string)
  use id <- decode.field("id", decode.string)
  use display_id <- decode.field("display_id", decode.optional(decode.string))
  use url <- decode.field("url", decode.optional(decode.string))
  decode.success(types.OutboxTaskRefDto(
    provider: provider,
    id: id,
    display_id: display_id,
    url: url,
  ))
}

fn task_summary_decoder() -> decode.Decoder(types.TaskSummaryDto) {
  use id <- decode.field("id", decode.string)
  use source <- decode.field("source", source_decoder())
  use title <- decode.field("title", decode.string)
  use state <- decode.field("state", state_category_decoder())
  use priority <- decode.field("priority", decode.optional(priority_decoder()))
  use labels <- decode.field("labels", decode.list(label_dto_decoder()))
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))
  decode.success(types.TaskSummaryDto(
    id: id,
    source: source,
    title: title,
    state: state,
    priority: priority,
    labels: labels,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

fn source_decoder() -> decode.Decoder(types.TaskSourceDto) {
  use provider <- decode.field("provider", decode.string)
  use id <- decode.field("id", decode.string)
  use display_id <- decode.field("display_id", decode.optional(decode.string))
  use url <- decode.field("url", decode.optional(decode.string))
  decode.success(types.TaskSourceDto(
    provider: provider,
    id: id,
    display_id: display_id,
    url: url,
  ))
}

fn priority_decoder() -> decode.Decoder(types.TaskPriorityDto) {
  use value <- decode.field("value", decode.int)
  use label <- decode.field("label", decode.string)
  decode.success(types.TaskPriorityDto(value: value, label: label))
}

fn label_dto_decoder() -> decode.Decoder(types.TaskLabelDto) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  decode.success(types.TaskLabelDto(id: id, name: name))
}

fn description_decoder() -> decode.Decoder(types.TaskDescriptionDto) {
  use format <- decode.field("format", decode.string)
  use body <- decode.field("body", decode.string)
  decode.success(types.TaskDescriptionDto(format: format, body: body))
}

fn page_decoder() -> decode.Decoder(types.PageDto) {
  use next_cursor <- decode.field("next_cursor", decode.optional(decode.string))
  use has_more <- decode.field("has_more", decode.bool)
  decode.success(types.PageDto(next_cursor: next_cursor, has_more: has_more))
}

fn state_category_decoder() -> decode.Decoder(task.TaskStateCategory) {
  use value <- decode.then(decode.string)
  case task.state_category_from_string(value) {
    Ok(category) -> decode.success(category)
    Error(Nil) -> decode.failure(task.Unknown, expected: "TaskStateCategory")
  }
}

fn outbox_status_decoder() -> decode.Decoder(types.OutboxRecordStatus) {
  use value <- decode.then(decode.string)
  case types.outbox_status_from_string(value) {
    Ok(status) -> decode.success(status)
    Error(Nil) ->
      decode.failure(types.OutboxPendingStatus, expected: "OutboxStatus")
  }
}

fn option_map(value: Option(a), mapper: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(mapper(value))
    None -> None
  }
}

fn option_with_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}
