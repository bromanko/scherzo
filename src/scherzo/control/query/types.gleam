import gleam/list
import gleam/option.{type Option, None}
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/work_item

pub type QueryRequest {
  Status
  Metrics
  TaskList(TaskListQuery)
  TaskShow(TaskShowQuery)
  WorkItemList(WorkItemListQuery)
  WorkItemShow(WorkItemShowQuery)
  OutboxList(OutboxListQuery)
  OutboxShow(OutboxShowQuery)
}

pub type QueryResponse {
  StatusResponse(status: StatusDto)
  MetricsResponse(metrics: OperationalMetricsDto)
  TaskListResponse(tasks: TaskListDto)
  TaskShowResponse(task: TaskDetailDto)
  WorkItemListResponse(work_item_list: work_item.WorkItemPage)
  WorkItemShowResponse(work_item: work_item.WorkItemDetail)
  OutboxListResponse(outbox: OutboxListDto)
  OutboxShowResponse(outbox: OutboxRecordDto)
}

pub type TaskListQuery {
  TaskListQuery(
    states: List(task.TaskStateCategory),
    limit: Int,
    cursor: Option(String),
  )
}

pub type TaskShowQuery {
  TaskShowQuery(ref: TaskQueryRef)
}

pub type WorkItemListQuery {
  WorkItemListQuery(
    state_filter: work_item.WorkItemStateFilter,
    search: Option(String),
    sort: work_item.WorkItemSort,
    limit: Int,
    cursor: Option(String),
  )
}

pub type WorkItemShowQuery {
  WorkItemShowQuery(ref: TaskQueryRef)
}

pub type OutboxListQuery {
  OutboxListQuery(
    statuses: List(OutboxRecordStatus),
    kinds: List(String),
    limit: Int,
    cursor: Option(String),
  )
}

pub type OutboxShowQuery {
  OutboxShowQuery(outbox_id: String)
}

pub type TaskQueryRef {
  TaskDisplayId(String)
  TaskRemoteId(provider: Option(String), id: String)
}

pub type OutboxRecordStatus {
  OutboxPendingStatus
  OutboxInFlightStatus
  OutboxRetryableStatus
  OutboxCompletedStatus
  OutboxFailedStatus
  OutboxPermanentStatus
}

pub type QueryErrorCode {
  InvalidCursor
  UnsupportedQuery
  QueryTimeout
  QueryOverloaded
  QueryShutdown
  QueryBackendFailed
  QueryNotFound
}

pub type QueryError {
  QueryError(code: QueryErrorCode, message: String)
}

pub type StatusDto {
  StatusDto(
    daemon_id: String,
    boot_id: String,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    supported_queries: List(String),
  )
}

pub type TaskSourceDto {
  TaskSourceDto(
    provider: String,
    id: String,
    display_id: Option(String),
    url: Option(String),
  )
}

pub type TaskPriorityDto {
  TaskPriorityDto(value: Int, label: String)
}

pub type TaskLabelDto {
  TaskLabelDto(id: Option(String), name: String)
}

pub type TaskDescriptionDto {
  TaskDescriptionDto(format: String, body: String)
}

pub type TaskSummaryDto {
  TaskSummaryDto(
    id: String,
    source: TaskSourceDto,
    title: String,
    state: task.TaskStateCategory,
    priority: Option(TaskPriorityDto),
    labels: List(TaskLabelDto),
    created_at: Option(String),
    updated_at: Option(String),
  )
}

pub type PageDto {
  PageDto(next_cursor: Option(String), has_more: Bool)
}

pub type TaskListDto {
  TaskListDto(items: List(TaskSummaryDto), page: PageDto)
}

pub type TaskDetailDto {
  TaskDetailDto(summary: TaskSummaryDto, description: TaskDescriptionDto)
}

pub type OutboxTaskRefDto {
  OutboxTaskRefDto(
    provider: String,
    id: String,
    display_id: Option(String),
    url: Option(String),
  )
}

pub type OutboxRecordDto {
  OutboxRecordDto(
    outbox_id: String,
    kind: String,
    status: OutboxRecordStatus,
    task_ref: OutboxTaskRefDto,
    dedupe_key: Option(String),
    attempt_count: Option(Int),
    next_attempt_at_ms: Option(Int),
    last_error_code: Option(String),
    pending_at_ms: Option(Int),
    attempted_at_ms: Option(Int),
    failed_at_ms: Option(Int),
    completed_at_ms: Option(Int),
    has_payload: Bool,
  )
}

pub type OutboxListDto {
  OutboxListDto(items: List(OutboxRecordDto), page: PageDto)
}

pub type StatusSource {
  StatusSource(
    daemon_id: String,
    boot_id: String,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    supported_queries: List(String),
    local_control_token: String,
    enrollment_token: String,
    tracker_payload: String,
    workflow_internals: List(String),
  )
}

pub type TokenTotalsDto {
  TokenTotalsDto(
    input: Int,
    output: Int,
    cache_read: Int,
    cache_write: Int,
    total: Int,
  )
}

pub type OperationalMetricsDto {
  OperationalMetricsDto(
    schema_version: Int,
    daemon_id: String,
    boot_id: String,
    sampled_at_ms: Int,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    remote_client_status: String,
    workflow_count: Int,
    scheduled_job_count: Int,
    active_sessions: Int,
    running_workers: Int,
    running_scheduled_workers: Int,
    queued_claims: Int,
    pending_dispatch_validations: Int,
    pending_review_lane_preflights: Int,
    claimed_tasks: Int,
    retry_tasks: Int,
    parked_tasks: Int,
    completed_tasks: Int,
    pending_outbox_count: Int,
    in_flight_outbox_count: Int,
    retryable_outbox_count: Int,
    permanent_outbox_count: Int,
    poll_generation: Int,
    poll_in_flight: Bool,
    poll_timer_active: Bool,
    retry_timer_count: Int,
    retry_refresh_in_flight_count: Int,
    lifecycle_projection_failed: Bool,
    scheduled_due_count: Int,
    scheduled_next_due_count: Int,
    scheduled_pending_count: Int,
    scheduled_retry_count: Int,
    scheduled_report_retry_count: Int,
    scheduled_retry_timer_count: Int,
    scheduled_report_retry_timer_count: Int,
    token_totals: TokenTotalsDto,
  )
}

pub type OperationalMetricsSource {
  OperationalMetricsSource(
    daemon_id: String,
    boot_id: String,
    sampled_at_ms: Int,
    dispatch_paused: Bool,
    ui_server_enabled: Bool,
    remote_client_status: String,
    workflow_count: Int,
    scheduled_job_count: Int,
    active_sessions: Int,
    running_workers: Int,
    running_scheduled_workers: Int,
    queued_claims: Int,
    pending_dispatch_validations: Int,
    pending_review_lane_preflights: Int,
    claimed_tasks: Int,
    retry_tasks: Int,
    parked_tasks: Int,
    completed_tasks: Int,
    pending_outbox_count: Int,
    in_flight_outbox_count: Int,
    retryable_outbox_count: Int,
    permanent_outbox_count: Int,
    poll_generation: Int,
    poll_in_flight: Bool,
    poll_timer_active: Bool,
    retry_timer_count: Int,
    retry_refresh_in_flight_count: Int,
    lifecycle_projection_failed: Bool,
    scheduled_due_count: Int,
    scheduled_next_due_count: Int,
    scheduled_pending_count: Int,
    scheduled_retry_count: Int,
    scheduled_report_retry_count: Int,
    scheduled_retry_timer_count: Int,
    scheduled_report_retry_timer_count: Int,
    aggregate_tokens: session_tokens.TokenTotals,
  )
}

pub const operational_metrics_schema_version = 1

pub fn supported_queries() -> List(String) {
  [
    "status",
    "metrics",
    "task_list",
    "task_show",
    "work_item_list",
    "work_item_show",
    "outbox_list",
    "outbox_show",
  ]
}

pub fn error_code_to_string(code: QueryErrorCode) -> String {
  case code {
    InvalidCursor -> "invalid_cursor"
    UnsupportedQuery -> "unsupported_query"
    QueryTimeout -> "query_timeout"
    QueryOverloaded -> "query_overloaded"
    QueryShutdown -> "query_shutdown"
    QueryBackendFailed -> "query_backend_failed"
    QueryNotFound -> "not_found"
  }
}

pub fn error_code_from_string(value: String) -> Result(QueryErrorCode, Nil) {
  case value {
    "invalid_cursor" -> Ok(InvalidCursor)
    "unsupported_query" -> Ok(UnsupportedQuery)
    "query_timeout" -> Ok(QueryTimeout)
    "query_overloaded" -> Ok(QueryOverloaded)
    "query_shutdown" -> Ok(QueryShutdown)
    "query_backend_failed" -> Ok(QueryBackendFailed)
    "not_found" -> Ok(QueryNotFound)
    _ -> Error(Nil)
  }
}

pub fn query_type(request: QueryRequest) -> String {
  case request {
    Status -> "status"
    Metrics -> "metrics"
    TaskList(_) -> "task_list"
    TaskShow(_) -> "task_show"
    WorkItemList(_) -> "work_item_list"
    WorkItemShow(_) -> "work_item_show"
    OutboxList(_) -> "outbox_list"
    OutboxShow(_) -> "outbox_show"
  }
}

pub fn response_type(response: QueryResponse) -> String {
  case response {
    StatusResponse(_) -> "status"
    MetricsResponse(_) -> "metrics"
    TaskListResponse(_) -> "task_list"
    TaskShowResponse(_) -> "task_show"
    WorkItemListResponse(_) -> "work_item_list"
    WorkItemShowResponse(_) -> "work_item_show"
    OutboxListResponse(_) -> "outbox_list"
    OutboxShowResponse(_) -> "outbox_show"
  }
}

pub fn default_task_list_query() -> TaskListQuery {
  TaskListQuery(states: [], limit: 50, cursor: None)
}

pub fn default_work_item_list_query() -> WorkItemListQuery {
  WorkItemListQuery(
    state_filter: work_item.default_state_filter(),
    search: None,
    sort: work_item.default_sort(),
    limit: 50,
    cursor: None,
  )
}

pub fn default_outbox_list_query() -> OutboxListQuery {
  OutboxListQuery(statuses: [], kinds: [], limit: 50, cursor: None)
}

pub fn outbox_status_to_string(status: OutboxRecordStatus) -> String {
  case status {
    OutboxPendingStatus -> "pending"
    OutboxInFlightStatus -> "in_flight"
    OutboxRetryableStatus -> "retryable"
    OutboxCompletedStatus -> "completed"
    OutboxFailedStatus -> "failed"
    OutboxPermanentStatus -> "permanent"
  }
}

pub fn outbox_status_from_string(
  value: String,
) -> Result(OutboxRecordStatus, Nil) {
  case value {
    "pending" -> Ok(OutboxPendingStatus)
    "in_flight" | "in-flight" -> Ok(OutboxInFlightStatus)
    "retryable" -> Ok(OutboxRetryableStatus)
    "completed" -> Ok(OutboxCompletedStatus)
    "failed" -> Ok(OutboxFailedStatus)
    "permanent" | "permanently_failed" | "permanently-failed" ->
      Ok(OutboxPermanentStatus)
    _ -> Error(Nil)
  }
}

pub fn default_status_source(
  daemon_id daemon_id: String,
  boot_id boot_id: String,
) -> StatusSource {
  StatusSource(
    daemon_id: daemon_id,
    boot_id: boot_id,
    dispatch_paused: False,
    ui_server_enabled: False,
    supported_queries: supported_queries(),
    local_control_token: "",
    enrollment_token: "",
    tracker_payload: "",
    workflow_internals: [],
  )
}

pub fn default_operational_metrics_source(
  daemon_id daemon_id: String,
  boot_id boot_id: String,
) -> OperationalMetricsSource {
  OperationalMetricsSource(
    daemon_id: daemon_id,
    boot_id: boot_id,
    sampled_at_ms: 0,
    dispatch_paused: False,
    ui_server_enabled: False,
    remote_client_status: "disabled",
    workflow_count: 0,
    scheduled_job_count: 0,
    active_sessions: 0,
    running_workers: 0,
    running_scheduled_workers: 0,
    queued_claims: 0,
    pending_dispatch_validations: 0,
    pending_review_lane_preflights: 0,
    claimed_tasks: 0,
    retry_tasks: 0,
    parked_tasks: 0,
    completed_tasks: 0,
    pending_outbox_count: 0,
    in_flight_outbox_count: 0,
    retryable_outbox_count: 0,
    permanent_outbox_count: 0,
    poll_generation: 0,
    poll_in_flight: False,
    poll_timer_active: False,
    retry_timer_count: 0,
    retry_refresh_in_flight_count: 0,
    lifecycle_projection_failed: False,
    scheduled_due_count: 0,
    scheduled_next_due_count: 0,
    scheduled_pending_count: 0,
    scheduled_retry_count: 0,
    scheduled_report_retry_count: 0,
    scheduled_retry_timer_count: 0,
    scheduled_report_retry_timer_count: 0,
    aggregate_tokens: session_tokens.zero_token_totals(),
  )
}

pub fn status_supported_queries(status: StatusDto) -> List(String) {
  case status {
    StatusDto(supported_queries: queries, ..) ->
      case list.is_empty(queries) {
        True -> supported_queries()
        False -> queries
      }
  }
}
