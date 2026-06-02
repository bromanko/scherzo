import gleam/list
import gleam/option.{type Option, None}
import scherzo/task

pub type QueryRequest {
  Status
  TaskList(TaskListQuery)
  TaskShow(TaskShowQuery)
}

pub type QueryResponse {
  StatusResponse(status: StatusDto)
  TaskListResponse(tasks: TaskListDto)
  TaskShowResponse(task: TaskDetailDto)
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

pub type TaskQueryRef {
  TaskDisplayId(String)
  TaskRemoteId(provider: Option(String), id: String)
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

pub fn supported_queries() -> List(String) {
  ["status", "task_list", "task_show"]
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
    TaskList(_) -> "task_list"
    TaskShow(_) -> "task_show"
  }
}

pub fn response_type(response: QueryResponse) -> String {
  case response {
    StatusResponse(_) -> "status"
    TaskListResponse(_) -> "task_list"
    TaskShowResponse(_) -> "task_show"
  }
}

pub fn default_task_list_query() -> TaskListQuery {
  TaskListQuery(states: [], limit: 50, cursor: None)
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

pub fn status_supported_queries(status: StatusDto) -> List(String) {
  case status {
    StatusDto(supported_queries: queries, ..) ->
      case list.is_empty(queries) {
        True -> supported_queries()
        False -> queries
      }
  }
}
