import gleam/list

pub type QueryRequest {
  Status
}

pub type QueryResponse {
  StatusResponse(status: StatusDto)
}

pub type QueryErrorCode {
  InvalidCursor
  UnsupportedQuery
  QueryTimeout
  QueryOverloaded
  QueryShutdown
  QueryBackendFailed
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
  ["status"]
}

pub fn error_code_to_string(code: QueryErrorCode) -> String {
  case code {
    InvalidCursor -> "invalid_cursor"
    UnsupportedQuery -> "unsupported_query"
    QueryTimeout -> "query_timeout"
    QueryOverloaded -> "query_overloaded"
    QueryShutdown -> "query_shutdown"
    QueryBackendFailed -> "query_backend_failed"
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
    _ -> Error(Nil)
  }
}

pub fn query_type(request: QueryRequest) -> String {
  case request {
    Status -> "status"
  }
}

pub fn response_type(response: QueryResponse) -> String {
  case response {
    StatusResponse(_) -> "status"
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

pub fn status_supported_queries(status: StatusDto) -> List(String) {
  case status {
    StatusDto(supported_queries: queries, ..) ->
      case list.is_empty(queries) {
        True -> supported_queries()
        False -> queries
      }
  }
}
