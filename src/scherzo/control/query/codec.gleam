import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/task

pub const version = 1

pub fn request_to_json(request: types.QueryRequest) -> json.Json {
  request_entries(request) |> json.object
}

fn request_entries(request: types.QueryRequest) -> List(#(String, json.Json)) {
  case request {
    types.Status -> base_request_entries(types.query_type(request))
    types.Metrics -> base_request_entries(types.query_type(request))
    types.TaskList(query) ->
      list.append(
        task_list_query_entries(query),
        base_request_entries(types.query_type(request)),
      )
    types.TaskShow(query) -> [
      #("ref", task_query_ref_to_json(query.ref)),
      ..base_request_entries(types.query_type(request))
    ]
  }
}

fn base_request_entries(type_: String) -> List(#(String, json.Json)) {
  [#("version", json.int(version)), #("type", json.string(type_))]
}

fn task_list_query_entries(
  query: types.TaskListQuery,
) -> List(#(String, json.Json)) {
  [
    #("states", json.array(query.states, of: task_state_category_to_json)),
    #("limit", json.int(query.limit)),
    #("cursor", json.nullable(query.cursor, of: json.string)),
  ]
}

fn task_query_ref_to_json(ref: types.TaskQueryRef) -> json.Json {
  case ref {
    types.TaskDisplayId(value) ->
      json.object([
        #("kind", json.string("display_id")),
        #("value", json.string(value)),
      ])
    types.TaskRemoteId(provider: provider, id: id) ->
      json.object([
        #("kind", json.string("remote_id")),
        #("provider", json.nullable(provider, of: json.string)),
        #("id", json.string(id)),
      ])
  }
}

fn task_state_category_to_json(category: task.TaskStateCategory) -> json.Json {
  category |> task.state_category_to_string |> json.string
}

pub fn request_to_string(request: types.QueryRequest) -> String {
  request |> request_to_json |> json.to_string
}

pub fn decode_request(
  line: String,
) -> Result(types.QueryRequest, types.QueryError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_request_dynamic(value)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "malformed query request JSON",
      ))
  }
}

pub fn decode_request_dynamic(
  value: Dynamic,
) -> Result(types.QueryRequest, types.QueryError) {
  case decode.run(value, request_fields_decoder()) {
    Ok(fields) -> request_from_fields(fields)
    Error(_) ->
      Error(types.QueryError(types.QueryBackendFailed, "invalid query request"))
  }
}

pub fn response_to_json(response: types.QueryResponse) -> json.Json {
  case response {
    types.StatusResponse(status) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("status", dto.status_to_json(status)),
      ])
    types.MetricsResponse(metrics) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("metrics", dto.operational_metrics_to_json(metrics)),
      ])
    types.TaskListResponse(tasks) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("task_list", dto.task_list_to_json(tasks)),
      ])
    types.TaskShowResponse(task) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("task", dto.task_detail_to_json(task)),
      ])
  }
}

pub fn response_to_string(response: types.QueryResponse) -> String {
  response |> response_to_json |> json.to_string
}

pub fn error_to_json(error: types.QueryError) -> json.Json {
  let types.QueryError(code: code, message: message) = error
  json.object([
    #("version", json.int(version)),
    #("ok", json.bool(False)),
    #(
      "error",
      json.object([
        #("code", json.string(types.error_code_to_string(code))),
        #("message", json.string(message)),
      ]),
    ),
  ])
}

pub fn error_to_string(error: types.QueryError) -> String {
  error |> error_to_json |> json.to_string
}

pub fn decode_response(
  line: String,
) -> Result(types.QueryResponse, types.QueryError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_response_dynamic(value)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "malformed query response JSON",
      ))
  }
}

pub fn decode_response_dynamic(
  value: Dynamic,
) -> Result(types.QueryResponse, types.QueryError) {
  case decode.run(value, response_fields_decoder()) {
    Ok(fields) -> response_from_fields(fields)
    Error(_) ->
      Error(types.QueryError(types.QueryBackendFailed, "invalid query response"))
  }
}

type RequestFields {
  RequestFields(
    version: Option(Int),
    type_: Option(String),
    limit: Option(Int),
    cursor: Option(String),
    states: List(String),
    ref: Option(TaskRefFields),
  )
}

type TaskRefFields {
  TaskRefFields(
    kind: Option(String),
    value: Option(String),
    provider: Option(String),
    id: Option(String),
  )
}

type ResponseFields {
  ResponseFields(
    version: Option(Int),
    ok: Option(Bool),
    type_: Option(String),
    status: Option(Dynamic),
    metrics: Option(Dynamic),
    task_list: Option(Dynamic),
    task: Option(Dynamic),
    error: Option(ErrorFields),
  )
}

type ErrorFields {
  ErrorFields(code: Option(String), message: Option(String))
}

fn request_fields_decoder() -> decode.Decoder(RequestFields) {
  use version <- decode.optional_field(
    "version",
    None,
    decode.optional(decode.int),
  )
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use limit <- decode.optional_field("limit", None, decode.optional(decode.int))
  use cursor <- decode.optional_field(
    "cursor",
    None,
    decode.optional(decode.string),
  )
  use states <- decode.optional_field("states", [], decode.list(decode.string))
  use ref <- decode.optional_field(
    "ref",
    None,
    decode.optional(task_ref_fields_decoder()),
  )
  decode.success(RequestFields(
    version: version,
    type_: type_,
    limit: limit,
    cursor: cursor,
    states: states,
    ref: ref,
  ))
}

fn task_ref_fields_decoder() -> decode.Decoder(TaskRefFields) {
  use kind <- decode.optional_field(
    "kind",
    None,
    decode.optional(decode.string),
  )
  use value <- decode.optional_field(
    "value",
    None,
    decode.optional(decode.string),
  )
  use provider <- decode.optional_field(
    "provider",
    None,
    decode.optional(decode.string),
  )
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  decode.success(TaskRefFields(
    kind: kind,
    value: value,
    provider: provider,
    id: id,
  ))
}

fn response_fields_decoder() -> decode.Decoder(ResponseFields) {
  use version <- decode.optional_field(
    "version",
    None,
    decode.optional(decode.int),
  )
  use ok <- decode.optional_field("ok", None, decode.optional(decode.bool))
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use status <- decode.optional_field(
    "status",
    None,
    decode.optional(decode.dynamic),
  )
  use metrics <- decode.optional_field(
    "metrics",
    None,
    decode.optional(decode.dynamic),
  )
  use task_list <- decode.optional_field(
    "task_list",
    None,
    decode.optional(decode.dynamic),
  )
  use task <- decode.optional_field(
    "task",
    None,
    decode.optional(decode.dynamic),
  )
  use error <- decode.optional_field(
    "error",
    None,
    decode.optional(error_fields_decoder()),
  )
  decode.success(ResponseFields(
    version: version,
    ok: ok,
    type_: type_,
    status: status,
    metrics: metrics,
    task_list: task_list,
    task: task,
    error: error,
  ))
}

fn error_fields_decoder() -> decode.Decoder(ErrorFields) {
  use code <- decode.optional_field(
    "code",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  decode.success(ErrorFields(code: code, message: message))
}

fn request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case require_version(fields.version) {
    Error(error) -> Error(error)
    Ok(Nil) ->
      case fields.type_ {
        Some("status") -> Ok(types.Status)
        Some("metrics") -> Ok(types.Metrics)
        Some("task_list") -> task_list_request_from_fields(fields)
        Some("task_show") -> task_show_request_from_fields(fields)
        Some(other) ->
          Error(types.QueryError(
            types.UnsupportedQuery,
            "unsupported query type: " <> other,
          ))
        None ->
          Error(types.QueryError(types.QueryBackendFailed, "missing query type"))
      }
  }
}

fn task_list_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  use states <- result.try(decode_state_categories(fields.states))
  use limit <- result.try(required_positive_limit(fields.limit))
  Ok(
    types.TaskList(types.TaskListQuery(
      states: states,
      limit: limit,
      cursor: fields.cursor,
    )),
  )
}

fn task_show_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case fields.ref {
    Some(ref) ->
      ref_from_fields(ref)
      |> result.map(fn(ref) { types.TaskShow(types.TaskShowQuery(ref: ref)) })
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing task reference"))
  }
}

fn decode_state_categories(
  values: List(String),
) -> Result(List(task.TaskStateCategory), types.QueryError) {
  decode_state_categories_loop(values, [])
}

fn decode_state_categories_loop(
  values: List(String),
  acc: List(task.TaskStateCategory),
) -> Result(List(task.TaskStateCategory), types.QueryError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] ->
      case task.state_category_from_string(value) {
        Ok(category) -> decode_state_categories_loop(rest, [category, ..acc])
        Error(_) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "invalid task state: " <> value,
          ))
      }
  }
}

fn required_positive_limit(
  limit: Option(Int),
) -> Result(Int, types.QueryError) {
  case limit {
    Some(limit) if limit > 0 -> Ok(limit)
    Some(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "task list limit must be positive",
      ))
    None -> Ok(50)
  }
}

fn ref_from_fields(
  ref: TaskRefFields,
) -> Result(types.TaskQueryRef, types.QueryError) {
  case ref.kind {
    Some("display_id") ->
      case ref.value {
        Some(value) -> Ok(types.TaskDisplayId(value))
        None -> missing_ref_value()
      }
    Some("remote_id") ->
      case ref.id {
        Some(id) -> Ok(types.TaskRemoteId(provider: ref.provider, id: id))
        None -> missing_ref_value()
      }
    Some(other) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid task reference kind: " <> other,
      ))
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing task reference kind",
      ))
  }
}

fn missing_ref_value() -> Result(types.TaskQueryRef, types.QueryError) {
  Error(types.QueryError(
    types.QueryBackendFailed,
    "missing task reference value",
  ))
}

fn response_from_fields(
  fields: ResponseFields,
) -> Result(types.QueryResponse, types.QueryError) {
  case require_version(fields.version) {
    Error(error) -> Error(error)
    Ok(Nil) ->
      case fields.ok {
        Some(True) -> decode_success_response(fields)
        Some(False) -> decode_error_response(fields.error)
        None ->
          Error(types.QueryError(types.QueryBackendFailed, "missing ok flag"))
      }
  }
}

fn decode_success_response(
  fields: ResponseFields,
) -> Result(types.QueryResponse, types.QueryError) {
  case fields.type_ {
    Some("status") -> decode_status_response(fields.status)
    Some("metrics") -> decode_metrics_response(fields.metrics)
    Some("task_list") ->
      case fields.task_list {
        Some(task_list) ->
          dto.decode_task_list_dynamic(task_list)
          |> result.map(types.TaskListResponse)
        None -> missing_response_payload()
      }
    Some("task_show") ->
      case fields.task {
        Some(task) ->
          dto.decode_task_detail_dynamic(task)
          |> result.map(types.TaskShowResponse)
        None -> missing_response_payload()
      }
    Some(other) ->
      Error(types.QueryError(
        types.UnsupportedQuery,
        "unsupported query type: " <> other,
      ))
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing query type"))
  }
}

fn decode_status_response(
  status: Option(Dynamic),
) -> Result(types.QueryResponse, types.QueryError) {
  case status {
    Some(status) ->
      dto.decode_status_dynamic(status) |> result.map(types.StatusResponse)
    None -> missing_response_payload()
  }
}

fn decode_metrics_response(
  metrics: Option(Dynamic),
) -> Result(types.QueryResponse, types.QueryError) {
  case metrics {
    Some(metrics) ->
      dto.decode_operational_metrics_dynamic(metrics)
      |> result.map(types.MetricsResponse)
    None -> missing_response_payload()
  }
}

fn missing_response_payload() -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(
    types.QueryBackendFailed,
    "missing query response payload",
  ))
}

fn decode_error_response(
  maybe_error: Option(ErrorFields),
) -> Result(types.QueryResponse, types.QueryError) {
  case maybe_error {
    Some(ErrorFields(code: Some(code), message: Some(message))) ->
      case types.error_code_from_string(code) {
        Ok(parsed) -> Error(types.QueryError(parsed, message))
        Error(_) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "unknown query error code: " <> code,
          ))
      }
    _ ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid query error payload",
      ))
  }
}

fn require_version(
  codec_version: Option(Int),
) -> Result(Nil, types.QueryError) {
  case codec_version {
    Some(value) if value == version -> Ok(Nil)
    Some(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "unsupported query codec version",
      ))
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing query codec version",
      ))
  }
}
