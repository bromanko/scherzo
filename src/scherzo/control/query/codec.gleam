import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/control/query/work_item_dto
import scherzo/control/query/workflow_dto
import scherzo/task
import scherzo/work_item

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
    types.WorkItemList(query) ->
      list.append(
        work_item_list_query_entries(query),
        base_request_entries(types.query_type(request)),
      )
    types.WorkItemShow(query) -> [
      #("ref", task_query_ref_to_json(query.ref)),
      ..base_request_entries(types.query_type(request))
    ]
    types.OutboxList(query) ->
      list.append(
        outbox_list_query_entries(query),
        base_request_entries(types.query_type(request)),
      )
    types.OutboxShow(query) -> [
      #("outbox_id", json.string(query.outbox_id)),
      ..base_request_entries(types.query_type(request))
    ]
    types.OperationStatus(query) -> [
      #("operation_id", json.string(query.operation_id)),
      ..base_request_entries(types.query_type(request))
    ]
    types.WorkflowList -> base_request_entries(types.query_type(request))
    types.WorkflowDetail(query) -> [
      #("workflow_id", json.string(query.workflow_id)),
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

fn work_item_list_query_entries(
  query: types.WorkItemListQuery,
) -> List(#(String, json.Json)) {
  let state_entries = case query.state_filter {
    work_item.CategoryWorkItems(states) -> [
      #("states", json.array(states, of: task_state_category_to_json)),
    ]
    _ -> []
  }
  [
    #(
      "state_filter",
      json.string(work_item.state_filter_to_string(query.state_filter)),
    ),
    #("search", json.nullable(query.search, of: json.string)),
    #("sort", json.string(work_item.sort_to_string(query.sort))),
    #("limit", json.int(query.limit)),
    #("cursor", json.nullable(query.cursor, of: json.string)),
    ..state_entries
  ]
}

fn outbox_list_query_entries(
  query: types.OutboxListQuery,
) -> List(#(String, json.Json)) {
  [
    #("statuses", json.array(query.statuses, of: outbox_status_to_json)),
    #("kinds", json.array(query.kinds, of: json.string)),
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

fn outbox_status_to_json(status: types.OutboxRecordStatus) -> json.Json {
  status |> types.outbox_status_to_string |> json.string
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
    types.WorkItemListResponse(work_item_list) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #(
          "work_item_list",
          work_item_dto.work_item_list_to_json(work_item_list),
        ),
      ])
    types.WorkItemShowResponse(work_item) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("work_item", work_item_dto.work_item_detail_to_json(work_item)),
      ])
    types.OutboxListResponse(outbox) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("outbox", dto.outbox_list_to_json(outbox)),
      ])
    types.OutboxShowResponse(outbox) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("outbox_record", dto.outbox_record_to_json(outbox)),
      ])
    types.OperationStatusResponse(operation) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("operation", dto.operation_status_to_json(operation)),
      ])
    types.WorkflowListResponse(workflows) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("workflow_list", workflow_dto.workflow_list_to_json(workflows)),
      ])
    types.WorkflowDetailResponse(workflow) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("workflow", workflow_dto.workflow_detail_to_json(workflow)),
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
    state_filter: Option(String),
    search: Option(String),
    sort: Option(String),
    states: List(String),
    statuses: List(String),
    kinds: List(String),
    ref: Option(TaskRefFields),
    outbox_id: Option(String),
    operation_id: Option(String),
    workflow_id: Option(String),
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
    work_item_list: Option(Dynamic),
    work_item: Option(Dynamic),
    outbox: Option(Dynamic),
    outbox_record: Option(Dynamic),
    operation: Option(Dynamic),
    workflow_list: Option(Dynamic),
    workflow: Option(Dynamic),
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
  use state_filter <- decode.optional_field(
    "state_filter",
    None,
    decode.optional(decode.string),
  )
  use search <- decode.optional_field(
    "search",
    None,
    decode.optional(decode.string),
  )
  use sort <- decode.optional_field(
    "sort",
    None,
    decode.optional(decode.string),
  )
  use states <- decode.optional_field("states", [], decode.list(decode.string))
  use statuses <- decode.optional_field(
    "statuses",
    [],
    decode.list(decode.string),
  )
  use kinds <- decode.optional_field("kinds", [], decode.list(decode.string))
  use ref <- decode.optional_field(
    "ref",
    None,
    decode.optional(task_ref_fields_decoder()),
  )
  use outbox_id <- decode.optional_field(
    "outbox_id",
    None,
    decode.optional(decode.string),
  )
  use operation_id <- decode.optional_field(
    "operation_id",
    None,
    decode.optional(decode.string),
  )
  use workflow_id <- decode.optional_field(
    "workflow_id",
    None,
    decode.optional(decode.string),
  )
  decode.success(RequestFields(
    version: version,
    type_: type_,
    limit: limit,
    cursor: cursor,
    state_filter: state_filter,
    search: search,
    sort: sort,
    states: states,
    statuses: statuses,
    kinds: kinds,
    ref: ref,
    outbox_id: outbox_id,
    operation_id: operation_id,
    workflow_id: workflow_id,
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
  use work_item_list <- decode.optional_field(
    "work_item_list",
    None,
    decode.optional(decode.dynamic),
  )
  use work_item <- decode.optional_field(
    "work_item",
    None,
    decode.optional(decode.dynamic),
  )
  use outbox <- decode.optional_field(
    "outbox",
    None,
    decode.optional(decode.dynamic),
  )
  use outbox_record <- decode.optional_field(
    "outbox_record",
    None,
    decode.optional(decode.dynamic),
  )
  use operation <- decode.optional_field(
    "operation",
    None,
    decode.optional(decode.dynamic),
  )
  use workflow_list <- decode.optional_field(
    "workflow_list",
    None,
    decode.optional(decode.dynamic),
  )
  use workflow <- decode.optional_field(
    "workflow",
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
    work_item_list: work_item_list,
    work_item: work_item,
    outbox: outbox,
    outbox_record: outbox_record,
    operation: operation,
    workflow_list: workflow_list,
    workflow: workflow,
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
        Some("work_item_list") -> work_item_list_request_from_fields(fields)
        Some("work_item_show") -> work_item_show_request_from_fields(fields)
        Some("outbox_list") -> outbox_list_request_from_fields(fields)
        Some("outbox_show") -> outbox_show_request_from_fields(fields)
        Some("operation_status") -> operation_status_request_from_fields(fields)
        Some("workflow_list") -> Ok(types.WorkflowList)
        Some("workflow_detail") -> workflow_detail_request_from_fields(fields)
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

fn work_item_list_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  use state_filter <- result.try(decode_work_item_state_filter(
    fields.state_filter,
    fields.states,
  ))
  use limit <- result.try(required_positive_limit_named(
    fields.limit,
    "work item list",
  ))
  use sort <- result.try(decode_work_item_sort(fields.sort))
  let search = work_item.normalize_search(fields.search)
  Ok(
    types.WorkItemList(types.WorkItemListQuery(
      state_filter: state_filter,
      search: search,
      sort: sort,
      limit: limit,
      cursor: fields.cursor,
    )),
  )
}

fn work_item_show_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case fields.ref {
    Some(ref) ->
      ref_from_fields(ref)
      |> result.map(fn(ref) {
        types.WorkItemShow(types.WorkItemShowQuery(ref: ref))
      })
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing work item reference",
      ))
  }
}

fn outbox_list_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  use statuses <- result.try(decode_outbox_statuses(fields.statuses))
  use limit <- result.try(required_positive_limit_named(
    fields.limit,
    "outbox list",
  ))
  Ok(
    types.OutboxList(types.OutboxListQuery(
      statuses: statuses,
      kinds: fields.kinds,
      limit: limit,
      cursor: fields.cursor,
    )),
  )
}

fn outbox_show_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case fields.outbox_id {
    Some(outbox_id) ->
      Ok(types.OutboxShow(types.OutboxShowQuery(outbox_id: outbox_id)))
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing outbox id"))
  }
}

fn operation_status_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case fields.operation_id {
    Some(operation_id) ->
      Ok(
        types.OperationStatus(types.OperationStatusQuery(
          operation_id: operation_id,
        )),
      )
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing operation id"))
  }
}

fn workflow_detail_request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case fields.workflow_id {
    Some(workflow_id) ->
      Ok(
        types.WorkflowDetail(types.WorkflowDetailQuery(workflow_id: workflow_id)),
      )
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing workflow id"))
  }
}

fn decode_work_item_state_filter(
  state_filter: Option(String),
  states: List(String),
) -> Result(work_item.WorkItemStateFilter, types.QueryError) {
  case state_filter {
    Some(state_filter) ->
      case state_filter |> string.trim |> string.lowercase {
        "active" -> Ok(work_item.ActiveWorkItems)
        "archive" -> Ok(work_item.ArchiveWorkItems)
        "categories" -> {
          use categories <- result.try(decode_state_categories_named(
            states,
            "state_filter categories",
          ))
          case categories {
            [] ->
              Error(types.QueryError(
                types.QueryBackendFailed,
                "work item state_filter categories requires non-empty states",
              ))
            categories -> Ok(work_item.CategoryWorkItems(categories))
          }
        }
        other ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "invalid work item state_filter: " <> other,
          ))
      }
    None ->
      case states {
        [] -> Ok(work_item.default_state_filter())
        _ ->
          decode_state_categories_named(states, "work item state")
          |> result.map(work_item.CategoryWorkItems)
      }
  }
}

fn decode_work_item_sort(
  sort: Option(String),
) -> Result(work_item.WorkItemSort, types.QueryError) {
  case sort {
    Some(sort) ->
      case work_item.sort_from_string(sort) {
        Ok(sort) -> Ok(sort)
        Error(Nil) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "invalid work item sort: " <> sort,
          ))
      }
    None -> Ok(work_item.default_sort())
  }
}

fn decode_state_categories(
  values: List(String),
) -> Result(List(task.TaskStateCategory), types.QueryError) {
  decode_state_categories_named(values, "task state")
}

fn decode_state_categories_named(
  values: List(String),
  label: String,
) -> Result(List(task.TaskStateCategory), types.QueryError) {
  decode_state_categories_loop(values, [], label)
}

fn decode_state_categories_loop(
  values: List(String),
  acc: List(task.TaskStateCategory),
  label: String,
) -> Result(List(task.TaskStateCategory), types.QueryError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] ->
      case task.state_category_from_string(value) {
        Ok(category) ->
          decode_state_categories_loop(rest, [category, ..acc], label)
        Error(_) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "invalid " <> label <> ": " <> value,
          ))
      }
  }
}

fn decode_outbox_statuses(
  values: List(String),
) -> Result(List(types.OutboxRecordStatus), types.QueryError) {
  decode_outbox_statuses_loop(values, [])
}

fn decode_outbox_statuses_loop(
  values: List(String),
  acc: List(types.OutboxRecordStatus),
) -> Result(List(types.OutboxRecordStatus), types.QueryError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] ->
      case types.outbox_status_from_string(value) {
        Ok(status) -> decode_outbox_statuses_loop(rest, [status, ..acc])
        Error(_) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "invalid outbox status: " <> value,
          ))
      }
  }
}

fn required_positive_limit(
  limit: Option(Int),
) -> Result(Int, types.QueryError) {
  required_positive_limit_named(limit, "task list")
}

fn required_positive_limit_named(
  limit: Option(Int),
  label: String,
) -> Result(Int, types.QueryError) {
  case limit {
    Some(limit) if limit > 0 -> Ok(limit)
    Some(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        label <> " limit must be positive",
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
    Some("work_item_list") ->
      case fields.work_item_list {
        Some(work_item_list) ->
          work_item_dto.decode_work_item_page_dynamic(work_item_list)
          |> result.map(types.WorkItemListResponse)
        None -> missing_response_payload()
      }
    Some("work_item_show") ->
      case fields.work_item {
        Some(work_item) ->
          work_item_dto.decode_work_item_detail_dynamic(work_item)
          |> result.map(types.WorkItemShowResponse)
        None -> missing_response_payload()
      }
    Some("outbox_list") ->
      case fields.outbox {
        Some(outbox) ->
          dto.decode_outbox_list_dynamic(outbox)
          |> result.map(types.OutboxListResponse)
        None -> missing_response_payload()
      }
    Some("outbox_show") ->
      case fields.outbox_record {
        Some(outbox_record) ->
          dto.decode_outbox_record_dynamic(outbox_record)
          |> result.map(types.OutboxShowResponse)
        None -> missing_response_payload()
      }
    Some("operation_status") ->
      case fields.operation {
        Some(operation) ->
          dto.decode_operation_status_dynamic(operation)
          |> result.map(types.OperationStatusResponse)
        None -> missing_response_payload()
      }
    Some("workflow_list") ->
      case fields.workflow_list {
        Some(workflow_list) ->
          workflow_dto.decode_workflow_list_dynamic(workflow_list)
          |> result.map(types.WorkflowListResponse)
        None -> missing_response_payload()
      }
    Some("workflow_detail") ->
      case fields.workflow {
        Some(workflow) ->
          workflow_dto.decode_workflow_detail_dynamic(workflow)
          |> result.map(types.WorkflowDetailResponse)
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
