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
