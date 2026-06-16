import birl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/control/query/types
import scherzo/task
import scherzo/work_item

pub fn work_item_list_to_json(page: work_item.WorkItemPage) -> json.Json {
  json.object([
    #("items", json.array(page.items, of: work_item_summary_to_json)),
    #(
      "page",
      page_to_json(types.PageDto(
        next_cursor: page.next_cursor,
        has_more: page.has_more,
      )),
    ),
  ])
}

pub fn work_item_detail_to_json(detail: work_item.WorkItemDetail) -> json.Json {
  json.object([
    #("summary", work_item_summary_to_json(detail.summary)),
    #("subtasks", json.array(detail.subtasks, of: work_item_summary_to_json)),
    #("subtasks_truncated", json.bool(detail.subtasks_truncated)),
  ])
}

pub fn decode_work_item_page_dynamic(
  value: Dynamic,
) -> Result(work_item.WorkItemPage, types.QueryError) {
  case decode.run(value, work_item_page_decoder()) {
    Ok(page) -> Ok(page)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid work item list query payload",
      ))
  }
}

pub fn decode_work_item_detail_dynamic(
  value: Dynamic,
) -> Result(work_item.WorkItemDetail, types.QueryError) {
  case decode.run(value, work_item_detail_decoder()) {
    Ok(detail) -> Ok(detail)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid work item detail query payload",
      ))
  }
}

fn work_item_summary_to_json(summary: work_item.WorkItemSummary) -> json.Json {
  json.object([
    #("id", json.string(summary.id)),
    #("source", work_item_source_to_json(summary.source)),
    #("title", json.string(summary.title)),
    #("state", work_item_state_to_json(summary.state)),
    #("labels", json.array(summary.labels, of: label_to_json)),
    #("labels_truncated", json.bool(summary.labels_truncated)),
    #(
      "created_at",
      json.nullable(
        option_map(summary.created_at, birl.to_iso8601),
        of: json.string,
      ),
    ),
    #(
      "updated_at",
      json.nullable(
        option_map(summary.updated_at, birl.to_iso8601),
        of: json.string,
      ),
    ),
  ])
}

fn work_item_source_to_json(source: work_item.WorkItemSource) -> json.Json {
  json.object([
    #("provider", json.string(source.provider)),
    #("id", json.string(source.id)),
    #("display_id", json.nullable(source.display_id, of: json.string)),
    #("url", json.nullable(source.url, of: json.string)),
  ])
}

fn work_item_state_to_json(state: task.TaskState) -> json.Json {
  json.object([
    #("id", json.nullable(state.id, of: json.string)),
    #("name", json.string(state.name)),
    #("category", json.string(task.state_category_to_string(state.category))),
  ])
}

fn label_to_json(label: task.TaskLabel) -> json.Json {
  json.object([
    #("id", json.nullable(label.id, of: json.string)),
    #("name", json.string(label.name)),
  ])
}

fn page_to_json(page: types.PageDto) -> json.Json {
  json.object([
    #("next_cursor", json.nullable(page.next_cursor, of: json.string)),
    #("has_more", json.bool(page.has_more)),
  ])
}

fn work_item_page_decoder() -> decode.Decoder(work_item.WorkItemPage) {
  use items <- decode.field("items", decode.list(work_item_summary_decoder()))
  use page <- decode.field("page", page_decoder())
  decode.success(work_item.WorkItemPage(
    items: items,
    next_cursor: page.next_cursor,
    has_more: page.has_more,
  ))
}

fn work_item_detail_decoder() -> decode.Decoder(work_item.WorkItemDetail) {
  use summary <- decode.field("summary", work_item_summary_decoder())
  use subtasks <- decode.field(
    "subtasks",
    decode.list(work_item_summary_decoder()),
  )
  use subtasks_truncated <- decode.field("subtasks_truncated", decode.bool)
  decode.success(work_item.WorkItemDetail(
    summary: summary,
    subtasks: subtasks,
    subtasks_truncated: subtasks_truncated,
  ))
}

fn work_item_summary_decoder() -> decode.Decoder(work_item.WorkItemSummary) {
  use id <- decode.field("id", decode.string)
  use source <- decode.field("source", work_item_source_decoder())
  use title <- decode.field("title", decode.string)
  use state <- decode.field("state", work_item_state_decoder())
  use labels <- decode.field("labels", decode.list(work_item_label_decoder()))
  use labels_truncated <- decode.field("labels_truncated", decode.bool)
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))
  decode.success(work_item.WorkItemSummary(
    id: id,
    source: source,
    title: title,
    state: state,
    labels: labels,
    labels_truncated: labels_truncated,
    created_at: parse_optional_time(created_at),
    updated_at: parse_optional_time(updated_at),
  ))
}

fn work_item_source_decoder() -> decode.Decoder(work_item.WorkItemSource) {
  use provider <- decode.field("provider", decode.string)
  use id <- decode.field("id", decode.string)
  use display_id <- decode.field("display_id", decode.optional(decode.string))
  use url <- decode.field("url", decode.optional(decode.string))
  decode.success(work_item.WorkItemSource(
    provider: provider,
    id: id,
    display_id: display_id,
    url: url,
  ))
}

fn work_item_state_decoder() -> decode.Decoder(task.TaskState) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  use category <- decode.field("category", state_category_decoder())
  decode.success(task.TaskState(id: id, name: name, category: category))
}

fn work_item_label_decoder() -> decode.Decoder(task.TaskLabel) {
  use id <- decode.field("id", decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  decode.success(task.TaskLabel(id: id, name: name))
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

fn parse_optional_time(value: Option(String)) -> Option(birl.Time) {
  case value {
    Some(value) ->
      case birl.parse(value) {
        Ok(time) -> Some(time)
        Error(_) -> None
      }
    None -> None
  }
}
