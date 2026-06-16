import birl
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/control/query/types
import scherzo/task
import scherzo/work_item
import scherzo/work_item/action

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
    #("parent", json.nullable(summary.parent, of: work_item_source_to_json)),
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
    #("actions", json.array(summary.actions, of: work_item_action_to_json)),
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
  use parent <- decode.optional_field(
    "parent",
    None,
    decode.optional(work_item_source_decoder()),
  )
  use title <- decode.field("title", decode.string)
  use state <- decode.field("state", work_item_state_decoder())
  use labels <- decode.field("labels", decode.list(work_item_label_decoder()))
  use labels_truncated <- decode.field("labels_truncated", decode.bool)
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))
  use actions <- decode.field(
    "actions",
    decode.list(work_item_action_decoder()),
  )
  decode.success(work_item.WorkItemSummary(
    id: id,
    source: source,
    parent: parent,
    title: title,
    state: state,
    labels: labels,
    labels_truncated: labels_truncated,
    created_at: parse_optional_time(created_at),
    updated_at: parse_optional_time(updated_at),
    actions: actions,
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

fn work_item_action_to_json(action_value: action.WorkItemAction) -> json.Json {
  json.object([
    #("action_id", json.string(action_value.action_id)),
    #("instance_id", json.string(action_value.instance_id)),
    #("label", json.string(action_value.label)),
    #("kind", json.string(action.kind_to_string(action_value.kind))),
    #("enabled", json.bool(action_value.enabled)),
    #(
      "disabled_reason",
      json.nullable(action_value.disabled_reason, of: disabled_reason_to_json),
    ),
    #("fingerprint", json.string(action_value.fingerprint)),
    #("target", target_summary_to_json(action_value.target)),
    #("artifacts", json.array(action_value.artifacts, of: artifact_to_json)),
  ])
}

fn disabled_reason_to_json(reason: action.ActionDisabledReason) -> json.Json {
  json.object([
    #("code", json.string(reason.code)),
    #("message", json.string(reason.message)),
  ])
}

fn target_summary_to_json(target: action.ActionTargetSummary) -> json.Json {
  json.object([
    #("kind", json.string(target.kind)),
    #("provider", json.string(target.provider)),
    #("id", json.string(target.id)),
    #("display_id", json.nullable(target.display_id, of: json.string)),
    #("workflow_id", json.nullable(target.workflow_id, of: json.string)),
    #("run_id", json.nullable(target.run_id, of: json.string)),
  ])
}

fn artifact_to_json(artifact: action.ActionArtifactSummary) -> json.Json {
  json.object([
    #("kind", json.string(artifact.kind)),
    #("ref", json.string(artifact.ref)),
    #("sha256", json.string(artifact.sha256)),
    #("bytes", json.int(artifact.bytes)),
    #("display_path", json.string(artifact.display_path)),
    #("run_id", json.nullable(artifact.run_id, of: json.string)),
    #("step_id", json.nullable(artifact.step_id, of: json.string)),
    #("publication_id", json.nullable(artifact.publication_id, of: json.string)),
  ])
}

fn work_item_action_decoder() -> decode.Decoder(action.WorkItemAction) {
  use action_id <- decode.field("action_id", decode.string)
  use instance_id <- decode.field("instance_id", decode.string)
  use label <- decode.field("label", decode.string)
  use kind <- decode.field("kind", action_kind_decoder())
  use enabled <- decode.field("enabled", decode.bool)
  use disabled_reason <- decode.field(
    "disabled_reason",
    decode.optional(disabled_reason_decoder()),
  )
  use fingerprint <- decode.field("fingerprint", decode.string)
  use target <- decode.field("target", action_target_decoder())
  use artifacts <- decode.field(
    "artifacts",
    decode.list(action_artifact_decoder()),
  )
  decode.success(action.WorkItemAction(
    action_id: action_id,
    instance_id: instance_id,
    label: label,
    kind: kind,
    enabled: enabled,
    disabled_reason: disabled_reason,
    fingerprint: fingerprint,
    target: target,
    artifacts: artifacts,
  ))
}

fn disabled_reason_decoder() -> decode.Decoder(action.ActionDisabledReason) {
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  decode.success(action.ActionDisabledReason(code: code, message: message))
}

fn action_target_decoder() -> decode.Decoder(action.ActionTargetSummary) {
  use kind <- decode.field("kind", decode.string)
  use provider <- decode.field("provider", decode.string)
  use id <- decode.field("id", decode.string)
  use display_id <- decode.field("display_id", decode.optional(decode.string))
  use workflow_id <- decode.field("workflow_id", decode.optional(decode.string))
  use run_id <- decode.field("run_id", decode.optional(decode.string))
  decode.success(action.ActionTargetSummary(
    kind: kind,
    provider: provider,
    id: id,
    display_id: display_id,
    workflow_id: workflow_id,
    run_id: run_id,
  ))
}

fn action_artifact_decoder() -> decode.Decoder(action.ActionArtifactSummary) {
  use kind <- decode.field("kind", decode.string)
  use ref <- decode.field("ref", decode.string)
  use sha256 <- decode.field("sha256", decode.string)
  use bytes <- decode.field("bytes", decode.int)
  use display_path <- decode.field("display_path", decode.string)
  use run_id <- decode.field("run_id", decode.optional(decode.string))
  use step_id <- decode.field("step_id", decode.optional(decode.string))
  use publication_id <- decode.field(
    "publication_id",
    decode.optional(decode.string),
  )
  decode.success(action.ActionArtifactSummary(
    kind: kind,
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    display_path: display_path,
    run_id: run_id,
    step_id: step_id,
    publication_id: publication_id,
  ))
}

fn action_kind_decoder() -> decode.Decoder(action.ActionKind) {
  use value <- decode.then(decode.string)
  case action.kind_from_string(value) {
    Ok(kind) -> decode.success(kind)
    Error(Nil) -> decode.failure(action.ReadOnly, expected: "ActionKind")
  }
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
