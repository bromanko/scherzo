import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/conformance/types

pub fn comment_request_to_json(
  comment: types.CommentRequestPayload,
) -> json.Json {
  let types.CommentRequestPayload(task: task, body: body, mode: mode) = comment
  json.object([
    #("task", task_ref_to_json(task)),
    #("body", json.string(body)),
    #("mode", comment_write_mode_to_json(mode)),
  ])
}

pub fn comment_request_payload_decoder() -> decode.Decoder(types.RequestPayload) {
  use task <- decode.field("task", task_ref_decoder())
  use body <- decode.field("body", decode.string)
  use mode <- decode.field("mode", comment_write_mode_decoder())
  decode.success(
    types.CommentsPostOrUpdatePayload(comment: types.CommentRequestPayload(
      task: task,
      body: body,
      mode: mode,
    )),
  )
}

pub fn state_transition_request_to_json(
  transition: types.StateTransitionRequestPayload,
) -> json.Json {
  let types.StateTransitionRequestPayload(
    task: task,
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    reason: reason,
  ) = transition
  json.object([
    #("task", task_ref_to_json(task)),
    #("target_state_id", option_json(target_state_id, json.string)),
    #("target_state_name", json.string(target_state_name)),
    #("reason", json.string(reason)),
  ])
}

pub fn state_transition_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  use task <- decode.field("task", task_ref_decoder())
  use target_state_id <- decode.optional_field(
    "target_state_id",
    None,
    decode.optional(decode.string),
  )
  use target_state_name <- decode.field("target_state_name", decode.string)
  use reason <- decode.field("reason", decode.string)
  decode.success(
    types.StateTransitionPayload(
      transition: types.StateTransitionRequestPayload(
        task: task,
        target_state_id: target_state_id,
        target_state_name: target_state_name,
        reason: reason,
      ),
    ),
  )
}

pub fn comment_receipt_to_json(
  comment: types.CommentReceiptPayload,
) -> json.Json {
  let types.CommentReceiptPayload(
    id: id,
    task: task,
    url: url,
    created: created,
  ) = comment
  json.object([
    #("id", json.string(id)),
    #("task", task_ref_to_json(task)),
    #("url", option_json(url, json.string)),
    #("created", json.bool(created)),
  ])
}

pub fn comment_receipt_decoder() -> decode.Decoder(types.CommentReceiptPayload) {
  use id <- decode.field("id", decode.string)
  use task <- decode.field("task", task_ref_decoder())
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  use created <- decode.field("created", decode.bool)
  decode.success(types.CommentReceiptPayload(
    id: id,
    task: task,
    url: url,
    created: created,
  ))
}

pub fn state_transition_receipt_to_json(
  transition: types.StateTransitionReceiptPayload,
) -> json.Json {
  let types.StateTransitionReceiptPayload(task: task, state: state) = transition
  json.object([
    #("task", task_ref_to_json(task)),
    #("state", task_state_to_json(state)),
  ])
}

pub fn state_transition_receipt_decoder() -> decode.Decoder(
  types.StateTransitionReceiptPayload,
) {
  use task <- decode.field("task", task_ref_decoder())
  use state <- decode.field("state", task_state_decoder())
  decode.success(types.StateTransitionReceiptPayload(task: task, state: state))
}

fn comment_write_mode_to_json(mode: types.CommentWriteMode) -> json.Json {
  case mode {
    types.CreateOnlyComment ->
      json.object([#("kind", json.string("create_only"))])
    types.UpdateExistingComment(
      comment_id: comment_id,
      allow_create_fallback: allow_create_fallback,
    ) ->
      json.object([
        #("kind", json.string("update_existing")),
        #("comment_id", json.string(comment_id)),
        #("allow_create_fallback", json.bool(allow_create_fallback)),
      ])
  }
}

fn comment_write_mode_decoder() -> decode.Decoder(types.CommentWriteMode) {
  use kind <- decode.field("kind", decode.string)
  case string.trim(kind) {
    "create_only" -> decode.success(types.CreateOnlyComment)
    "update_existing" -> {
      use comment_id <- decode.field("comment_id", decode.string)
      use allow_create_fallback <- decode.optional_field(
        "allow_create_fallback",
        False,
        decode.bool,
      )
      decode.success(types.UpdateExistingComment(
        comment_id: comment_id,
        allow_create_fallback: allow_create_fallback,
      ))
    }
    _ -> decode.failure(types.CreateOnlyComment, expected: "comment write mode")
  }
}

fn task_ref_to_json(ref: task.TaskRef) -> json.Json {
  let task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ) = ref
  json.object([
    #("backend_kind", json.string(backend_kind)),
    #("remote_id", json.string(remote_id)),
    #("key", option_json(key, json.string)),
    #("url", option_json(url, json.string)),
  ])
}

fn task_ref_decoder() -> decode.Decoder(task.TaskRef) {
  use backend_kind <- decode.field("backend_kind", decode.string)
  use remote_id <- decode.field("remote_id", decode.string)
  use key <- decode.optional_field("key", None, decode.optional(decode.string))
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  decode.success(task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ))
}

fn task_state_to_json(state: task.TaskState) -> json.Json {
  let task.TaskState(id: id, name: name, category: category) = state
  json.object([
    #("id", option_json(id, json.string)),
    #("name", json.string(name)),
    #("category", json.string(task_state_category_to_string(category))),
  ])
}

fn task_state_decoder() -> decode.Decoder(task.TaskState) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  use category <- decode.field("category", task_state_category_decoder())
  decode.success(task.TaskState(id: id, name: name, category: category))
}

fn task_state_category_to_string(category: task.TaskStateCategory) -> String {
  case category {
    task.Backlog -> "backlog"
    task.Ready -> "ready"
    task.Active -> "active"
    task.Done -> "done"
    task.Canceled -> "canceled"
    task.Duplicate -> "duplicate"
    task.Unknown -> "unknown"
  }
}

fn task_state_category_decoder() -> decode.Decoder(task.TaskStateCategory) {
  use value <- decode.then(decode.string)
  case string.trim(value) {
    "backlog" -> decode.success(task.Backlog)
    "ready" -> decode.success(task.Ready)
    "active" -> decode.success(task.Active)
    "done" -> decode.success(task.Done)
    "canceled" -> decode.success(task.Canceled)
    "duplicate" -> decode.success(task.Duplicate)
    "unknown" -> decode.success(task.Unknown)
    _ -> decode.failure(task.Unknown, expected: "task state category")
  }
}

fn option_json(value: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case value {
    Some(inner) -> encoder(inner)
    None -> json.null()
  }
}
