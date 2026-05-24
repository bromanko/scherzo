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

pub fn remote_command_fetch_to_json(
  fetch: types.RemoteCommandFetchPayload,
) -> json.Json {
  let types.RemoteCommandFetchPayload(
    task_refs: task_refs,
    since_event_ids: since_event_ids,
    limit_per_task: limit_per_task,
  ) = fetch
  json.object([
    #("task_refs", json.array(task_refs, of: task_ref_to_json)),
    #("since_event_ids", json.array(since_event_ids, of: json.string)),
    #("limit_per_task", json.int(limit_per_task)),
  ])
}

pub fn remote_command_fetch_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  use task_refs <- decode.field("task_refs", decode.list(task_ref_decoder()))
  use since_event_ids <- decode.optional_field(
    "since_event_ids",
    [],
    decode.list(decode.string),
  )
  use limit_per_task <- decode.field("limit_per_task", decode.int)
  decode.success(
    types.RemoteCommandsFetchPayload(fetch: types.RemoteCommandFetchPayload(
      task_refs: task_refs,
      since_event_ids: since_event_ids,
      limit_per_task: limit_per_task,
    )),
  )
}

pub fn remote_command_event_to_json(
  event: types.RemoteCommandEventPayload,
) -> json.Json {
  let types.RemoteCommandEventPayload(
    event_id: event_id,
    task: task,
    author_id: author_id,
    body: body,
    command_name: command_name,
    excerpt: excerpt,
    observed_at_ms: observed_at_ms,
  ) = event
  json.object([
    #("event_id", json.string(event_id)),
    #("task", task_ref_to_json(task)),
    #("author_id", json.string(author_id)),
    #("body", json.string(body)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
    #("observed_at_ms", json.int(observed_at_ms)),
  ])
}

pub fn remote_command_event_decoder() -> decode.Decoder(
  types.RemoteCommandEventPayload,
) {
  use event_id <- decode.field("event_id", decode.string)
  use task <- decode.field("task", task_ref_decoder())
  use author_id <- decode.field("author_id", decode.string)
  use body <- decode.field("body", decode.string)
  use command_name <- decode.field("command_name", decode.string)
  use excerpt <- decode.field("excerpt", decode.string)
  use observed_at_ms <- decode.field("observed_at_ms", decode.int)
  decode.success(types.RemoteCommandEventPayload(
    event_id: event_id,
    task: task,
    author_id: author_id,
    body: body,
    command_name: command_name,
    excerpt: excerpt,
    observed_at_ms: observed_at_ms,
  ))
}

pub fn remote_command_ack_to_json(
  ack: types.RemoteCommandAckPayload,
) -> json.Json {
  let types.RemoteCommandAckPayload(event: event, body: body) = ack
  json.object([
    #("event", remote_command_event_to_json(event)),
    #("body", json.string(body)),
  ])
}

pub fn remote_command_ack_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  use event <- decode.field("event", remote_command_event_decoder())
  use body <- decode.field("body", decode.string)
  decode.success(
    types.RemoteCommandsPostAckPayload(ack: types.RemoteCommandAckPayload(
      event: event,
      body: body,
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

pub fn handoff_event_to_json(event: types.HandoffEventPayload) -> json.Json {
  case event {
    types.HandoffClaimEvent(
      task: task,
      workspace_path: workspace_path,
      run_id: run_id,
    ) ->
      json.object([
        #("kind", json.string("claim")),
        #("task", task_ref_to_json(task)),
        #("workspace_path", json.string(workspace_path)),
        #("run_id", json.string(run_id)),
      ])
    types.HandoffSuccessEvent(task: task, run_id: run_id, summary: summary) ->
      json.object([
        #("kind", json.string("success")),
        #("task", task_ref_to_json(task)),
        #("run_id", json.string(run_id)),
        #("summary", json.string(summary)),
      ])
    types.HandoffFailureEvent(task: task, run_id: run_id, reason: reason) ->
      json.object([
        #("kind", json.string("failure")),
        #("task", task_ref_to_json(task)),
        #("run_id", json.string(run_id)),
        #("reason", json.string(reason)),
      ])
    types.HandoffParkEvent(
      task: task,
      reason: reason,
      release_policy: release_policy,
    ) ->
      json.object([
        #("kind", json.string("park")),
        #("task", task_ref_to_json(task)),
        #("reason", json.string(reason)),
        #("release_policy", json.string(release_policy)),
      ])
    types.LegacyHandoffClaimEvent(
      issue_identifier: issue_identifier,
      workspace_path: workspace_path,
      run_id: run_id,
    ) ->
      json.object([
        #("kind", json.string("legacy_claim")),
        #("issue_identifier", json.string(issue_identifier)),
        #("workspace_path", json.string(workspace_path)),
        #("run_id", json.string(run_id)),
      ])
    types.LegacyHandoffSuccessEvent(
      issue_identifier: issue_identifier,
      success: success,
      run_id: run_id,
      workflow_id: workflow_id,
    ) ->
      json.object([
        #("kind", json.string("legacy_success")),
        #("issue_identifier", json.string(issue_identifier)),
        #("success", json.string(success)),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
      ])
    types.LegacyHandoffFailureEvent(
      issue_identifier: issue_identifier,
      failure: failure,
      run_id: run_id,
      workflow_id: workflow_id,
    ) ->
      json.object([
        #("kind", json.string("legacy_failure")),
        #("issue_identifier", json.string(issue_identifier)),
        #("failure", json.string(failure)),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
      ])
    types.LegacyHandoffParkEvent(
      task: task,
      issue_identifier: issue_identifier,
      reason: reason,
      release_policy: release_policy,
      run_id: run_id,
    ) ->
      json.object([
        #("kind", json.string("legacy_park")),
        #("task", task_ref_to_json(task)),
        #("issue_identifier", json.string(issue_identifier)),
        #("reason", json.string(reason)),
        #("release_policy", option_json(release_policy, json.string)),
        #("run_id", option_json(run_id, json.string)),
      ])
  }
}

pub fn handoff_report_payload_decoder() -> decode.Decoder(types.RequestPayload) {
  handoff_event_decoder()
  |> decode.map(fn(event) { types.HandoffReportPayload(event: event) })
}

pub fn handoff_event_decoder() -> decode.Decoder(types.HandoffEventPayload) {
  use kind <- decode.field("kind", decode.string)
  case string.trim(kind) {
    "claim" -> {
      use task <- decode.field("task", task_ref_decoder())
      use workspace_path <- decode.field("workspace_path", decode.string)
      use run_id <- decode.field("run_id", decode.string)
      decode.success(types.HandoffClaimEvent(
        task: task,
        workspace_path: workspace_path,
        run_id: run_id,
      ))
    }
    "success" -> {
      use task <- decode.field("task", task_ref_decoder())
      use run_id <- decode.field("run_id", decode.string)
      use summary <- decode.field("summary", decode.string)
      decode.success(types.HandoffSuccessEvent(
        task: task,
        run_id: run_id,
        summary: summary,
      ))
    }
    "failure" -> {
      use task <- decode.field("task", task_ref_decoder())
      use run_id <- decode.field("run_id", decode.string)
      use reason <- decode.field("reason", decode.string)
      decode.success(types.HandoffFailureEvent(
        task: task,
        run_id: run_id,
        reason: reason,
      ))
    }
    "park" -> {
      use task <- decode.field("task", task_ref_decoder())
      use reason <- decode.field("reason", decode.string)
      use release_policy <- decode.field("release_policy", decode.string)
      decode.success(types.HandoffParkEvent(
        task: task,
        reason: reason,
        release_policy: release_policy,
      ))
    }
    "legacy_claim" -> {
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use run_id <- decode.field("run_id", decode.string)
      decode.success(types.LegacyHandoffClaimEvent(
        issue_identifier: issue_identifier,
        workspace_path: workspace_path,
        run_id: run_id,
      ))
    }
    "legacy_success" -> {
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use success <- decode.field("success", decode.string)
      use run_id <- decode.field("run_id", decode.string)
      use workflow_id <- decode.field("workflow_id", decode.string)
      decode.success(types.LegacyHandoffSuccessEvent(
        issue_identifier: issue_identifier,
        success: success,
        run_id: run_id,
        workflow_id: workflow_id,
      ))
    }
    "legacy_failure" -> {
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use failure <- decode.field("failure", decode.string)
      use run_id <- decode.field("run_id", decode.string)
      use workflow_id <- decode.field("workflow_id", decode.string)
      decode.success(types.LegacyHandoffFailureEvent(
        issue_identifier: issue_identifier,
        failure: failure,
        run_id: run_id,
        workflow_id: workflow_id,
      ))
    }
    "legacy_park" -> {
      use task <- decode.field("task", task_ref_decoder())
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use reason <- decode.field("reason", decode.string)
      use release_policy <- decode.optional_field(
        "release_policy",
        None,
        decode.optional(decode.string),
      )
      use run_id <- decode.optional_field(
        "run_id",
        None,
        decode.optional(decode.string),
      )
      decode.success(types.LegacyHandoffParkEvent(
        task: task,
        issue_identifier: issue_identifier,
        reason: reason,
        release_policy: release_policy,
        run_id: run_id,
      ))
    }
    _ ->
      decode.failure(
        types.HandoffClaimEvent(
          task: task.TaskRef(
            backend_kind: "",
            remote_id: "",
            key: None,
            url: None,
          ),
          workspace_path: "",
          run_id: "",
        ),
        expected: "handoff event kind",
      )
  }
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

pub fn handoff_report_receipt_to_json(
  receipt: types.HandoffReportReceiptPayload,
) -> json.Json {
  let types.HandoffReportReceiptPayload(reported: reported) = receipt
  json.object([#("reported", json.bool(reported))])
}

pub fn handoff_report_receipt_decoder() -> decode.Decoder(
  types.HandoffReportReceiptPayload,
) {
  use reported <- decode.field("reported", decode.bool)
  decode.success(types.HandoffReportReceiptPayload(reported: reported))
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
