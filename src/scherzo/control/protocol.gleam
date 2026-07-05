import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types
import scherzo/session/event
import scherzo/session/json as session_json
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

pub const version = 1

pub type Request {
  Ping(id: String, token: String)
  ListSessions(id: String, token: String)
  GetSession(id: String, token: String, session_id: String)
  GetEvents(
    id: String,
    token: String,
    session_id: String,
    after: Int,
    limit: Int,
  )
  StreamEvents(id: String, token: String, session_id: String, after: Int)
  Query(id: String, token: String, query: query_types.QueryRequest)
  Pause(id: String, token: String)
  Resume(id: String, token: String)
  ReloadWorkflow(id: String, token: String)
  RetryIssue(id: String, token: String, issue_ref: command.IssueRef)
  RetryIssueStartFresh(
    id: String,
    token: String,
    issue_ref: command.IssueRef,
    reason: String,
  )
  RetryWorkflowStep(
    id: String,
    token: String,
    target: command.RetryWorkflowStepTarget,
    step_id: Option(String),
  )
  RetryWorkflowStepExact(
    id: String,
    token: String,
    target: command.RetryWorkflowStepTarget,
    step_id: Option(String),
  )
  RecollectWorkflowOutputs(id: String, token: String, run_id: String)
  RunFinalize(
    id: String,
    token: String,
    run_id: String,
    validate: Bool,
    outputs: command.RunFinalizeOutputs,
    publish: Bool,
    update_tracker: Bool,
    dry_run: Bool,
    reason: String,
    allow_unpublished: Bool,
  )
  RetryArtifactPublication(
    id: String,
    token: String,
    run_id: String,
    publication_id: Option(String),
  )
  ParkIssue(
    id: String,
    token: String,
    issue_ref: command.IssueRef,
    reason: String,
  )
  UnparkIssue(id: String, token: String, issue_ref: command.IssueRef)
  AbortSession(id: String, token: String, session_id: String)
  StopAfterCurrentTurn(id: String, token: String, session_id: String)
  CleanupOrphanSteps(id: String, token: String, run_id: String, dry_run: Bool)
  PromptSession(id: String, token: String, session_id: String, message: String)
  RespondUi(
    id: String,
    token: String,
    session_id: String,
    request_id: String,
    response: command.UiResponse,
  )
  RunScheduleNow(id: String, token: String, job_id: String)
  ReenableSchedule(id: String, token: String, job_id: String)
  WorkItemAction(
    id: String,
    token: String,
    request: command.WorkItemActionRequest,
  )
}

pub type ErrorBody {
  ErrorBody(code: String, message: String)
}

pub type Response {
  Response(
    id: String,
    ok: Bool,
    data: Option(json.Json),
    error: Option(ErrorBody),
  )
}

pub type RequestError {
  RequestError(id: String, code: String, message: String)
}

type RequestFields {
  RequestFields(
    version: Int,
    id: String,
    token: String,
    type_: String,
    session_id: Option(String),
    query: Option(Dynamic),
    after: Int,
    limit: Int,
    issue_id: Option(String),
    issue_identifier: Option(String),
    target: Option(String),
    run_id: Option(String),
    step_id: Option(String),
    publication_id: Option(String),
    reason: Option(String),
    message: Option(String),
    request_id: Option(String),
    cancel: Option(Bool),
    value: Option(String),
    job_id: Option(String),
    dry_run: Option(Bool),
    validate: Option(Bool),
    outputs: Option(String),
    publish: Option(Bool),
    update_tracker: Option(Bool),
    allow_unpublished: Option(Bool),
    action_id: Option(String),
    action_instance_id: Option(String),
    target_kind: Option(String),
    target_provider: Option(String),
    target_id: Option(String),
    observed_fingerprint: Option(String),
    idempotency_key: Option(String),
    params: Option(Dynamic),
  )
}

pub fn request_id(request: Request) -> String {
  case request {
    Ping(id, _) -> id
    ListSessions(id, _) -> id
    GetSession(id, _, _) -> id
    GetEvents(id, _, _, _, _) -> id
    StreamEvents(id, _, _, _) -> id
    Query(id, _, _) -> id
    Pause(id, _) -> id
    Resume(id, _) -> id
    ReloadWorkflow(id, _) -> id
    RetryIssue(id, _, _) -> id
    RetryIssueStartFresh(id, _, _, _) -> id
    RetryWorkflowStep(id, _, _, _) -> id
    RetryWorkflowStepExact(id, _, _, _) -> id
    RecollectWorkflowOutputs(id, _, _) -> id
    RunFinalize(id, _, _, _, _, _, _, _, _, _) -> id
    RetryArtifactPublication(id, _, _, _) -> id
    ParkIssue(id, _, _, _) -> id
    UnparkIssue(id, _, _) -> id
    AbortSession(id, _, _) -> id
    StopAfterCurrentTurn(id, _, _) -> id
    CleanupOrphanSteps(id, _, _, _) -> id
    PromptSession(id, _, _, _) -> id
    RespondUi(id, _, _, _, _) -> id
    RunScheduleNow(id, _, _) -> id
    ReenableSchedule(id, _, _) -> id
    WorkItemAction(id, _, _) -> id
  }
}

pub fn request_token(request: Request) -> String {
  case request {
    Ping(_, token) -> token
    ListSessions(_, token) -> token
    GetSession(_, token, _) -> token
    GetEvents(_, token, _, _, _) -> token
    StreamEvents(_, token, _, _) -> token
    Query(_, token, _) -> token
    Pause(_, token) -> token
    Resume(_, token) -> token
    ReloadWorkflow(_, token) -> token
    RetryIssue(_, token, _) -> token
    RetryIssueStartFresh(_, token, _, _) -> token
    RetryWorkflowStep(_, token, _, _) -> token
    RetryWorkflowStepExact(_, token, _, _) -> token
    RecollectWorkflowOutputs(_, token, _) -> token
    RunFinalize(_, token, _, _, _, _, _, _, _, _) -> token
    RetryArtifactPublication(_, token, _, _) -> token
    ParkIssue(_, token, _, _) -> token
    UnparkIssue(_, token, _) -> token
    AbortSession(_, token, _) -> token
    StopAfterCurrentTurn(_, token, _) -> token
    CleanupOrphanSteps(_, token, _, _) -> token
    PromptSession(_, token, _, _) -> token
    RespondUi(_, token, _, _, _) -> token
    RunScheduleNow(_, token, _) -> token
    ReenableSchedule(_, token, _) -> token
    WorkItemAction(_, token, _) -> token
  }
}

pub fn request_to_string(request: Request) -> String {
  request_to_json(request) |> json.to_string
}

pub fn request_to_json(request: Request) -> json.Json {
  case request {
    Ping(id, token) -> base_request_entries(id, token, "ping") |> json.object
    ListSessions(id, token) ->
      base_request_entries(id, token, "list_sessions") |> json.object
    GetSession(id, token, session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_request_entries(id, token, "get_session")
      ]
      |> json.object
    GetEvents(id, token, session_id, after, limit) ->
      [
        #("session_id", json.string(session_id)),
        #("after", json.int(after)),
        #("limit", json.int(limit)),
        ..base_request_entries(id, token, "get_events")
      ]
      |> json.object
    StreamEvents(id, token, session_id, after) ->
      [
        #("session_id", json.string(session_id)),
        #("after", json.int(after)),
        ..base_request_entries(id, token, "stream_events")
      ]
      |> json.object
    Query(id, token, query) ->
      [
        #("query", query_codec.request_to_json(query)),
        ..base_request_entries(id, token, "query")
      ]
      |> json.object
    Pause(id, token) -> base_request_entries(id, token, "pause") |> json.object
    Resume(id, token) ->
      base_request_entries(id, token, "resume") |> json.object
    ReloadWorkflow(id, token) ->
      base_request_entries(id, token, "reload") |> json.object
    RetryIssue(id, token, issue_ref) ->
      list.append(
        issue_ref_entries(issue_ref),
        base_request_entries(id, token, "retry"),
      )
      |> json.object
    RetryIssueStartFresh(id, token, issue_ref, reason) ->
      list.append(
        [#("reason", json.string(reason)), ..issue_ref_entries(issue_ref)],
        base_request_entries(id, token, "retry_start_fresh"),
      )
      |> json.object
    RetryWorkflowStep(id, token, target, step_id) ->
      list.append(
        retry_workflow_step_entries(target, step_id),
        base_request_entries(id, token, "retry_step"),
      )
      |> json.object
    RetryWorkflowStepExact(id, token, target, step_id) ->
      list.append(
        retry_workflow_step_entries(target, step_id),
        base_request_entries(id, token, "retry_step_exact"),
      )
      |> json.object
    RecollectWorkflowOutputs(id, token, run_id) ->
      [
        #("run_id", json.string(run_id)),
        ..base_request_entries(id, token, "recollect_outputs")
      ]
      |> json.object
    RunFinalize(
      id,
      token,
      run_id,
      validate,
      outputs,
      publish,
      update_tracker,
      dry_run,
      reason,
      allow_unpublished,
    ) ->
      [
        #("run_id", json.string(run_id)),
        #("validate", json.bool(validate)),
        #("outputs", json.string(run_finalize_outputs_to_string(outputs))),
        #("publish", json.bool(publish)),
        #("update_tracker", json.bool(update_tracker)),
        #("dry_run", json.bool(dry_run)),
        #("reason", json.string(reason)),
        #("allow_unpublished", json.bool(allow_unpublished)),
        ..base_request_entries(id, token, "run_finalize")
      ]
      |> json.object
    RetryArtifactPublication(id, token, run_id, publication_id) ->
      list.append(
        retry_artifact_publication_entries(run_id, publication_id),
        base_request_entries(id, token, "retry_artifact_publication"),
      )
      |> json.object
    ParkIssue(id, token, issue_ref, reason) ->
      list.append(
        [#("reason", json.string(reason)), ..issue_ref_entries(issue_ref)],
        base_request_entries(id, token, "park"),
      )
      |> json.object
    UnparkIssue(id, token, issue_ref) ->
      list.append(
        issue_ref_entries(issue_ref),
        base_request_entries(id, token, "unpark"),
      )
      |> json.object
    AbortSession(id, token, session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_request_entries(id, token, "abort")
      ]
      |> json.object
    StopAfterCurrentTurn(id, token, session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_request_entries(id, token, "stop_after_current_turn")
      ]
      |> json.object
    CleanupOrphanSteps(id, token, run_id, dry_run) ->
      [
        #("run_id", json.string(run_id)),
        #("dry_run", json.bool(dry_run)),
        ..base_request_entries(id, token, "cleanup_orphan_steps")
      ]
      |> json.object
    PromptSession(id, token, session_id, message) ->
      [
        #("session_id", json.string(session_id)),
        #("message", json.string(message)),
        ..base_request_entries(id, token, "prompt")
      ]
      |> json.object
    RespondUi(id, token, session_id, request_id, response) ->
      list.append(
        [
          #("session_id", json.string(session_id)),
          #("request_id", json.string(request_id)),
          ..ui_response_entries(response)
        ],
        base_request_entries(id, token, "respond_ui"),
      )
      |> json.object
    RunScheduleNow(id, token, job_id) ->
      [
        #("job_id", json.string(job_id)),
        ..base_request_entries(id, token, "schedule_run_now")
      ]
      |> json.object
    ReenableSchedule(id, token, job_id) ->
      [
        #("job_id", json.string(job_id)),
        ..base_request_entries(id, token, "schedule_reenable")
      ]
      |> json.object
    WorkItemAction(id, token, request) ->
      list.append(
        work_item_action_request_entries(request),
        base_request_entries(id, token, "work_item_action"),
      )
      |> json.object
  }
}

fn issue_ref_entries(
  issue_ref: command.IssueRef,
) -> List(#(String, json.Json)) {
  case issue_ref {
    command.IssueId(id) -> [#("issue_id", json.string(id))]
    command.IssueIdentifier(identifier) -> [
      #("issue_identifier", json.string(identifier)),
    ]
  }
}

fn retry_workflow_step_entries(
  target: command.RetryWorkflowStepTarget,
  step_id: Option(String),
) -> List(#(String, json.Json)) {
  let base = case target {
    command.RetryWorkflowStepAutoTarget(target) -> [
      #("target", json.string(target)),
    ]
    command.RetryWorkflowStepIssueRef(issue_ref) -> issue_ref_entries(issue_ref)
    command.RetryWorkflowStepRunId(run_id) -> [#("run_id", json.string(run_id))]
  }
  case step_id {
    Some(step_id) -> [#("step_id", json.string(step_id)), ..base]
    None -> base
  }
}

fn retry_artifact_publication_entries(
  run_id: String,
  publication_id: Option(String),
) -> List(#(String, json.Json)) {
  case publication_id {
    Some(publication_id) -> [
      #("publication_id", json.string(publication_id)),
      #("run_id", json.string(run_id)),
    ]
    None -> [#("run_id", json.string(run_id))]
  }
}

fn ui_response_entries(
  response: command.UiResponse,
) -> List(#(String, json.Json)) {
  case response {
    command.UiCancel -> [#("cancel", json.bool(True))]
    command.UiValue(value) -> [#("value", json.string(value))]
  }
}

fn work_item_action_request_entries(
  request: command.WorkItemActionRequest,
) -> List(#(String, json.Json)) {
  [
    #("action_id", json.string(request.action_id)),
    #("action_instance_id", json.string(request.action_instance_id)),
    #("target_kind", json.string(request.target_kind)),
    #(
      "target_provider",
      json.nullable(request.target_provider, of: json.string),
    ),
    #("target_id", json.string(request.target_id)),
    #("observed_fingerprint", json.string(request.observed_fingerprint)),
    #("idempotency_key", json.string(request.idempotency_key)),
    #("params", json.array(request.params, of: work_item_action_param_to_json)),
  ]
}

fn work_item_action_param_to_json(param: #(String, String)) -> json.Json {
  let #(name, value) = param
  json.object([#("name", json.string(name)), #("value", json.string(value))])
}

fn base_request_entries(
  id: String,
  token: String,
  type_: String,
) -> List(#(String, json.Json)) {
  [
    #("version", json.int(version)),
    #("id", json.string(id)),
    #("token", json.string(token)),
    #("type", json.string(type_)),
  ]
}

pub fn decode_request(line: String) -> Result(Request, RequestError) {
  case json.parse(line, request_fields_decoder()) {
    Error(_) -> Error(RequestError("unknown", "bad_json", "malformed JSON"))
    Ok(fields) -> validate_request_fields(fields)
  }
}

fn validate_request_fields(
  fields: RequestFields,
) -> Result(Request, RequestError) {
  case fields.version != version {
    True -> invalid(fields.id, "unsupported protocol version")
    False ->
      case fields.id == "" || fields.token == "" || fields.type_ == "" {
        True -> invalid(fields.id, "missing id, token, or type")
        False -> request_for_type(fields)
      }
  }
}

fn request_for_type(fields: RequestFields) -> Result(Request, RequestError) {
  case fields.type_ {
    "ping" -> Ok(Ping(fields.id, fields.token))
    "list_sessions" -> Ok(ListSessions(fields.id, fields.token))
    "get_session" ->
      case required_session_id(fields) {
        Ok(session_id) -> Ok(GetSession(fields.id, fields.token, session_id))
        Error(err) -> Error(err)
      }
    "get_events" ->
      case required_session_id(fields) {
        Error(err) -> Error(err)
        Ok(session_id) ->
          case fields.limit <= 0 {
            True ->
              Error(RequestError(
                fields.id,
                "invalid_limit",
                "limit must be positive",
              ))
            False ->
              case valid_after(fields) {
                Error(err) -> Error(err)
                Ok(after) ->
                  Ok(GetEvents(
                    fields.id,
                    fields.token,
                    session_id,
                    after,
                    fields.limit,
                  ))
              }
          }
      }
    "stream_events" ->
      case required_session_id(fields) {
        Error(err) -> Error(err)
        Ok(session_id) ->
          case valid_after(fields) {
            Error(err) -> Error(err)
            Ok(after) ->
              Ok(StreamEvents(fields.id, fields.token, session_id, after))
          }
      }
    "query" ->
      case required_query(fields) {
        Ok(query) -> Ok(Query(fields.id, fields.token, query))
        Error(err) -> Error(err)
      }
    "pause" -> Ok(Pause(fields.id, fields.token))
    "resume" -> Ok(Resume(fields.id, fields.token))
    "reload" | "reload_workflow" -> Ok(ReloadWorkflow(fields.id, fields.token))
    "retry" | "retry_issue" ->
      case required_issue_ref(fields) {
        Ok(issue_ref) -> Ok(RetryIssue(fields.id, fields.token, issue_ref))
        Error(err) -> Error(err)
      }
    "retry_start_fresh" ->
      case required_issue_ref(fields), required_reason(fields) {
        Ok(issue_ref), Ok(reason) ->
          Ok(RetryIssueStartFresh(fields.id, fields.token, issue_ref, reason))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "retry_step" ->
      case
        required_retry_workflow_step_target(fields),
        optional_step_id(fields)
      {
        Ok(target), Ok(step_id) ->
          Ok(RetryWorkflowStep(fields.id, fields.token, target, step_id))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "retry_step_exact" ->
      case
        required_retry_workflow_step_target(fields),
        optional_step_id(fields)
      {
        Ok(target), Ok(step_id) ->
          Ok(RetryWorkflowStepExact(fields.id, fields.token, target, step_id))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "recollect_outputs" ->
      case required_run_id(fields) {
        Ok(run_id) ->
          Ok(RecollectWorkflowOutputs(fields.id, fields.token, run_id))
        Error(err) -> Error(err)
      }
    "run_finalize" ->
      case
        required_run_id(fields),
        required_true_flag(fields, fields.validate, "validate"),
        required_run_finalize_outputs(fields),
        required_true_flag(fields, fields.publish, "publish"),
        required_true_flag(fields, fields.update_tracker, "update_tracker"),
        required_reason(fields)
      {
        Ok(run_id),
          Ok(validate),
          Ok(outputs),
          Ok(publish),
          Ok(update_tracker),
          Ok(reason)
        ->
          Ok(RunFinalize(
            fields.id,
            fields.token,
            run_id,
            validate,
            outputs,
            publish,
            update_tracker,
            request_dry_run(fields),
            reason,
            request_allow_unpublished(fields),
          ))
        Error(err), _, _, _, _, _
        | _, Error(err), _, _, _, _
        | _, _, Error(err), _, _, _
        | _, _, _, Error(err), _, _
        | _, _, _, _, Error(err), _
        | _, _, _, _, _, Error(err)
        -> Error(err)
      }
    "retry_artifact_publication" ->
      case required_run_id(fields), optional_publication_id(fields) {
        Ok(run_id), Ok(publication_id) ->
          Ok(RetryArtifactPublication(
            fields.id,
            fields.token,
            run_id,
            publication_id,
          ))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "park" | "park_issue" ->
      case required_issue_ref(fields), required_reason(fields) {
        Ok(issue_ref), Ok(reason) ->
          Ok(ParkIssue(fields.id, fields.token, issue_ref, reason))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "unpark" | "unpark_issue" ->
      case required_issue_ref(fields) {
        Ok(issue_ref) -> Ok(UnparkIssue(fields.id, fields.token, issue_ref))
        Error(err) -> Error(err)
      }
    "abort" | "abort_session" ->
      case required_session_id(fields) {
        Ok(session_id) -> Ok(AbortSession(fields.id, fields.token, session_id))
        Error(err) -> Error(err)
      }
    "stop_after_current_turn" | "stop_after_turn" ->
      case required_session_id(fields) {
        Ok(session_id) ->
          Ok(StopAfterCurrentTurn(fields.id, fields.token, session_id))
        Error(err) -> Error(err)
      }
    "cleanup_orphan_steps" ->
      case required_run_id(fields) {
        Ok(run_id) ->
          Ok(CleanupOrphanSteps(
            fields.id,
            fields.token,
            run_id,
            request_dry_run(fields),
          ))
        Error(err) -> Error(err)
      }
    "prompt" | "prompt_session" ->
      case required_session_id(fields), required_message(fields) {
        Ok(session_id), Ok(message) ->
          Ok(PromptSession(fields.id, fields.token, session_id, message))
        Error(err), _ | _, Error(err) -> Error(err)
      }
    "respond_ui" | "ui_respond" ->
      case
        required_session_id(fields),
        required_request_id(fields),
        required_ui_response(fields)
      {
        Ok(session_id), Ok(request_id), Ok(response) ->
          Ok(RespondUi(
            fields.id,
            fields.token,
            session_id,
            request_id,
            response,
          ))
        Error(err), _, _ | _, Error(err), _ | _, _, Error(err) -> Error(err)
      }
    "schedule_run_now" | "run_schedule_now" ->
      case required_job_id(fields) {
        Ok(job_id) -> Ok(RunScheduleNow(fields.id, fields.token, job_id))
        Error(err) -> Error(err)
      }
    "schedule_reenable" | "reenable_schedule" ->
      case required_job_id(fields) {
        Ok(job_id) -> Ok(ReenableSchedule(fields.id, fields.token, job_id))
        Error(err) -> Error(err)
      }
    "work_item_action" ->
      case required_work_item_action_request(fields) {
        Ok(request) -> Ok(WorkItemAction(fields.id, fields.token, request))
        Error(err) -> Error(err)
      }
    other ->
      Error(RequestError(
        fields.id,
        "unknown_command",
        "unknown command: " <> other,
      ))
  }
}

fn required_query(
  fields: RequestFields,
) -> Result(query_types.QueryRequest, RequestError) {
  case fields.query {
    Some(query) ->
      case query_codec.decode_request_dynamic(query) {
        Ok(request) -> Ok(request)
        Error(query_types.QueryError(code: code, message: message)) ->
          Error(RequestError(
            fields.id,
            query_types.error_code_to_string(code),
            message,
          ))
      }
    None -> invalid(fields.id, "missing query payload")
  }
}

fn required_session_id(fields: RequestFields) -> Result(String, RequestError) {
  case fields.session_id {
    Some("") -> invalid(fields.id, "session_id must not be empty")
    Some(session_id) -> Ok(session_id)
    None -> invalid(fields.id, "missing session_id")
  }
}

fn required_issue_ref(
  fields: RequestFields,
) -> Result(command.IssueRef, RequestError) {
  case fields.issue_id, fields.issue_identifier {
    Some(_), Some(_) ->
      invalid(fields.id, "provide issue_id or issue_identifier, not both")
    Some(issue_id), None -> {
      let issue_id = string.trim(issue_id)
      case issue_id == "" {
        True -> invalid(fields.id, "issue reference must not be empty")
        False -> Ok(command.IssueId(issue_id))
      }
    }
    None, Some(identifier) -> {
      let identifier = string.trim(identifier)
      case identifier == "" {
        True -> invalid(fields.id, "issue reference must not be empty")
        False -> Ok(command.IssueIdentifier(identifier))
      }
    }
    None, None -> invalid(fields.id, "missing issue reference")
  }
}

fn required_retry_workflow_step_target(
  fields: RequestFields,
) -> Result(command.RetryWorkflowStepTarget, RequestError) {
  case fields.target, fields.run_id, fields.issue_id, fields.issue_identifier {
    Some(_), Some(_), _, _
    | Some(_), _, Some(_), _
    | Some(_), _, _, Some(_)
    | _, Some(_), Some(_), _
    | _, Some(_), _, Some(_)
    | _, _, Some(_), Some(_)
    ->
      invalid(
        fields.id,
        "provide exactly one of target, run_id, issue_id, or issue_identifier",
      )
    Some(target), None, None, None -> {
      let target = string.trim(target)
      case target == "" {
        True -> invalid(fields.id, "target must not be empty")
        False -> Ok(command.RetryWorkflowStepAutoTarget(target))
      }
    }
    None, Some(run_id), None, None -> {
      let run_id = string.trim(run_id)
      case run_id == "" {
        True -> invalid(fields.id, "run_id must not be empty")
        False -> Ok(command.RetryWorkflowStepRunId(run_id))
      }
    }
    None, None, Some(_), None | None, None, None, Some(_) ->
      required_issue_ref(fields)
      |> result.map(command.RetryWorkflowStepIssueRef)
    None, None, None, None -> invalid(fields.id, "missing retry_step target")
  }
}

fn optional_step_id(
  fields: RequestFields,
) -> Result(Option(String), RequestError) {
  case fields.step_id {
    Some(step_id) -> {
      let step_id = string.trim(step_id)
      case step_id == "" {
        True -> invalid(fields.id, "step_id must not be empty")
        False -> Ok(Some(step_id))
      }
    }
    None -> Ok(None)
  }
}

fn optional_publication_id(
  fields: RequestFields,
) -> Result(Option(String), RequestError) {
  case fields.publication_id {
    Some(publication_id) -> {
      let publication_id = string.trim(publication_id)
      case publication_id == "" {
        True -> invalid(fields.id, "publication_id must not be empty")
        False -> Ok(Some(publication_id))
      }
    }
    None -> Ok(None)
  }
}

fn required_true_flag(
  fields: RequestFields,
  value: Option(Bool),
  name: String,
) -> Result(Bool, RequestError) {
  case value {
    Some(True) -> Ok(True)
    Some(False) | None -> invalid(fields.id, name <> " must be true")
  }
}

fn required_run_finalize_outputs(
  fields: RequestFields,
) -> Result(command.RunFinalizeOutputs, RequestError) {
  case fields.outputs {
    Some("auto") -> Ok(command.RunFinalizeOutputsAuto)
    Some(_) -> invalid(fields.id, "outputs must be auto")
    None -> invalid(fields.id, "missing outputs")
  }
}

fn run_finalize_outputs_to_string(
  outputs: command.RunFinalizeOutputs,
) -> String {
  case outputs {
    command.RunFinalizeOutputsAuto -> "auto"
  }
}

fn required_reason(fields: RequestFields) -> Result(String, RequestError) {
  case fields.reason {
    Some(reason) -> {
      let reason = string.trim(reason)
      case reason == "" {
        True -> invalid(fields.id, "reason must not be empty")
        False -> Ok(reason)
      }
    }
    None -> invalid(fields.id, "missing reason")
  }
}

fn required_message(fields: RequestFields) -> Result(String, RequestError) {
  case fields.message {
    Some(message) -> {
      let message = string.trim(message)
      case message == "" {
        True -> invalid(fields.id, "message must not be empty")
        False -> Ok(message)
      }
    }
    None -> invalid(fields.id, "missing message")
  }
}

fn required_request_id(fields: RequestFields) -> Result(String, RequestError) {
  case fields.request_id {
    Some(request_id) -> {
      let request_id = string.trim(request_id)
      case request_id == "" {
        True -> invalid(fields.id, "request_id must not be empty")
        False -> Ok(request_id)
      }
    }
    None -> invalid(fields.id, "missing request_id")
  }
}

fn required_job_id(fields: RequestFields) -> Result(String, RequestError) {
  case fields.job_id {
    Some(job_id) -> {
      let job_id = string.trim(job_id)
      case job_id == "" {
        True -> invalid(fields.id, "job_id must not be empty")
        False -> Ok(job_id)
      }
    }
    None -> invalid(fields.id, "missing job_id")
  }
}

fn required_run_id(fields: RequestFields) -> Result(String, RequestError) {
  case fields.run_id {
    Some(run_id) -> {
      let run_id = string.trim(run_id)
      case run_id == "" {
        True -> invalid(fields.id, "run_id must not be empty")
        False -> Ok(run_id)
      }
    }
    None -> invalid(fields.id, "missing run_id")
  }
}

fn required_work_item_action_request(
  fields: RequestFields,
) -> Result(command.WorkItemActionRequest, RequestError) {
  case
    required_non_empty(fields.id, fields.action_id, "action_id"),
    required_non_empty(
      fields.id,
      fields.action_instance_id,
      "action_instance_id",
    ),
    required_non_empty(fields.id, fields.target_kind, "target_kind"),
    required_non_empty(fields.id, fields.target_id, "target_id"),
    required_non_empty(
      fields.id,
      fields.observed_fingerprint,
      "observed_fingerprint",
    ),
    required_non_empty(fields.id, fields.idempotency_key, "idempotency_key"),
    optional_work_item_action_params(fields)
  {
    Ok(action_id),
      Ok(action_instance_id),
      Ok(target_kind),
      Ok(target_id),
      Ok(observed_fingerprint),
      Ok(idempotency_key),
      Ok(params)
    ->
      Ok(command.WorkItemActionRequest(
        action_id: action_id,
        action_instance_id: action_instance_id,
        target_kind: target_kind,
        target_provider: fields.target_provider,
        target_id: target_id,
        observed_fingerprint: observed_fingerprint,
        idempotency_key: idempotency_key,
        params: params,
      ))
    Error(err), _, _, _, _, _, _
    | _, Error(err), _, _, _, _, _
    | _, _, Error(err), _, _, _, _
    | _, _, _, Error(err), _, _, _
    | _, _, _, _, Error(err), _, _
    | _, _, _, _, _, Error(err), _
    | _, _, _, _, _, _, Error(err)
    -> Error(err)
  }
}

fn optional_work_item_action_params(
  fields: RequestFields,
) -> Result(List(#(String, String)), RequestError) {
  case fields.params {
    Some(value) -> decode_work_item_action_params(fields.id, value)
    None -> Ok([])
  }
}

fn decode_work_item_action_params(
  id: String,
  value: Dynamic,
) -> Result(List(#(String, String)), RequestError) {
  case decode.run(value, decode.list(work_item_action_param_decoder())) {
    Ok(params) -> Ok(params)
    Error(_) -> invalid(id, "params must be an array of {name, value} objects")
  }
}

fn work_item_action_param_decoder() -> decode.Decoder(#(String, String)) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  decode.success(#(name, value))
}

fn required_non_empty(
  id: String,
  value: Option(String),
  field_name: String,
) -> Result(String, RequestError) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> invalid(id, field_name <> " must not be empty")
        False -> Ok(value)
      }
    }
    None -> invalid(id, "missing " <> field_name)
  }
}

fn request_dry_run(fields: RequestFields) -> Bool {
  case fields.dry_run {
    Some(value) -> value
    None -> True
  }
}

fn request_allow_unpublished(fields: RequestFields) -> Bool {
  case fields.allow_unpublished {
    Some(value) -> value
    None -> False
  }
}

fn required_ui_response(
  fields: RequestFields,
) -> Result(command.UiResponse, RequestError) {
  case fields.cancel, fields.value {
    Some(True), None -> Ok(command.UiCancel)
    Some(False), None -> invalid(fields.id, "cancel must be true when provided")
    None, Some(value) -> Ok(command.UiValue(value))
    Some(True), Some(_) ->
      invalid(fields.id, "provide --cancel or value, not both")
    Some(False), Some(_) ->
      invalid(fields.id, "cancel must be true when provided")
    None, None -> invalid(fields.id, "missing UI response")
  }
}

fn valid_after(fields: RequestFields) -> Result(Int, RequestError) {
  case fields.after < 0 {
    True -> invalid(fields.id, "after must be non-negative")
    False -> Ok(fields.after)
  }
}

fn invalid(id: String, message: String) -> Result(a, RequestError) {
  Error(RequestError(id, "invalid_request", message))
}

fn request_fields_decoder() -> decode.Decoder(RequestFields) {
  use version <- decode.optional_field("version", 0, decode.int)
  use id <- decode.optional_field("id", "", decode.string)
  use token <- decode.optional_field("token", "", decode.string)
  use type_ <- decode.optional_field("type", "", decode.string)
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use query <- decode.optional_field(
    "query",
    None,
    decode.optional(decode.dynamic),
  )
  use after <- decode.optional_field("after", 0, decode.int)
  use limit <- decode.optional_field("limit", 100, decode.int)
  use issue_id <- decode.optional_field(
    "issue_id",
    None,
    decode.optional(decode.string),
  )
  use issue_identifier <- decode.optional_field(
    "issue_identifier",
    None,
    decode.optional(decode.string),
  )
  use target <- decode.optional_field(
    "target",
    None,
    decode.optional(decode.string),
  )
  use run_id <- decode.optional_field(
    "run_id",
    None,
    decode.optional(decode.string),
  )
  use step_id <- decode.optional_field(
    "step_id",
    None,
    decode.optional(decode.string),
  )
  use publication_id <- decode.optional_field(
    "publication_id",
    None,
    decode.optional(decode.string),
  )
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use request_id <- decode.optional_field(
    "request_id",
    None,
    decode.optional(decode.string),
  )
  use cancel <- decode.optional_field(
    "cancel",
    None,
    decode.optional(decode.bool),
  )
  use value <- decode.optional_field(
    "value",
    None,
    decode.optional(decode.string),
  )
  use job_id <- decode.optional_field(
    "job_id",
    None,
    decode.optional(decode.string),
  )
  use dry_run <- decode.optional_field(
    "dry_run",
    None,
    decode.optional(decode.bool),
  )
  use validate <- decode.optional_field(
    "validate",
    None,
    decode.optional(decode.bool),
  )
  use outputs <- decode.optional_field(
    "outputs",
    None,
    decode.optional(decode.string),
  )
  use publish <- decode.optional_field(
    "publish",
    None,
    decode.optional(decode.bool),
  )
  use update_tracker <- decode.optional_field(
    "update_tracker",
    None,
    decode.optional(decode.bool),
  )
  use allow_unpublished <- decode.optional_field(
    "allow_unpublished",
    None,
    decode.optional(decode.bool),
  )
  use action_id <- decode.optional_field(
    "action_id",
    None,
    decode.optional(decode.string),
  )
  use action_instance_id <- decode.optional_field(
    "action_instance_id",
    None,
    decode.optional(decode.string),
  )
  use target_kind <- decode.optional_field(
    "target_kind",
    None,
    decode.optional(decode.string),
  )
  use target_provider <- decode.optional_field(
    "target_provider",
    None,
    decode.optional(decode.string),
  )
  use target_id <- decode.optional_field(
    "target_id",
    None,
    decode.optional(decode.string),
  )
  use observed_fingerprint <- decode.optional_field(
    "observed_fingerprint",
    None,
    decode.optional(decode.string),
  )
  use idempotency_key <- decode.optional_field(
    "idempotency_key",
    None,
    decode.optional(decode.string),
  )
  use params <- decode.optional_field(
    "params",
    None,
    decode.optional(decode.dynamic),
  )
  decode.success(RequestFields(
    version: version,
    id: id,
    token: token,
    type_: type_,
    session_id: session_id,
    query: query,
    after: after,
    limit: limit,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    target: target,
    run_id: run_id,
    step_id: step_id,
    publication_id: publication_id,
    reason: reason,
    message: message,
    request_id: request_id,
    cancel: cancel,
    value: value,
    job_id: job_id,
    dry_run: dry_run,
    validate: validate,
    outputs: outputs,
    publish: publish,
    update_tracker: update_tracker,
    allow_unpublished: allow_unpublished,
    action_id: action_id,
    action_instance_id: action_instance_id,
    target_kind: target_kind,
    target_provider: target_provider,
    target_id: target_id,
    observed_fingerprint: observed_fingerprint,
    idempotency_key: idempotency_key,
    params: params,
  ))
}

pub fn success_response(id: String, data: json.Json) -> Response {
  Response(id: id, ok: True, data: Some(data), error: None)
}

pub fn error_response(id: String, code: String, message: String) -> Response {
  Response(
    id: id,
    ok: False,
    data: None,
    error: Some(ErrorBody(code: code, message: message)),
  )
}

pub fn request_error_response(error: RequestError) -> Response {
  error_response(error.id, error.code, error.message)
}

pub fn response_to_string(response: Response) -> String {
  response_to_json(response) |> json.to_string
}

pub fn response_to_json(response: Response) -> json.Json {
  response_to_json_with_fields(response, [])
}

pub fn response_to_json_with_fields(
  response: Response,
  extra_fields: List(#(String, json.Json)),
) -> json.Json {
  let required =
    list.append(
      [
        #("version", json.int(version)),
        #("id", json.string(response.id)),
        #("ok", json.bool(response.ok)),
      ],
      extra_fields,
    )
  case response.ok {
    True ->
      [#("data", option_json(response.data)), ..required]
      |> list.reverse
      |> json.object
    False ->
      [#("error", error_body_to_json(option_error(response.error))), ..required]
      |> list.reverse
      |> json.object
  }
}

fn option_json(value: Option(json.Json)) -> json.Json {
  case value {
    Some(json) -> json
    None -> json.null()
  }
}

fn option_error(value: Option(ErrorBody)) -> ErrorBody {
  case value {
    Some(error) -> error
    None -> ErrorBody("unknown_error", "unknown error")
  }
}

fn error_body_to_json(error: ErrorBody) -> json.Json {
  json.object([
    #("code", json.string(error.code)),
    #("message", json.string(error.message)),
  ])
}

pub fn ping_data() -> json.Json {
  json.object([#("pong", json.bool(True))])
}

pub fn list_sessions_data(snapshot: event.SessionList) -> json.Json {
  json.object([
    #(
      "sessions",
      json.array(snapshot.sessions, of: session_json.summary_to_json),
    ),
    #("now_ms", json.int(snapshot.now_ms)),
  ])
}

pub fn session_data(summary: Option(event.SessionSummary)) -> json.Json {
  json.object([
    #("session", json.nullable(summary, of: session_json.summary_to_json)),
  ])
}

pub fn event_page_data(page: event.EventPage) -> json.Json {
  session_json.page_to_json(page)
}

pub fn command_result_data(result: command.CommandResult) -> json.Json {
  command.command_result_to_json(result)
}

pub fn query_data(
  result: Result(query_types.QueryResponse, query_types.QueryError),
) -> json.Json {
  case result {
    Ok(response) -> query_codec.response_to_json(response)
    Error(error) -> query_codec.error_to_json(error)
  }
}

pub fn command_request(
  id: String,
  token: String,
  operator_command: command.OperatorCommand,
) -> Request {
  case operator_command {
    command.PauseDispatch -> Pause(id, token)
    command.ResumeDispatch -> Resume(id, token)
    command.ReloadWorkflow -> ReloadWorkflow(id, token)
    command.RetryIssue(issue_ref) -> RetryIssue(id, token, issue_ref)
    command.RetryIssueStartFresh(issue_ref, reason) ->
      RetryIssueStartFresh(id, token, issue_ref, reason)
    command.RetryWorkflowStep(target, step_id) ->
      RetryWorkflowStep(id, token, target, step_id)
    command.RetryWorkflowStepExact(target, step_id) ->
      RetryWorkflowStepExact(id, token, target, step_id)
    command.RecollectWorkflowOutputs(run_id) ->
      RecollectWorkflowOutputs(id, token, run_id)
    command.RunFinalize(
      run_id: run_id,
      validate: validate,
      outputs: outputs,
      publish: publish,
      update_tracker: update_tracker,
      dry_run: dry_run,
      reason: reason,
      allow_unpublished: allow_unpublished,
    ) ->
      RunFinalize(
        id,
        token,
        run_id,
        validate,
        outputs,
        publish,
        update_tracker,
        dry_run,
        reason,
        allow_unpublished,
      )
    command.RetryArtifactPublication(run_id, publication_id) ->
      RetryArtifactPublication(id, token, run_id, publication_id)
    command.ParkIssue(issue_ref, reason) ->
      ParkIssue(id, token, issue_ref, reason)
    command.UnparkIssue(issue_ref) -> UnparkIssue(id, token, issue_ref)
    command.AbortSession(session_id) -> AbortSession(id, token, session_id)
    command.StopAfterCurrentTurn(session_id) ->
      StopAfterCurrentTurn(id, token, session_id)
    command.CleanupOrphanSteps(run_id, dry_run) ->
      CleanupOrphanSteps(id, token, run_id, dry_run)
    command.PromptSession(session_id, message) ->
      PromptSession(id, token, session_id, message)
    command.RespondUi(session_id, request_id, response) ->
      RespondUi(id, token, session_id, request_id, response)
    command.RunScheduleNow(job_id) -> RunScheduleNow(id, token, job_id)
    command.ReenableSchedule(job_id) -> ReenableSchedule(id, token, job_id)
    command.WorkItemAction(request) -> WorkItemAction(id, token, request)
  }
}

pub fn query_request(
  id: String,
  token: String,
  query: query_types.QueryRequest,
) -> Request {
  Query(id, token, query)
}

pub fn request_operator_command(
  request: Request,
) -> Option(command.OperatorCommand) {
  case request {
    Pause(_, _) -> Some(command.PauseDispatch)
    Resume(_, _) -> Some(command.ResumeDispatch)
    ReloadWorkflow(_, _) -> Some(command.ReloadWorkflow)
    RetryIssue(_, _, issue_ref) -> Some(command.RetryIssue(issue_ref))
    RetryIssueStartFresh(_, _, issue_ref, reason) ->
      Some(command.RetryIssueStartFresh(issue_ref, reason))
    RetryWorkflowStep(_, _, target, step_id) ->
      Some(command.RetryWorkflowStep(target, step_id))
    RetryWorkflowStepExact(_, _, target, step_id) ->
      Some(command.RetryWorkflowStepExact(target, step_id))
    RecollectWorkflowOutputs(_, _, run_id) ->
      Some(command.RecollectWorkflowOutputs(run_id))
    RunFinalize(
      _,
      _,
      run_id,
      validate,
      outputs,
      publish,
      update_tracker,
      dry_run,
      reason,
      allow_unpublished,
    ) ->
      Some(command.RunFinalize(
        run_id: run_id,
        validate: validate,
        outputs: outputs,
        publish: publish,
        update_tracker: update_tracker,
        dry_run: dry_run,
        reason: reason,
        allow_unpublished: allow_unpublished,
      ))
    RetryArtifactPublication(_, _, run_id, publication_id) ->
      Some(command.RetryArtifactPublication(run_id, publication_id))
    ParkIssue(_, _, issue_ref, reason) ->
      Some(command.ParkIssue(issue_ref, reason))
    UnparkIssue(_, _, issue_ref) -> Some(command.UnparkIssue(issue_ref))
    AbortSession(_, _, session_id) -> Some(command.AbortSession(session_id))
    StopAfterCurrentTurn(_, _, session_id) ->
      Some(command.StopAfterCurrentTurn(session_id))
    CleanupOrphanSteps(_, _, run_id, dry_run) ->
      Some(command.CleanupOrphanSteps(run_id, dry_run))
    PromptSession(_, _, session_id, message) ->
      Some(command.PromptSession(session_id, message))
    RespondUi(_, _, session_id, request_id, response) ->
      Some(command.RespondUi(session_id, request_id, response))
    RunScheduleNow(_, _, job_id) -> Some(command.RunScheduleNow(job_id))
    ReenableSchedule(_, _, job_id) -> Some(command.ReenableSchedule(job_id))
    WorkItemAction(_, _, request) -> Some(command.WorkItemAction(request))
    Ping(_, _)
    | ListSessions(_, _)
    | GetSession(_, _, _)
    | GetEvents(_, _, _, _, _)
    | StreamEvents(_, _, _, _)
    | Query(_, _, _) -> None
  }
}

pub fn stream_started_data(session_id: String, after: Int) -> json.Json {
  json.object([
    #("streaming", json.bool(True)),
    #("session_id", json.string(session_id)),
    #("after", json.int(after)),
  ])
}

pub fn stream_event_to_string(
  id: String,
  stored_event: event.SessionEvent,
) -> String {
  json.object([
    #("version", json.int(version)),
    #("id", json.string(id)),
    #("stream", json.bool(True)),
    #("session_id", json.string(stored_event.session_id)),
    #("cursor", json.int(stored_event.cursor)),
    #("event", session_json.event_to_json(stored_event)),
  ])
  |> json.to_string
}

pub fn decode_response(line: String) -> Result(Response, ErrorBody) {
  case json.parse(line, response_decoder()) {
    Ok(response) -> Ok(response)
    Error(_) -> Error(ErrorBody("bad_json", "malformed response JSON"))
  }
}

pub fn decode_ping_response(line: String) -> Result(Nil, ErrorBody) {
  decode_response_result(line, ping_decoder())
}

pub fn decode_list_sessions_response(
  line: String,
) -> Result(List(event.SessionSummary), ErrorBody) {
  case decode_list_sessions_snapshot_response(line) {
    Ok(snapshot) -> Ok(snapshot.sessions)
    Error(error) -> Error(error)
  }
}

pub fn decode_list_sessions_snapshot_response(
  line: String,
) -> Result(event.SessionList, ErrorBody) {
  decode_response_result(line, session_list_decoder())
}

pub fn decode_get_session_response(
  line: String,
) -> Result(Option(event.SessionSummary), ErrorBody) {
  decode_response_result(
    line,
    decode.at(["session"], decode.optional(session_summary_decoder())),
  )
}

pub fn decode_get_events_response(
  line: String,
) -> Result(event.EventPage, ErrorBody) {
  decode_response_result(line, event_page_decoder())
}

pub fn decode_command_result_response(
  line: String,
) -> Result(command.CommandResult, ErrorBody) {
  decode_response_result(line, command.command_result_decoder())
}

pub fn decode_stream_event(
  line: String,
) -> Result(event.SessionEvent, ErrorBody) {
  case json.parse(line, stream_event_decoder()) {
    Ok(stored_event) -> Ok(stored_event)
    Error(_) -> Error(ErrorBody("bad_json", "malformed stream event JSON"))
  }
}

fn decode_response_result(
  line: String,
  data_decoder: decode.Decoder(a),
) -> Result(a, ErrorBody) {
  case json.parse(line, response_result_decoder(data_decoder)) {
    Ok(result) -> result
    Error(_) -> Error(ErrorBody("bad_json", "malformed response JSON"))
  }
}

fn response_decoder() -> decode.Decoder(Response) {
  use id <- decode.optional_field("id", "unknown", decode.string)
  use ok <- decode.field("ok", decode.bool)
  use data <- decode.optional_field(
    "data",
    None,
    decode.optional(decode.dynamic),
  )
  use error <- decode.optional_field(
    "error",
    None,
    decode.optional(error_body_decoder()),
  )
  let data = option_dynamic_to_json(data)
  decode.success(Response(id: id, ok: ok, data: data, error: error))
}

fn response_result_decoder(
  data_decoder: decode.Decoder(a),
) -> decode.Decoder(Result(a, ErrorBody)) {
  use ok <- decode.field("ok", decode.bool)
  case ok {
    True -> {
      use data <- decode.field("data", data_decoder)
      decode.success(Ok(data))
    }
    False -> {
      use error <- decode.field("error", error_body_decoder())
      decode.success(Error(error))
    }
  }
}

fn ping_decoder() -> decode.Decoder(Nil) {
  use _pong <- decode.optional_field("pong", True, decode.bool)
  decode.success(Nil)
}

fn error_body_decoder() -> decode.Decoder(ErrorBody) {
  use code <- decode.field("code", decode.string)
  use message <- decode.optional_field("message", code, decode.string)
  decode.success(ErrorBody(code: code, message: message))
}

fn session_list_decoder() -> decode.Decoder(event.SessionList) {
  use sessions <- decode.field(
    "sessions",
    decode.list(of: session_summary_decoder()),
  )
  use now_ms <- decode.optional_field(
    "now_ms",
    None,
    decode.optional(decode.int),
  )
  let snapshot_now_ms = case now_ms {
    Some(value) -> value
    None -> fallback_list_now_ms(sessions)
  }
  decode.success(event.SessionList(sessions: sessions, now_ms: snapshot_now_ms))
}

fn fallback_list_now_ms(sessions: List(event.SessionSummary)) -> Int {
  case sessions {
    [] -> 0
    [first, ..rest] ->
      list.fold(rest, first.last_event_at_ms, fn(latest, summary) {
        case summary.last_event_at_ms > latest {
          True -> summary.last_event_at_ms
          False -> latest
        }
      })
  }
}

fn session_summary_decoder() -> decode.Decoder(event.SessionSummary) {
  use session_id <- decode.field("session_id", decode.string)
  use maybe_display_name <- decode.optional_field(
    "display_name",
    None,
    decode.optional(decode.string),
  )
  let display_name = case maybe_display_name {
    Some(value) -> value
    None -> session_id
  }
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use issue_title <- decode.optional_field("issue_title", "", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use pi_session_id <- decode.optional_field(
    "pi_session_id",
    None,
    decode.optional(decode.string),
  )
  use status_name <- decode.field("status", decode.string)
  use exit_reason <- decode.optional_field(
    "exit_reason",
    None,
    decode.optional(decode.string),
  )
  use recovery <- decode.optional_field(
    "recovery",
    None,
    decode.optional(recovery_info_decoder()),
  )
  use current_turn <- decode.field("current_turn", decode.int)
  use current_turn_status_name <- decode.optional_field(
    "current_turn_status",
    None,
    decode.optional(decode.string),
  )
  use current_turn_started_at_ms <- decode.optional_field(
    "current_turn_started_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_turn_finished_at_ms <- decode.optional_field(
    "last_turn_finished_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_turn_duration_ms <- decode.optional_field(
    "last_turn_duration_ms",
    None,
    decode.optional(decode.int),
  )
  use last_turn_token_delta <- decode.optional_field(
    "last_turn_token_delta",
    session_tokens.zero_token_totals(),
    token_totals_decoder(),
  )
  use last_turn_reason_name <- decode.optional_field(
    "last_turn_reason",
    None,
    decode.optional(decode.string),
  )
  let current_turn_status =
    turn_status_from_optional_string(current_turn_status_name)
  let last_turn_reason = turn_reason_from_optional_string(last_turn_reason_name)
  use started_at_ms <- decode.field("started_at_ms", decode.int)
  use last_event_at_ms <- decode.field("last_event_at_ms", decode.int)
  use token_totals <- decode.field("tokens", token_totals_decoder())
  case status_from_string(status_name, exit_reason) {
    Ok(status) ->
      decode.success(event.SessionSummary(
        session_id: session_id,
        display_name: display_name,
        issue_id: issue_id,
        issue_identifier: issue_identifier,
        issue_title: issue_title,
        workspace_path: workspace_path,
        pi_session_id: pi_session_id,
        status: status,
        recovery: recovery,
        current_turn: current_turn,
        current_turn_status: current_turn_status,
        current_turn_started_at_ms: current_turn_started_at_ms,
        last_turn_finished_at_ms: last_turn_finished_at_ms,
        last_turn_duration_ms: last_turn_duration_ms,
        last_turn_token_delta: last_turn_token_delta,
        last_turn_reason: last_turn_reason,
        started_at_ms: started_at_ms,
        last_event_at_ms: last_event_at_ms,
        token_totals: token_totals,
      ))
    Error(Nil) ->
      decode.failure(
        event.SessionSummary(
          session_id: session_id,
          display_name: display_name,
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          issue_title: issue_title,
          workspace_path: workspace_path,
          pi_session_id: pi_session_id,
          status: event.Exited(session_reason.Failed),
          recovery: recovery,
          current_turn: current_turn,
          current_turn_status: current_turn_status,
          current_turn_started_at_ms: current_turn_started_at_ms,
          last_turn_finished_at_ms: last_turn_finished_at_ms,
          last_turn_duration_ms: last_turn_duration_ms,
          last_turn_token_delta: last_turn_token_delta,
          last_turn_reason: last_turn_reason,
          started_at_ms: started_at_ms,
          last_event_at_ms: last_event_at_ms,
          token_totals: token_totals,
        ),
        expected: "SessionSummary",
      )
  }
}

fn recovery_info_decoder() -> decode.Decoder(event.RecoveryInfo) {
  use status_name <- decode.field("status", decode.string)
  use source <- decode.optional_field("source", "unknown", decode.string)
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use safe_actions <- decode.optional_field(
    "safe_actions",
    [],
    decode.list(of: recovery_action_decoder()),
  )
  use workflow_run_id <- decode.optional_field(
    "workflow_run_id",
    None,
    decode.optional(decode.string),
  )
  use workflow_step_id <- decode.optional_field(
    "workflow_step_id",
    None,
    decode.optional(decode.string),
  )
  use workflow_attempt_index <- decode.optional_field(
    "workflow_attempt_index",
    None,
    decode.optional(decode.int),
  )
  use parent_session_id <- decode.optional_field(
    "parent_session_id",
    None,
    decode.optional(decode.string),
  )
  use orphan_status <- decode.optional_field(
    "orphan_status",
    None,
    decode.optional(decode.string),
  )
  use issue_state <- decode.optional_field(
    "issue_state",
    None,
    decode.optional(decode.string),
  )
  use recommended_action <- decode.optional_field(
    "recommended_action",
    None,
    decode.optional(decode.string),
  )
  use current_pi_session_id <- decode.optional_field(
    "current_pi_session_id",
    None,
    decode.optional(decode.string),
  )
  use previous_pi_session_id <- decode.optional_field(
    "previous_pi_session_id",
    None,
    decode.optional(decode.string),
  )
  use park_reason <- decode.optional_field(
    "park_reason",
    None,
    decode.optional(decode.string),
  )
  use park_release_policy <- decode.optional_field(
    "park_release_policy",
    None,
    decode.optional(decode.string),
  )
  use parked_at_ms <- decode.optional_field(
    "parked_at_ms",
    None,
    decode.optional(decode.int),
  )
  use drift_kind <- decode.optional_field(
    "drift_kind",
    None,
    decode.optional(decode.string),
  )
  use retention_until_ms <- decode.optional_field(
    "retention_until_ms",
    None,
    decode.optional(decode.int),
  )
  use cleanup_eligible_at_ms <- decode.optional_field(
    "cleanup_eligible_at_ms",
    None,
    decode.optional(decode.int),
  )
  use cleanup_phase <- decode.optional_field(
    "cleanup_phase",
    None,
    decode.optional(cleanup_phase_decoder()),
  )
  case event.recovery_status_from_string(status_name) {
    Some(status) ->
      decode.success(event.RecoveryInfo(
        status: status,
        source: source,
        message: message,
        safe_actions: safe_actions,
        workflow_run_id: workflow_run_id,
        workflow_step_id: workflow_step_id,
        workflow_attempt_index: workflow_attempt_index,
        parent_session_id: parent_session_id,
        orphan_status: orphan_status,
        issue_state: issue_state,
        recommended_action: recommended_action,
        current_pi_session_id: current_pi_session_id,
        previous_pi_session_id: previous_pi_session_id,
        park_reason: park_reason,
        park_release_policy: park_release_policy,
        parked_at_ms: parked_at_ms,
        drift_kind: drift_kind,
        retention_until_ms: retention_until_ms,
        cleanup_eligible_at_ms: cleanup_eligible_at_ms,
        cleanup_phase: cleanup_phase,
      ))
    None ->
      decode.failure(
        event.RecoveryInfo(
          status: event.Recovered,
          source: source,
          message: message,
          safe_actions: safe_actions,
          workflow_run_id: workflow_run_id,
          workflow_step_id: workflow_step_id,
          workflow_attempt_index: workflow_attempt_index,
          parent_session_id: parent_session_id,
          orphan_status: orphan_status,
          issue_state: issue_state,
          recommended_action: recommended_action,
          current_pi_session_id: current_pi_session_id,
          previous_pi_session_id: previous_pi_session_id,
          park_reason: park_reason,
          park_release_policy: park_release_policy,
          parked_at_ms: parked_at_ms,
          drift_kind: drift_kind,
          retention_until_ms: retention_until_ms,
          cleanup_eligible_at_ms: cleanup_eligible_at_ms,
          cleanup_phase: cleanup_phase,
        ),
        expected: "RecoveryInfo",
      )
  }
}

fn recovery_action_decoder() -> decode.Decoder(event.RecoveryAction) {
  use value <- decode.then(decode.string)
  case event.recovery_action_from_string(value) {
    Some(action) -> decode.success(action)
    None -> decode.failure(event.Inspect, expected: "RecoveryAction")
  }
}

fn cleanup_phase_decoder() -> decode.Decoder(event.CleanupPhase) {
  use value <- decode.then(decode.string)
  case event.cleanup_phase_from_string(value) {
    Some(phase) -> decode.success(phase)
    None -> decode.failure(event.Retained, expected: "CleanupPhase")
  }
}

fn event_page_decoder() -> decode.Decoder(event.EventPage) {
  use events <- decode.field("events", decode.list(of: session_event_decoder()))
  use next_cursor <- decode.field("next_cursor", decode.int)
  use truncated <- decode.field("truncated", decode.bool)
  decode.success(event.EventPage(
    events: events,
    next_cursor: next_cursor,
    truncated: truncated,
  ))
}

fn stream_event_decoder() -> decode.Decoder(event.SessionEvent) {
  use stored_event <- decode.field("event", session_event_decoder())
  decode.success(stored_event)
}

fn session_event_decoder() -> decode.Decoder(event.SessionEvent) {
  use cursor <- decode.field("cursor", decode.int)
  use at_ms <- decode.field("at_ms", decode.int)
  use session_id <- decode.field("session_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use payload <- decode.then(event_payload_decoder())
  decode.success(event.SessionEvent(
    cursor: cursor,
    at_ms: at_ms,
    session_id: session_id,
    issue_id: issue_id,
    payload: payload,
  ))
}

fn event_payload_decoder() -> decode.Decoder(event.EventPayload) {
  use kind <- decode.field("kind", event_kind_decoder())
  use name_string <- decode.field("name", decode.string)
  use turn <- decode.optional_field("turn", None, decode.optional(decode.int))
  use pi_type <- decode.optional_field(
    "pi_type",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use recovery <- decode.optional_field(
    "recovery",
    None,
    decode.optional(recovery_info_decoder()),
  )
  use request_id <- decode.optional_field(
    "request_id",
    None,
    decode.optional(decode.string),
  )
  use method <- decode.optional_field(
    "method",
    None,
    decode.optional(decode.string),
  )
  use tool_name <- decode.optional_field(
    "tool_name",
    None,
    decode.optional(decode.string),
  )
  use tool_input <- decode.optional_field(
    "tool_input",
    None,
    decode.optional(decode.string),
  )
  use tool_output <- decode.optional_field(
    "tool_output",
    None,
    decode.optional(decode.string),
  )
  use tool_status <- decode.optional_field(
    "tool_status",
    None,
    decode.optional(decode.string),
  )
  use tokens <- decode.optional_field(
    "tokens",
    session_tokens.zero_token_totals(),
    token_totals_decoder(),
  )
  use turn_status_name <- decode.optional_field(
    "turn_status",
    None,
    decode.optional(decode.string),
  )
  use turn_started_at_ms <- decode.optional_field(
    "turn_started_at_ms",
    None,
    decode.optional(decode.int),
  )
  use turn_finished_at_ms <- decode.optional_field(
    "turn_finished_at_ms",
    None,
    decode.optional(decode.int),
  )
  use turn_duration_ms <- decode.optional_field(
    "turn_duration_ms",
    None,
    decode.optional(decode.int),
  )
  use token_delta <- decode.optional_field(
    "token_delta",
    session_tokens.zero_token_totals(),
    token_totals_decoder(),
  )
  use reason_name <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use raw_json <- decode.optional_field(
    "raw_json",
    None,
    decode.optional(redacted_raw_json_decoder()),
  )
  let payload =
    event.decoded_payload(
      kind,
      name_string,
      turn,
      pi_type,
      message,
      recovery,
      request_id,
      method,
      tool_name,
      tool_input,
      tool_output,
      tool_status,
      tokens,
      turn_status_from_optional_string(turn_status_name),
      turn_started_at_ms,
      turn_finished_at_ms,
      turn_duration_ms,
      token_delta,
      turn_reason_from_optional_string(reason_name),
      raw_json,
    )
  decode.success(payload)
}

fn token_totals_decoder() -> decode.Decoder(session_tokens.TokenTotals) {
  use input <- decode.optional_field("input", 0, decode.int)
  use output <- decode.optional_field("output", 0, decode.int)
  use cache_read <- decode.optional_field("cache_read", 0, decode.int)
  use cache_write <- decode.optional_field("cache_write", 0, decode.int)
  use total <- decode.optional_field("total", 0, decode.int)
  decode.success(session_tokens.TokenTotals(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  ))
}

fn redacted_raw_json_decoder() -> decode.Decoder(event.RedactedRawJson) {
  use value <- decode.field("value", decode.string)
  use truncated <- decode.field("truncated", decode.bool)
  decode.success(event.RedactedRawJson(value: value, truncated: truncated))
}

fn event_kind_decoder() -> decode.Decoder(event.EventKind) {
  use name <- decode.then(decode.string)
  decode.success(kind_from_string(name))
}

fn kind_from_string(name: String) -> event.EventKind {
  case name {
    "lifecycle" -> event.Lifecycle
    "pi" -> event.Pi
    "assistant_message" -> event.AssistantMessage
    "tool" -> event.Tool
    "ui_request" -> event.UiRequest
    "ui_response" -> event.UiResponse
    "token_stats" -> event.TokenStats
    "error" -> event.Error
    "pi_raw" -> event.PiRaw
    "turn" -> event.Turn
    _ -> event.PiRaw
  }
}

fn turn_status_from_optional_string(
  value: Option(String),
) -> Option(turn_telemetry.TurnStatus) {
  case value {
    Some(value) -> turn_telemetry.status_from_string(value)
    None -> None
  }
}

fn turn_reason_from_optional_string(
  value: Option(String),
) -> Option(turn_telemetry.TurnReason) {
  case value {
    Some(value) -> turn_telemetry.reason_from_string(value)
    None -> None
  }
}

fn status_from_string(
  name: String,
  exit_reason: Option(String),
) -> Result(event.SessionStatus, Nil) {
  case name {
    "preparing" -> Ok(event.Preparing)
    "probing" -> Ok(event.Probing)
    "running" -> Ok(event.Running)
    "waiting_ui" -> Ok(event.WaitingUi)
    "stopping" -> Ok(event.Stopping)
    "exited" ->
      case exit_reason {
        Some(reason) ->
          case session_reason.from_string(reason) {
            Ok(reason) -> Ok(event.Exited(reason))
            Error(Nil) -> Ok(event.Exited(session_reason.Failed))
          }
        None -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn option_dynamic_to_json(data: Option(Dynamic)) -> Option(json.Json) {
  case data {
    Some(value) -> Some(dynamic_to_json(value))
    None -> None
  }
}

@external(erlang, "scherzo_control_ffi", "dynamic_to_json")
fn dynamic_to_json(value: Dynamic) -> json.Json
