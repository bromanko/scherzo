import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type IssueRef {
  IssueId(String)
  IssueIdentifier(String)
}

pub type RetryWorkflowStepTarget {
  RetryWorkflowStepAutoTarget(String)
  RetryWorkflowStepIssueRef(IssueRef)
  RetryWorkflowStepRunId(String)
}

pub type UiResponse {
  UiCancel
  UiValue(String)
}

pub type WorkItemActionRequest {
  WorkItemActionRequest(
    action_id: String,
    action_instance_id: String,
    target_kind: String,
    target_provider: Option(String),
    target_id: String,
    observed_fingerprint: String,
    idempotency_key: String,
    params: List(#(String, String)),
  )
}

pub type RunFinalizeOutputs {
  RunFinalizeOutputsAuto
}

pub type OperatorCommand {
  PauseDispatch
  ResumeDispatch
  ReloadWorkflow
  RetryIssue(IssueRef)
  RetryIssueStartFresh(IssueRef, reason: String)
  RetryWorkflowStep(target: RetryWorkflowStepTarget, step_id: Option(String))
  RetryWorkflowStepExact(
    target: RetryWorkflowStepTarget,
    step_id: Option(String),
  )
  RecollectWorkflowOutputs(run_id: String)
  RunFinalize(
    run_id: String,
    validate: Bool,
    outputs: RunFinalizeOutputs,
    publish: Bool,
    update_tracker: Bool,
    dry_run: Bool,
    reason: String,
    allow_unpublished: Bool,
  )
  RetryArtifactPublication(run_id: String, publication_id: Option(String))
  ParkIssue(IssueRef, reason: String)
  UnparkIssue(IssueRef)
  AbortSession(session_id: String)
  StopAfterCurrentTurn(session_id: String)
  CleanupOrphanSteps(run_id: String, dry_run: Bool)
  PromptSession(session_id: String, message: String)
  RespondUi(session_id: String, request_id: String, response: UiResponse)
  RunScheduleNow(job_id: String)
  ReenableSchedule(job_id: String)
  WorkItemAction(WorkItemActionRequest)
}

pub type CommandStatus {
  Applied
  Queued
  Rejected(reason: String)
  NotFound
  NotAllowed(reason: String)
}

pub type CommandResult {
  CommandResult(
    command: String,
    status: CommandStatus,
    target: Option(String),
    message: Option(String),
    operation_id: Option(String),
  )
}

pub type CodecError {
  CodecError(code: String, message: String)
}

type CommandFields {
  CommandFields(
    type_: Option(String),
    issue_id: Option(String),
    issue_identifier: Option(String),
    target: Option(String),
    run_id: Option(String),
    step_id: Option(String),
    publication_id: Option(String),
    reason: Option(String),
    session_id: Option(String),
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

type CommandResultFields {
  CommandResultFields(
    command_name: String,
    status_name: String,
    target: Option(String),
    message: Option(String),
    reason: Option(String),
    operation_id: Option(String),
  )
}

pub fn command_name(command: OperatorCommand) -> String {
  case command {
    PauseDispatch -> "pause"
    ResumeDispatch -> "resume"
    ReloadWorkflow -> "reload"
    RetryIssue(_) -> "retry"
    RetryIssueStartFresh(_, _) -> "retry_start_fresh"
    RetryWorkflowStep(_, _) -> "retry_step"
    RetryWorkflowStepExact(_, _) -> "retry_step_exact"
    RecollectWorkflowOutputs(_) -> "recollect_outputs"
    RunFinalize(..) -> "run_finalize"
    RetryArtifactPublication(_, _) -> "retry_artifact_publication"
    ParkIssue(_, _) -> "park"
    UnparkIssue(_) -> "unpark"
    AbortSession(_) -> "abort"
    StopAfterCurrentTurn(_) -> "stop_after_current_turn"
    CleanupOrphanSteps(_, _) -> "cleanup_orphan_steps"
    PromptSession(_, _) -> "prompt"
    RespondUi(_, _, _) -> "respond_ui"
    RunScheduleNow(_) -> "schedule_run_now"
    ReenableSchedule(_) -> "schedule_reenable"
    WorkItemAction(_) -> "work_item_action"
  }
}

pub fn command_target(command: OperatorCommand) -> Option(String) {
  case command {
    PauseDispatch | ResumeDispatch | ReloadWorkflow -> None
    RetryIssue(issue_ref)
    | RetryIssueStartFresh(issue_ref, _)
    | ParkIssue(issue_ref, _)
    | UnparkIssue(issue_ref) -> Some(issue_ref_to_string(issue_ref))
    RetryWorkflowStep(target, _) | RetryWorkflowStepExact(target, _) ->
      Some(retry_workflow_step_target_to_string(target))
    RecollectWorkflowOutputs(run_id) -> Some("run:" <> run_id)
    RunFinalize(run_id: run_id, ..) -> Some("run:" <> run_id)
    RetryArtifactPublication(run_id, publication_id) ->
      Some(retry_artifact_publication_target_to_string(run_id, publication_id))
    AbortSession(session_id)
    | StopAfterCurrentTurn(session_id)
    | PromptSession(session_id, _)
    | RespondUi(session_id, _, _) -> Some(session_id)
    CleanupOrphanSteps(run_id, _) -> Some("run:" <> run_id)
    RunScheduleNow(job_id) | ReenableSchedule(job_id) -> Some(job_id)
    WorkItemAction(request) ->
      Some(request.target_kind <> ":" <> request.target_id)
  }
}

pub fn issue_ref_to_string(issue_ref: IssueRef) -> String {
  case issue_ref {
    IssueId(id) -> id
    IssueIdentifier(identifier) -> identifier
  }
}

pub fn retry_workflow_step_target_to_string(
  target: RetryWorkflowStepTarget,
) -> String {
  case target {
    RetryWorkflowStepAutoTarget(target) -> target
    RetryWorkflowStepIssueRef(issue_ref) -> issue_ref_to_string(issue_ref)
    RetryWorkflowStepRunId(run_id) -> run_id
  }
}

fn retry_artifact_publication_target_to_string(
  run_id: String,
  publication_id: Option(String),
) -> String {
  case publication_id {
    Some(publication_id) -> "run:" <> run_id <> ":" <> publication_id
    None -> "run:" <> run_id
  }
}

pub fn status_to_string(status: CommandStatus) -> String {
  case status {
    Applied -> "applied"
    Queued -> "queued"
    Rejected(_) -> "rejected"
    NotFound -> "not_found"
    NotAllowed(_) -> "not_allowed"
  }
}

pub fn status_reason(status: CommandStatus) -> Option(String) {
  case status {
    Rejected(reason) | NotAllowed(reason) -> Some(reason)
    Applied | Queued | NotFound -> None
  }
}

pub fn status_from_string(
  name: String,
  reason: Option(String),
) -> CommandStatus {
  case name {
    "applied" -> Applied
    "queued" -> Queued
    "rejected" -> Rejected(option.unwrap(reason, "rejected"))
    "not_found" -> NotFound
    "not_allowed" -> NotAllowed(option.unwrap(reason, "not_allowed"))
    _ -> Rejected("unknown_status:" <> name)
  }
}

pub fn operator_command_to_json(
  operator_command: OperatorCommand,
) -> json.Json {
  case operator_command {
    PauseDispatch -> base_command_entries("pause") |> json.object
    ResumeDispatch -> base_command_entries("resume") |> json.object
    ReloadWorkflow -> base_command_entries("reload") |> json.object
    RetryIssue(issue_ref) ->
      list.append(issue_ref_entries(issue_ref), base_command_entries("retry"))
      |> json.object
    RetryIssueStartFresh(issue_ref, reason) ->
      list.append(
        [#("reason", json.string(reason)), ..issue_ref_entries(issue_ref)],
        base_command_entries("retry_start_fresh"),
      )
      |> json.object
    RetryWorkflowStep(target, step_id) ->
      list.append(
        retry_workflow_step_entries(target, step_id),
        base_command_entries("retry_step"),
      )
      |> json.object
    RetryWorkflowStepExact(target, step_id) ->
      list.append(
        retry_workflow_step_entries(target, step_id),
        base_command_entries("retry_step_exact"),
      )
      |> json.object
    RecollectWorkflowOutputs(run_id) ->
      [
        #("run_id", json.string(run_id)),
        ..base_command_entries("recollect_outputs")
      ]
      |> json.object
    RunFinalize(
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
        ..base_command_entries("run_finalize")
      ]
      |> json.object
    RetryArtifactPublication(run_id, publication_id) ->
      list.append(
        retry_artifact_publication_entries(run_id, publication_id),
        base_command_entries("retry_artifact_publication"),
      )
      |> json.object
    ParkIssue(issue_ref, reason) ->
      list.append(
        [#("reason", json.string(reason)), ..issue_ref_entries(issue_ref)],
        base_command_entries("park"),
      )
      |> json.object
    UnparkIssue(issue_ref) ->
      list.append(issue_ref_entries(issue_ref), base_command_entries("unpark"))
      |> json.object
    AbortSession(session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_command_entries("abort")
      ]
      |> json.object
    StopAfterCurrentTurn(session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_command_entries("stop_after_current_turn")
      ]
      |> json.object
    CleanupOrphanSteps(run_id, dry_run) ->
      [
        #("run_id", json.string(run_id)),
        #("dry_run", json.bool(dry_run)),
        ..base_command_entries("cleanup_orphan_steps")
      ]
      |> json.object
    PromptSession(session_id, message) ->
      [
        #("session_id", json.string(session_id)),
        #("message", json.string(message)),
        ..base_command_entries("prompt")
      ]
      |> json.object
    RespondUi(session_id, request_id, response) ->
      list.append(
        [
          #("session_id", json.string(session_id)),
          #("request_id", json.string(request_id)),
          ..ui_response_entries(response)
        ],
        base_command_entries("respond_ui"),
      )
      |> json.object
    RunScheduleNow(job_id) ->
      [
        #("job_id", json.string(job_id)),
        ..base_command_entries("schedule_run_now")
      ]
      |> json.object
    ReenableSchedule(job_id) ->
      [
        #("job_id", json.string(job_id)),
        ..base_command_entries("schedule_reenable")
      ]
      |> json.object
    WorkItemAction(request) ->
      list.append(
        work_item_action_entries(request),
        base_command_entries("work_item_action"),
      )
      |> json.object
  }
}

pub fn decode_operator_command_dynamic(
  value: Dynamic,
) -> Result(OperatorCommand, CodecError) {
  case decode.run(value, command_fields_decoder()) {
    Ok(fields) -> operator_command_from_fields(fields)
    Error(_) -> Error(CodecError("invalid_command", "invalid command payload"))
  }
}

pub fn command_result_to_json(result: CommandResult) -> json.Json {
  let base = [
    #("command", json.string(result.command)),
    #("status", json.string(status_to_string(result.status))),
    #("accepted", json.bool(True)),
  ]
  let with_target = case result.target {
    Some(target) -> [#("target", json.string(target)), ..base]
    None -> base
  }
  let with_message = case result.message {
    Some(message) -> [#("message", json.string(message)), ..with_target]
    None -> with_target
  }
  let with_operation_id = case result.operation_id {
    Some(operation_id) -> [
      #("operation_id", json.string(operation_id)),
      ..with_message
    ]
    None -> with_message
  }
  let entries = case status_reason(result.status) {
    Some(reason) -> [#("reason", json.string(reason)), ..with_operation_id]
    None -> with_operation_id
  }
  entries |> list.reverse |> json.object
}

pub fn command_result_decoder() -> decode.Decoder(CommandResult) {
  decode.then(
    command_result_fields_decoder(),
    command_result_from_fields_decoder,
  )
}

pub fn decode_command_result_dynamic(
  value: Dynamic,
) -> Result(CommandResult, CodecError) {
  case decode.run(value, command_result_decoder()) {
    Ok(result) -> Ok(result)
    Error(_) ->
      Error(CodecError("invalid_result", "invalid command result payload"))
  }
}

pub fn result_for(
  operator_command: OperatorCommand,
  status: CommandStatus,
  message: Option(String),
) -> CommandResult {
  CommandResult(
    command: command_name(operator_command),
    status: status,
    target: command_target(operator_command),
    message: message,
    operation_id: None,
  )
}

pub fn applied(
  operator_command: OperatorCommand,
  message: Option(String),
) -> CommandResult {
  result_for(operator_command, Applied, message)
}

pub fn queued(
  operator_command: OperatorCommand,
  message: Option(String),
) -> CommandResult {
  result_for(operator_command, Queued, message)
}

pub fn queued_operation(
  operator_command: OperatorCommand,
  operation_id: String,
  message: Option(String),
) -> CommandResult {
  CommandResult(
    ..queued(operator_command, message),
    operation_id: Some(operation_id),
  )
}

pub fn rejected(
  operator_command: OperatorCommand,
  reason: String,
  message: Option(String),
) -> CommandResult {
  result_for(operator_command, Rejected(reason), message)
}

pub fn not_found(
  operator_command: OperatorCommand,
  message: Option(String),
) -> CommandResult {
  result_for(operator_command, NotFound, message)
}

pub fn not_allowed(
  operator_command: OperatorCommand,
  reason: String,
  message: Option(String),
) -> CommandResult {
  result_for(operator_command, NotAllowed(reason), message)
}

fn base_command_entries(type_: String) -> List(#(String, json.Json)) {
  [#("type", json.string(type_))]
}

fn issue_ref_entries(issue_ref: IssueRef) -> List(#(String, json.Json)) {
  case issue_ref {
    IssueId(id) -> [#("issue_id", json.string(id))]
    IssueIdentifier(identifier) -> [
      #("issue_identifier", json.string(identifier)),
    ]
  }
}

fn retry_workflow_step_entries(
  target: RetryWorkflowStepTarget,
  step_id: Option(String),
) -> List(#(String, json.Json)) {
  let base = case target {
    RetryWorkflowStepAutoTarget(target) -> [#("target", json.string(target))]
    RetryWorkflowStepIssueRef(issue_ref) -> issue_ref_entries(issue_ref)
    RetryWorkflowStepRunId(run_id) -> [#("run_id", json.string(run_id))]
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

fn ui_response_entries(response: UiResponse) -> List(#(String, json.Json)) {
  case response {
    UiCancel -> [#("cancel", json.bool(True))]
    UiValue(value) -> [#("value", json.string(value))]
  }
}

fn work_item_action_entries(
  request: WorkItemActionRequest,
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
    #("params", json.array(request.params, of: param_to_json)),
  ]
}

fn param_to_json(param: #(String, String)) -> json.Json {
  let #(name, value) = param
  json.object([#("name", json.string(name)), #("value", json.string(value))])
}

fn command_fields_decoder() -> decode.Decoder(CommandFields) {
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
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
  use session_id <- decode.optional_field(
    "session_id",
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
  decode.success(CommandFields(
    type_: type_,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    target: target,
    run_id: run_id,
    step_id: step_id,
    publication_id: publication_id,
    reason: reason,
    session_id: session_id,
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

fn operator_command_from_fields(
  fields: CommandFields,
) -> Result(OperatorCommand, CodecError) {
  case fields.type_ {
    Some(type_) -> {
      let type_ = string.trim(type_)
      case type_ {
        "pause" -> Ok(PauseDispatch)
        "resume" -> Ok(ResumeDispatch)
        "reload" -> Ok(ReloadWorkflow)
        "retry" -> required_issue_ref(fields) |> result.map(RetryIssue)
        "retry_start_fresh" -> {
          use issue_ref <- result.try(required_issue_ref(fields))
          use reason <- result.try(required_reason(fields))
          Ok(RetryIssueStartFresh(issue_ref, reason))
        }
        "retry_step" -> {
          use target <- result.try(required_retry_workflow_step_target(fields))
          use step_id <- result.try(optional_step_id(fields))
          Ok(RetryWorkflowStep(target, step_id))
        }
        "retry_step_exact" -> {
          use target <- result.try(required_retry_workflow_step_target(fields))
          use step_id <- result.try(optional_step_id(fields))
          Ok(RetryWorkflowStepExact(target, step_id))
        }
        "recollect_outputs" ->
          required_run_id(fields) |> result.map(RecollectWorkflowOutputs)
        "run_finalize" -> {
          use run_id <- result.try(required_run_id(fields))
          use validate <- result.try(required_true_flag(
            fields.validate,
            "validate",
          ))
          use outputs <- result.try(required_run_finalize_outputs(fields))
          use publish <- result.try(required_true_flag(
            fields.publish,
            "publish",
          ))
          use update_tracker <- result.try(required_true_flag(
            fields.update_tracker,
            "update_tracker",
          ))
          use reason <- result.try(required_reason(fields))
          Ok(RunFinalize(
            run_id: run_id,
            validate: validate,
            outputs: outputs,
            publish: publish,
            update_tracker: update_tracker,
            dry_run: command_dry_run(fields),
            reason: reason,
            allow_unpublished: command_allow_unpublished(fields),
          ))
        }
        "retry_artifact_publication" -> {
          use run_id <- result.try(required_run_id(fields))
          use publication_id <- result.try(optional_publication_id(fields))
          Ok(RetryArtifactPublication(run_id, publication_id))
        }
        "park" -> {
          use issue_ref <- result.try(required_issue_ref(fields))
          use reason <- result.try(required_reason(fields))
          Ok(ParkIssue(issue_ref, reason))
        }
        "unpark" -> required_issue_ref(fields) |> result.map(UnparkIssue)
        "abort" -> required_session_id(fields) |> result.map(AbortSession)
        "stop_after_current_turn" ->
          required_session_id(fields) |> result.map(StopAfterCurrentTurn)
        "cleanup_orphan_steps" -> {
          use run_id <- result.try(required_run_id(fields))
          Ok(CleanupOrphanSteps(run_id, command_dry_run(fields)))
        }
        "prompt" -> {
          use session_id <- result.try(required_session_id(fields))
          use message <- result.try(required_message(fields))
          Ok(PromptSession(session_id, message))
        }
        "respond_ui" -> {
          use session_id <- result.try(required_session_id(fields))
          use request_id <- result.try(required_request_id(fields))
          use response <- result.try(required_ui_response(fields))
          Ok(RespondUi(session_id, request_id, response))
        }
        "schedule_run_now" ->
          required_job_id(fields) |> result.map(RunScheduleNow)
        "schedule_reenable" ->
          required_job_id(fields) |> result.map(ReenableSchedule)
        "work_item_action" ->
          required_work_item_action_request(fields)
          |> result.map(WorkItemAction)
        _ ->
          Error(CodecError("unknown_command", "unknown command type: " <> type_))
      }
    }
    None -> Error(CodecError("invalid_command", "missing type"))
  }
}

fn required_issue_ref(fields: CommandFields) -> Result(IssueRef, CodecError) {
  case fields.issue_id, fields.issue_identifier {
    Some(_), Some(_) ->
      invalid_command("provide issue_id or issue_identifier, not both")
    Some(issue_id), None ->
      trimmed_non_empty(issue_id, "issue reference must not be empty")
      |> result.map(IssueId)
    None, Some(identifier) ->
      trimmed_non_empty(identifier, "issue reference must not be empty")
      |> result.map(IssueIdentifier)
    None, None -> invalid_command("missing issue reference")
  }
}

fn required_retry_workflow_step_target(
  fields: CommandFields,
) -> Result(RetryWorkflowStepTarget, CodecError) {
  case fields.target, fields.run_id, fields.issue_id, fields.issue_identifier {
    Some(_), Some(_), _, _
    | Some(_), _, Some(_), _
    | Some(_), _, _, Some(_)
    | _, Some(_), Some(_), _
    | _, Some(_), _, Some(_)
    | _, _, Some(_), Some(_)
    ->
      invalid_command(
        "provide exactly one of target, run_id, issue_id, or issue_identifier",
      )
    Some(target), None, None, None ->
      trimmed_non_empty(target, "target must not be empty")
      |> result.map(RetryWorkflowStepAutoTarget)
    None, Some(run_id), None, None ->
      trimmed_non_empty(run_id, "run_id must not be empty")
      |> result.map(RetryWorkflowStepRunId)
    None, None, Some(_), None | None, None, None, Some(_) ->
      required_issue_ref(fields) |> result.map(RetryWorkflowStepIssueRef)
    None, None, None, None -> invalid_command("missing retry_step target")
  }
}

fn optional_step_id(
  fields: CommandFields,
) -> Result(Option(String), CodecError) {
  case fields.step_id {
    Some(step_id) ->
      trimmed_non_empty(step_id, "step_id must not be empty")
      |> result.map(Some)
    None -> Ok(None)
  }
}

fn optional_publication_id(
  fields: CommandFields,
) -> Result(Option(String), CodecError) {
  case fields.publication_id {
    Some(publication_id) ->
      trimmed_non_empty(publication_id, "publication_id must not be empty")
      |> result.map(Some)
    None -> Ok(None)
  }
}

fn required_true_flag(
  value: Option(Bool),
  name: String,
) -> Result(Bool, CodecError) {
  case value {
    Some(True) -> Ok(True)
    Some(False) | None -> invalid_command(name <> " must be true")
  }
}

fn required_run_finalize_outputs(
  fields: CommandFields,
) -> Result(RunFinalizeOutputs, CodecError) {
  case fields.outputs {
    Some("auto") -> Ok(RunFinalizeOutputsAuto)
    Some(_) -> invalid_command("outputs must be auto")
    None -> invalid_command("missing outputs")
  }
}

fn run_finalize_outputs_to_string(outputs: RunFinalizeOutputs) -> String {
  case outputs {
    RunFinalizeOutputsAuto -> "auto"
  }
}

fn required_reason(fields: CommandFields) -> Result(String, CodecError) {
  case fields.reason {
    Some(reason) -> free_form_non_empty(reason, "reason must not be empty")
    None -> invalid_command("missing reason")
  }
}

fn required_session_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.session_id {
    Some(session_id) ->
      trimmed_non_empty(session_id, "session_id must not be empty")
    None -> invalid_command("missing session_id")
  }
}

fn required_message(fields: CommandFields) -> Result(String, CodecError) {
  case fields.message {
    Some(message) -> free_form_non_empty(message, "message must not be empty")
    None -> invalid_command("missing message")
  }
}

fn required_request_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.request_id {
    Some(request_id) ->
      trimmed_non_empty(request_id, "request_id must not be empty")
    None -> invalid_command("missing request_id")
  }
}

fn required_job_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.job_id {
    Some(job_id) -> trimmed_non_empty(job_id, "job_id must not be empty")
    None -> invalid_command("missing job_id")
  }
}

fn required_run_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.run_id {
    Some(run_id) -> trimmed_non_empty(run_id, "run_id must not be empty")
    None -> invalid_command("missing run_id")
  }
}

fn required_work_item_action_request(
  fields: CommandFields,
) -> Result(WorkItemActionRequest, CodecError) {
  use action_id <- result.try(required_action_id(fields))
  use action_instance_id <- result.try(required_action_instance_id(fields))
  use target_kind <- result.try(required_target_kind(fields))
  use target_id <- result.try(required_target_id(fields))
  use observed_fingerprint <- result.try(required_observed_fingerprint(fields))
  use idempotency_key <- result.try(required_idempotency_key(fields))
  use params <- result.try(optional_params(fields))
  Ok(WorkItemActionRequest(
    action_id: action_id,
    action_instance_id: action_instance_id,
    target_kind: target_kind,
    target_provider: fields.target_provider,
    target_id: target_id,
    observed_fingerprint: observed_fingerprint,
    idempotency_key: idempotency_key,
    params: params,
  ))
}

fn required_action_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.action_id {
    Some(value) -> trimmed_non_empty(value, "action_id must not be empty")
    None -> invalid_command("missing action_id")
  }
}

fn required_action_instance_id(
  fields: CommandFields,
) -> Result(String, CodecError) {
  case fields.action_instance_id {
    Some(value) ->
      trimmed_non_empty(value, "action_instance_id must not be empty")
    None -> invalid_command("missing action_instance_id")
  }
}

fn required_target_kind(fields: CommandFields) -> Result(String, CodecError) {
  case fields.target_kind {
    Some(value) -> trimmed_non_empty(value, "target_kind must not be empty")
    None -> invalid_command("missing target_kind")
  }
}

fn required_target_id(fields: CommandFields) -> Result(String, CodecError) {
  case fields.target_id {
    Some(value) -> trimmed_non_empty(value, "target_id must not be empty")
    None -> invalid_command("missing target_id")
  }
}

fn required_observed_fingerprint(
  fields: CommandFields,
) -> Result(String, CodecError) {
  case fields.observed_fingerprint {
    Some(value) ->
      trimmed_non_empty(value, "observed_fingerprint must not be empty")
    None -> invalid_command("missing observed_fingerprint")
  }
}

fn required_idempotency_key(
  fields: CommandFields,
) -> Result(String, CodecError) {
  case fields.idempotency_key {
    Some(value) -> trimmed_non_empty(value, "idempotency_key must not be empty")
    None -> invalid_command("missing idempotency_key")
  }
}

fn optional_params(
  fields: CommandFields,
) -> Result(List(#(String, String)), CodecError) {
  case fields.params {
    Some(params) -> decode_params(params)
    None -> Ok([])
  }
}

fn command_dry_run(fields: CommandFields) -> Bool {
  case fields.dry_run {
    Some(value) -> value
    None -> True
  }
}

fn command_allow_unpublished(fields: CommandFields) -> Bool {
  case fields.allow_unpublished {
    Some(value) -> value
    None -> False
  }
}

fn required_ui_response(
  fields: CommandFields,
) -> Result(UiResponse, CodecError) {
  case fields.cancel, fields.value {
    Some(True), None -> Ok(UiCancel)
    Some(False), None -> invalid_command("cancel must be true when provided")
    None, Some(value) -> Ok(UiValue(value))
    Some(True), Some(_) -> invalid_command("provide cancel or value, not both")
    Some(False), Some(_) -> invalid_command("cancel must be true when provided")
    None, None -> invalid_command("missing UI response")
  }
}

fn command_result_fields_decoder() -> decode.Decoder(CommandResultFields) {
  use command_name <- decode.field("command", decode.string)
  use status_name <- decode.field("status", decode.string)
  use target <- decode.optional_field(
    "target",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use operation_id <- decode.optional_field(
    "operation_id",
    None,
    decode.optional(decode.string),
  )
  decode.success(CommandResultFields(
    command_name: command_name,
    status_name: status_name,
    target: target,
    message: message,
    reason: reason,
    operation_id: operation_id,
  ))
}

fn command_result_from_fields_decoder(
  fields: CommandResultFields,
) -> decode.Decoder(CommandResult) {
  case known_status_from_string(fields.status_name, fields.reason) {
    Ok(status) ->
      decode.success(CommandResult(
        command: fields.command_name,
        status: status,
        target: fields.target,
        message: fields.message,
        operation_id: fields.operation_id,
      ))
    Error(Nil) ->
      decode.failure(
        CommandResult(
          command: fields.command_name,
          status: Rejected("invalid_status"),
          target: fields.target,
          message: fields.message,
          operation_id: fields.operation_id,
        ),
        expected: "known command result status",
      )
  }
}

fn decode_params(
  value: Dynamic,
) -> Result(List(#(String, String)), CodecError) {
  case decode.run(value, decode.list(param_decoder())) {
    Ok(params) -> Ok(params)
    Error(_) ->
      invalid_command("params must be an array of {name, value} objects")
  }
}

fn param_decoder() -> decode.Decoder(#(String, String)) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  decode.success(#(name, value))
}

fn known_status_from_string(
  name: String,
  reason: Option(String),
) -> Result(CommandStatus, Nil) {
  case name {
    "applied" -> Ok(Applied)
    "queued" -> Ok(Queued)
    "rejected" -> Ok(Rejected(option.unwrap(reason, "rejected")))
    "not_found" -> Ok(NotFound)
    "not_allowed" -> Ok(NotAllowed(option.unwrap(reason, "not_allowed")))
    _ -> Error(Nil)
  }
}

fn trimmed_non_empty(
  value: String,
  message: String,
) -> Result(String, CodecError) {
  let value = string.trim(value)
  case value == "" {
    True -> invalid_command(message)
    False -> Ok(value)
  }
}

fn free_form_non_empty(
  value: String,
  message: String,
) -> Result(String, CodecError) {
  case string.trim(value) == "" {
    True -> invalid_command(message)
    False -> Ok(value)
  }
}

fn invalid_command(message: String) -> Result(a, CodecError) {
  Error(CodecError("invalid_command", message))
}
