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

pub type OperatorCommand {
  PauseDispatch
  ResumeDispatch
  ReloadWorkflow
  RetryIssue(IssueRef)
  RetryWorkflowStep(target: RetryWorkflowStepTarget, step_id: Option(String))
  ParkIssue(IssueRef, reason: String)
  UnparkIssue(IssueRef)
  AbortSession(session_id: String)
  StopAfterCurrentTurn(session_id: String)
  CleanupOrphanSteps(run_id: String, dry_run: Bool)
  PromptSession(session_id: String, message: String)
  RespondUi(session_id: String, request_id: String, response: UiResponse)
  RunScheduleNow(job_id: String)
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
    reason: Option(String),
    session_id: Option(String),
    message: Option(String),
    request_id: Option(String),
    cancel: Option(Bool),
    value: Option(String),
    job_id: Option(String),
    dry_run: Option(Bool),
  )
}

type CommandResultFields {
  CommandResultFields(
    command_name: String,
    status_name: String,
    target: Option(String),
    message: Option(String),
    reason: Option(String),
  )
}

pub fn command_name(command: OperatorCommand) -> String {
  case command {
    PauseDispatch -> "pause"
    ResumeDispatch -> "resume"
    ReloadWorkflow -> "reload"
    RetryIssue(_) -> "retry"
    RetryWorkflowStep(_, _) -> "retry_step"
    ParkIssue(_, _) -> "park"
    UnparkIssue(_) -> "unpark"
    AbortSession(_) -> "abort"
    StopAfterCurrentTurn(_) -> "stop_after_current_turn"
    CleanupOrphanSteps(_, _) -> "cleanup_orphan_steps"
    PromptSession(_, _) -> "prompt"
    RespondUi(_, _, _) -> "respond_ui"
    RunScheduleNow(_) -> "schedule_run_now"
  }
}

pub fn command_target(command: OperatorCommand) -> Option(String) {
  case command {
    PauseDispatch | ResumeDispatch | ReloadWorkflow -> None
    RetryIssue(issue_ref) | ParkIssue(issue_ref, _) | UnparkIssue(issue_ref) ->
      Some(issue_ref_to_string(issue_ref))
    RetryWorkflowStep(target, _) ->
      Some(retry_workflow_step_target_to_string(target))
    AbortSession(session_id)
    | StopAfterCurrentTurn(session_id)
    | PromptSession(session_id, _)
    | RespondUi(session_id, _, _) -> Some(session_id)
    CleanupOrphanSteps(run_id, _) -> Some("run:" <> run_id)
    RunScheduleNow(job_id) -> Some(job_id)
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
    RetryWorkflowStep(target, step_id) ->
      list.append(
        retry_workflow_step_entries(target, step_id),
        base_command_entries("retry_step"),
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
  ]
  let with_target = case result.target {
    Some(target) -> [#("target", json.string(target)), ..base]
    None -> base
  }
  let with_message = case result.message {
    Some(message) -> [#("message", json.string(message)), ..with_target]
    None -> with_target
  }
  let entries = case status_reason(result.status) {
    Some(reason) -> [#("reason", json.string(reason)), ..with_message]
    None -> with_message
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

fn ui_response_entries(response: UiResponse) -> List(#(String, json.Json)) {
  case response {
    UiCancel -> [#("cancel", json.bool(True))]
    UiValue(value) -> [#("value", json.string(value))]
  }
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
  decode.success(CommandFields(
    type_: type_,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    target: target,
    run_id: run_id,
    step_id: step_id,
    reason: reason,
    session_id: session_id,
    message: message,
    request_id: request_id,
    cancel: cancel,
    value: value,
    job_id: job_id,
    dry_run: dry_run,
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
        "retry_step" -> {
          use target <- result.try(required_retry_workflow_step_target(fields))
          use step_id <- result.try(optional_step_id(fields))
          Ok(RetryWorkflowStep(target, step_id))
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

fn command_dry_run(fields: CommandFields) -> Bool {
  case fields.dry_run {
    Some(value) -> value
    None -> True
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
  decode.success(CommandResultFields(
    command_name: command_name,
    status_name: status_name,
    target: target,
    message: message,
    reason: reason,
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
      ))
    Error(Nil) ->
      decode.failure(
        CommandResult(
          command: fields.command_name,
          status: Rejected("invalid_status"),
          target: fields.target,
          message: fields.message,
        ),
        expected: "known command result status",
      )
  }
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
