import gleam/option.{type Option, None, Some}

pub type IssueRef {
  IssueId(String)
  IssueIdentifier(String)
}

pub type UiResponse {
  UiCancel
  UiValue(String)
}

pub type OperatorCommand {
  PauseDispatch
  ResumeDispatch
  ReloadWorkflow
  RunScheduleNow(job_id: String)
  RetryIssue(IssueRef)
  ParkIssue(IssueRef, reason: String)
  UnparkIssue(IssueRef)
  AbortSession(session_id: String)
  StopAfterCurrentTurn(session_id: String)
  PromptSession(session_id: String, message: String)
  RespondUi(session_id: String, request_id: String, response: UiResponse)
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

pub fn command_name(command: OperatorCommand) -> String {
  case command {
    PauseDispatch -> "pause"
    ResumeDispatch -> "resume"
    ReloadWorkflow -> "reload"
    RunScheduleNow(_) -> "schedule_run_now"
    RetryIssue(_) -> "retry"
    ParkIssue(_, _) -> "park"
    UnparkIssue(_) -> "unpark"
    AbortSession(_) -> "abort"
    StopAfterCurrentTurn(_) -> "stop_after_current_turn"
    PromptSession(_, _) -> "prompt"
    RespondUi(_, _, _) -> "respond_ui"
  }
}

pub fn command_target(command: OperatorCommand) -> Option(String) {
  case command {
    PauseDispatch | ResumeDispatch | ReloadWorkflow -> None
    RunScheduleNow(job_id) -> Some(job_id)
    RetryIssue(issue_ref) | ParkIssue(issue_ref, _) | UnparkIssue(issue_ref) ->
      Some(issue_ref_to_string(issue_ref))
    AbortSession(session_id)
    | StopAfterCurrentTurn(session_id)
    | PromptSession(session_id, _)
    | RespondUi(session_id, _, _) -> Some(session_id)
  }
}

pub fn issue_ref_to_string(issue_ref: IssueRef) -> String {
  case issue_ref {
    IssueId(id) -> id
    IssueIdentifier(identifier) -> identifier
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
