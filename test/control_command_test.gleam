import gleam/option.{None, Some}
import scherzo/control/command

pub fn command_names_and_targets_are_stable_test() {
  let issue_ref = command.IssueIdentifier("ABC-123")

  assert command.command_name(command.PauseDispatch) == "pause"
  assert command.command_target(command.PauseDispatch) == None
  assert command.command_name(command.ResumeDispatch) == "resume"
  assert command.command_name(command.ReloadWorkflow) == "reload"

  assert command.command_name(command.RetryIssue(issue_ref)) == "retry"
  assert command.command_target(command.RetryIssue(issue_ref))
    == Some("ABC-123")
  assert command.command_name(command.RetryWorkflowStep(
      command.RetryWorkflowStepRunId("run-1"),
      Some("step-1"),
    ))
    == "retry_step"
  assert command.command_target(command.RetryWorkflowStep(
      command.RetryWorkflowStepAutoTarget("ABC-123"),
      None,
    ))
    == Some("ABC-123")
  assert command.command_name(command.ParkIssue(issue_ref, "manual")) == "park"
  assert command.command_target(command.ParkIssue(issue_ref, "manual"))
    == Some("ABC-123")
  assert command.command_name(command.UnparkIssue(issue_ref)) == "unpark"

  assert command.command_name(command.AbortSession("session-1")) == "abort"
  assert command.command_target(command.AbortSession("session-1"))
    == Some("session-1")
  assert command.command_name(command.StopAfterCurrentTurn("session-1"))
    == "stop_after_current_turn"
  assert command.command_name(command.PromptSession("session-1", "continue"))
    == "prompt"
  assert command.command_name(command.RespondUi(
      "session-1",
      "ui-1",
      command.UiCancel,
    ))
    == "respond_ui"
}

pub fn command_status_strings_are_json_safe_test() {
  assert command.status_to_string(command.Applied) == "applied"
  assert command.status_to_string(command.Queued) == "queued"
  assert command.status_to_string(command.Rejected("busy")) == "rejected"
  assert command.status_to_string(command.NotFound) == "not_found"
  assert command.status_to_string(command.NotAllowed("policy")) == "not_allowed"
  assert command.status_reason(command.Rejected("busy")) == Some("busy")
  assert command.status_reason(command.Applied) == None
}

pub fn result_constructor_uses_command_metadata_test() {
  let result =
    command.rejected(
      command.PromptSession("session-1", "secret prompt"),
      "busy",
      Some("turn is currently streaming"),
    )

  assert result.command == "prompt"
  assert result.target == Some("session-1")
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("busy")
  assert result.message == Some("turn is currently streaming")
}
