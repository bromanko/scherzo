import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleam/string
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
  assert command.command_name(command.RetryIssueStartFresh(issue_ref, "drift"))
    == "retry_start_fresh"
  assert command.command_target(command.RetryIssueStartFresh(issue_ref, "drift"))
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
  assert command.command_name(command.RecollectWorkflowOutputs("run-1"))
    == "recollect_outputs"
  assert command.command_target(command.RecollectWorkflowOutputs("run-1"))
    == Some("run:run-1")
  assert command.command_name(command.RunFinalize(
      run_id: "run-1",
      validate: True,
      outputs: command.RunFinalizeOutputsAuto,
      publish: True,
      update_tracker: True,
      dry_run: True,
      reason: "operator salvage",
      allow_unpublished: False,
    ))
    == "run_finalize"
  assert command.command_target(command.RunFinalize(
      run_id: "run-1",
      validate: True,
      outputs: command.RunFinalizeOutputsAuto,
      publish: True,
      update_tracker: True,
      dry_run: True,
      reason: "operator salvage",
      allow_unpublished: False,
    ))
    == Some("run:run-1")
  assert command.command_name(command.RetryArtifactPublication(
      "run-1",
      Some("review_doc"),
    ))
    == "retry_artifact_publication"
  assert command.command_target(command.RetryArtifactPublication(
      "run-1",
      Some("review_doc"),
    ))
    == Some("run:run-1:review_doc")
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
  assert result.operation_id == None
}

pub fn operator_command_codec_roundtrips_all_variants_test() {
  assert_command_roundtrip(command.PauseDispatch)
  assert_command_roundtrip(command.ResumeDispatch)
  assert_command_roundtrip(command.ReloadWorkflow)
  assert_command_roundtrip(command.RetryIssue(command.IssueId("issue-123")))
  assert_command_roundtrip(
    command.RetryIssue(command.IssueIdentifier("ABC-123")),
  )
  assert_command_roundtrip(command.RetryIssueStartFresh(
    command.IssueIdentifier("ABC-123"),
    "workflow drift",
  ))
  assert_command_roundtrip(command.RetryWorkflowStep(
    command.RetryWorkflowStepAutoTarget("ABC-123"),
    None,
  ))
  assert_command_roundtrip(command.RetryWorkflowStep(
    command.RetryWorkflowStepIssueRef(command.IssueIdentifier("ABC-123")),
    Some("build"),
  ))
  assert_command_roundtrip(command.RetryWorkflowStep(
    command.RetryWorkflowStepRunId("run-1"),
    None,
  ))
  assert_command_roundtrip(command.RetryWorkflowStep(
    command.RetryWorkflowStepRunId("run-1"),
    Some("step-2"),
  ))
  assert_command_roundtrip(command.RecollectWorkflowOutputs("run-1"))
  assert_command_roundtrip(command.RunFinalize(
    run_id: "run-1",
    validate: True,
    outputs: command.RunFinalizeOutputsAuto,
    publish: True,
    update_tracker: True,
    dry_run: False,
    reason: "operator salvage",
    allow_unpublished: False,
  ))
  assert_command_roundtrip(command.RetryArtifactPublication("run-1", None))
  assert_command_roundtrip(command.RetryArtifactPublication(
    "run-1",
    Some("review_doc"),
  ))
  assert_command_roundtrip(command.ParkIssue(
    command.IssueId("issue-123"),
    "manual hold",
  ))
  assert_command_roundtrip(
    command.UnparkIssue(command.IssueIdentifier("ABC-123")),
  )
  assert_command_roundtrip(command.AbortSession("session-1"))
  assert_command_roundtrip(command.StopAfterCurrentTurn("session-1"))
  assert_command_roundtrip(command.PromptSession("session-1", "continue please"))
  assert_command_roundtrip(command.RespondUi(
    "session-1",
    "ui-1",
    command.UiCancel,
  ))
  assert_command_roundtrip(command.RespondUi(
    "session-1",
    "ui-2",
    command.UiValue("choice"),
  ))
  assert_command_roundtrip(command.RunScheduleNow("nightly-repair"))
  assert_command_roundtrip(
    command.WorkItemAction(
      command.WorkItemActionRequest(
        action_id: "work_subtask.cancel",
        action_instance_id: "wia_123",
        target_kind: "workflow_subtask",
        target_provider: Some("linear"),
        target_id: "issue-123",
        observed_fingerprint: "fingerprint-123",
        idempotency_key: "idempotency-123",
        params: [#("confirm", "true")],
      ),
    ),
  )
}

pub fn operator_command_codec_preserves_free_form_text_whitespace_test() {
  assert_command_roundtrip(command.ParkIssue(
    command.IssueId("issue-123"),
    "  manual hold  ",
  ))
  assert_command_roundtrip(command.PromptSession(
    "session-1",
    "  keep spacing  ",
  ))
}

pub fn invalid_operator_command_payloads_return_stable_errors_test() {
  assert_invalid_command("{}", "invalid_command")
  assert_invalid_command("{\"type\":\"unknown\"}", "unknown_command")
  assert_invalid_command("{\"type\":\"retry\"}", "invalid_command")
  assert_invalid_command(
    "{\"type\":\"retry\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"ABC-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"retry\",\"issue_identifier\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"retry_start_fresh\",\"issue_identifier\":\"ABC-1\",\"reason\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command("{\"type\":\"retry_step\"}", "invalid_command")
  assert_invalid_command(
    "{\"type\":\"retry_step\",\"target\":\"ABC-1\",\"run_id\":\"run-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"retry_step\",\"target\":\"ABC-1\",\"issue_id\":\"issue-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"retry_step\",\"step_id\":\"   \",\"target\":\"ABC-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"recollect_outputs\",\"run_id\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"run_finalize\",\"run_id\":\"run-1\",\"validate\":true,\"outputs\":\"bogus\",\"publish\":true,\"update_tracker\":true,\"reason\":\"ok\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"retry_artifact_publication\",\"run_id\":\"run-1\",\"publication_id\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"park\",\"issue_id\":\"issue-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"park\",\"issue_id\":\"issue-1\",\"reason\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"prompt\",\"session_id\":\"session-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"prompt\",\"session_id\":\"session-1\",\"message\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"respond_ui\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\",\"cancel\":false}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"respond_ui\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\",\"cancel\":true,\"value\":\"ok\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"respond_ui\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"schedule_run_now\",\"job_id\":\"   \"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"work_item_action\",\"action_id\":\"work_subtask.cancel\"}",
    "invalid_command",
  )
  assert_invalid_command(
    "{\"type\":\"work_item_action\",\"action_id\":\"work_subtask.cancel\",\"action_instance_id\":\"wia_1\",\"target_kind\":\"workflow_subtask\",\"target_id\":\"issue-1\",\"observed_fingerprint\":\"fp\",\"idempotency_key\":\"key\",\"params\":{}}",
    "invalid_command",
  )
}

pub fn command_result_codec_roundtrips_statuses_and_reasons_test() {
  assert_result_roundtrip(command.CommandResult(
    command: "pause",
    status: command.Applied,
    target: None,
    message: Some("paused"),
    operation_id: None,
  ))
  assert_result_roundtrip(command.CommandResult(
    command: "retry",
    status: command.Queued,
    target: Some("ABC-123"),
    message: Some("queued"),
    operation_id: None,
  ))
  assert_result_roundtrip(command.CommandResult(
    command: "retry_step",
    status: command.Queued,
    target: Some("run-1"),
    message: Some("queued durable repair"),
    operation_id: Some("op-123"),
  ))
  assert_result_roundtrip(command.CommandResult(
    command: "abort",
    status: command.Rejected("busy"),
    target: Some("session-1"),
    message: Some("session busy"),
    operation_id: None,
  ))
  assert_result_roundtrip(command.CommandResult(
    command: "reload",
    status: command.NotFound,
    target: None,
    message: Some("missing"),
    operation_id: None,
  ))
  assert_result_roundtrip(command.CommandResult(
    command: "prompt",
    status: command.NotAllowed("policy"),
    target: Some("session-1"),
    message: Some("policy denied"),
    operation_id: None,
  ))
}

pub fn command_result_decoder_accepts_missing_operation_id_test() {
  let assert Ok(dynamic) =
    json.parse(
      "{\"command\":\"retry_step\",\"status\":\"queued\",\"target\":\"run-1\",\"message\":\"queued durable repair\"}",
      decode.dynamic,
    )
  let assert Ok(result) = command.decode_command_result_dynamic(dynamic)
  assert result.command == "retry_step"
  assert command.status_to_string(result.status) == "queued"
  assert result.operation_id == None
}

pub fn queued_operation_helper_sets_operation_id_test() {
  let result =
    command.queued_operation(
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      "op-456",
      Some("queued durable repair"),
    )

  assert result.command == "retry_step"
  assert command.status_to_string(result.status) == "queued"
  assert result.target == Some("run-1")
  assert result.message == Some("queued durable repair")
  assert result.operation_id == Some("op-456")
}

pub fn invalid_command_result_payloads_return_stable_errors_test() {
  assert_invalid_result("{\"command\":\"prompt\"}", "invalid_result")
  assert_invalid_result(
    "{\"command\":\"prompt\",\"status\":123}",
    "invalid_result",
  )
  assert_invalid_result(
    "{\"command\":\"prompt\",\"status\":\"future_status\"}",
    "invalid_result",
  )
}

fn assert_command_roundtrip(operator_command: command.OperatorCommand) -> Nil {
  let encoded =
    command.operator_command_to_json(operator_command) |> json.to_string
  let assert Ok(dynamic) = json.parse(encoded, decode.dynamic)
  let assert Ok(decoded) = command.decode_operator_command_dynamic(dynamic)
  assert decoded == operator_command
}

fn assert_invalid_command(line: String, expected_code: String) -> Nil {
  let assert Ok(dynamic) = json.parse(line, decode.dynamic)
  let assert Error(command.CodecError(code: code, message: message)) =
    command.decode_operator_command_dynamic(dynamic)
  assert code == expected_code
  assert string.length(message) > 0
}

fn assert_result_roundtrip(result: command.CommandResult) -> Nil {
  let encoded = command.command_result_to_json(result) |> json.to_string
  let assert Ok(dynamic) = json.parse(encoded, decode.dynamic)
  let assert Ok(decoded) = command.decode_command_result_dynamic(dynamic)
  assert decoded == result
}

fn assert_invalid_result(line: String, expected_code: String) -> Nil {
  let assert Ok(dynamic) = json.parse(line, decode.dynamic)
  let assert Error(command.CodecError(code: code, message: message)) =
    command.decode_command_result_dynamic(dynamic)
  assert code == expected_code
  assert string.length(message) > 0
}
