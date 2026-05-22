import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/case_support
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn run(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  let subject = first_task(fixture_tasks)
  let base_cases = [
    run_target_id_precedence_case(manifest, subject),
    run_target_name_only_case(manifest, subject),
    run_blank_target_case(manifest, subject),
    run_unknown_target_case(manifest, subject),
  ]
  case manifest_claims_reason(manifest) {
    True ->
      list_append(base_cases, [run_reason_propagation_case(manifest, subject)])
    False -> base_cases
  }
}

fn run_target_id_precedence_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-state-transition-target-id"
  let operation = "state_transitions.transition"
  let expected_summary =
    "transition should honor target_state_id when both id and name are present and return the normalized target state."
  case
    driver.invoke(
      manifest,
      transition_request(
        request_id: request_id,
        subject: subject,
        target_state_id: Some("doing"),
        target_state_name: "Doing",
        reason: "[marker state-target-id] verify target id precedence",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "state_transitions.transition.target_id_precedence",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      transition_receipt_case_result(
        id: "state_transitions.transition.target_id_precedence",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        expected_state_id: Some("doing"),
        expected_state_name: "Doing",
        invocation: invocation,
      )
  }
}

fn run_target_name_only_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-state-transition-target-name-only"
  let operation = "state_transitions.transition"
  let expected_summary =
    "transition should resolve a target by name when no target_state_id is provided."
  case
    driver.invoke(
      manifest,
      transition_request(
        request_id: request_id,
        subject: subject,
        target_state_id: None,
        target_state_name: "Done",
        reason: "[marker state-target-name-only] verify name-only target resolution",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "state_transitions.transition.target_name_only",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      transition_receipt_case_result(
        id: "state_transitions.transition.target_name_only",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        expected_state_id: Some("done"),
        expected_state_name: "Done",
        invocation: invocation,
      )
  }
}

fn run_blank_target_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-state-transition-blank-target"
  let operation = "state_transitions.transition"
  let expected_summary =
    "transition should reject blank target state names with a normalized permanent error."
  case
    driver.invoke(
      manifest,
      transition_request(
        request_id: request_id,
        subject: subject,
        target_state_id: None,
        target_state_name: "",
        reason: "[marker state-blank-target] verify blank-target normalization",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "state_transitions.transition.blank_target",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      expected_permanent_error_case_result(
        id: "state_transitions.transition.blank_target",
        operation: operation,
        expected_summary: expected_summary,
        invocation: invocation,
      )
  }
}

fn run_unknown_target_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-state-transition-unknown-target"
  let operation = "state_transitions.transition"
  let expected_summary =
    "transition should reject unknown target states with a normalized permanent error."
  case
    driver.invoke(
      manifest,
      transition_request(
        request_id: request_id,
        subject: subject,
        target_state_id: None,
        target_state_name: "Unknown target",
        reason: "[marker state-unknown-target] verify unknown-target normalization",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "state_transitions.transition.unknown_target",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      expected_permanent_error_case_result(
        id: "state_transitions.transition.unknown_target",
        operation: operation,
        expected_summary: expected_summary,
        invocation: invocation,
      )
  }
}

fn run_reason_propagation_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-state-transition-reason-propagation"
  let operation = "state_transitions.transition"
  let expected_summary =
    "transition should preserve the requested reason when the adapter claims state_transitions.reason support."
  case
    driver.invoke(
      manifest,
      transition_request(
        request_id: request_id,
        subject: subject,
        target_state_id: Some("doing"),
        target_state_name: "Doing",
        reason: "[marker state-reason-propagation] verify reason persistence",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "state_transitions.transition.reason_propagation",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      transition_receipt_case_result(
        id: "state_transitions.transition.reason_propagation",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        expected_state_id: Some("doing"),
        expected_state_name: "Doing",
        invocation: invocation,
      )
  }
}

fn transition_request(
  request_id request_id: String,
  subject subject: task.Task,
  target_state_id target_state_id: Option(String),
  target_state_name target_state_name: String,
  reason reason: String,
) -> types.DriverRequest {
  let task.Task(ref: ref, ..) = subject
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.StateTransitionsTransition,
    payload: types.StateTransitionPayload(
      transition: types.StateTransitionRequestPayload(
        task: ref,
        target_state_id: target_state_id,
        target_state_name: target_state_name,
        reason: reason,
      ),
    ),
  )
}

fn transition_receipt_case_result(
  id id: String,
  operation operation: String,
  expected_summary expected_summary: String,
  subject subject: task.Task,
  expected_state_id expected_state_id: Option(String),
  expected_state_name expected_state_name: String,
  invocation invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = case_support.response_request_id(response)
  case response {
    types.DriverResponseSuccess(
      result: types.StateTransitionResult(transition: receipt),
      ..,
    ) ->
      case
        validate_transition_receipt(
          subject,
          receipt,
          expected_state_id,
          expected_state_name,
        )
      {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "state transition returned a normalized receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: actual_summary,
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        Error(validation_error) ->
          case_support.failed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "state transition returned a malformed or mismatched receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: describe_transition_receipt_validation_error(
              validation_error,
            ),
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: "state transition returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-transition success payload",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: case_support.driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: case_support.driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn expected_permanent_error_case_result(
  id id: String,
  operation operation: String,
  expected_summary expected_summary: String,
  invocation invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = case_support.response_request_id(response)
  case response {
    types.DriverResponseError(
      error: types.DriverError(kind: types.PermanentError, message: message, ..),
      ..,
    ) ->
      case_support.passed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: "unknown target returned normalized permanent error",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned permanent error: " <> message,
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: case_support.driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: case_support.driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: "unknown target should not succeed",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned success for an unknown target state",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

type TransitionReceiptValidationError {
  TransitionReceiptTaskMismatch
  TransitionReceiptStateNameMismatch
  TransitionReceiptStateIdMismatch
}

fn validate_transition_receipt(
  subject: task.Task,
  receipt: types.StateTransitionReceiptPayload,
  expected_state_id: Option(String),
  expected_state_name: String,
) -> Result(String, TransitionReceiptValidationError) {
  let task.Task(ref: subject_ref, ..) = subject
  let types.StateTransitionReceiptPayload(task: receipt_task, state: state) =
    receipt
  let task.TaskState(id: actual_state_id, name: actual_state_name, ..) = state
  case case_support.same_ref(subject_ref, receipt_task) {
    False -> Error(TransitionReceiptTaskMismatch)
    True ->
      case actual_state_name == expected_state_name {
        False -> Error(TransitionReceiptStateNameMismatch)
        True ->
          case option_string_equals(actual_state_id, expected_state_id) {
            False -> Error(TransitionReceiptStateIdMismatch)
            True ->
              Ok("driver returned normalized state " <> expected_state_name)
          }
      }
  }
}

fn describe_transition_receipt_validation_error(
  error: TransitionReceiptValidationError,
) -> String {
  case error {
    TransitionReceiptTaskMismatch ->
      "receipt.task did not match the fixture task"
    TransitionReceiptStateNameMismatch ->
      "receipt.state.name did not match the requested target"
    TransitionReceiptStateIdMismatch ->
      "receipt.state.id did not match the requested target"
  }
}

fn option_string_equals(left: Option(String), right: Option(String)) -> Bool {
  case left, right {
    None, None -> True
    Some(left_value), Some(right_value) -> left_value == right_value
    _, _ -> False
  }
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  case left {
    [] -> right
    [first, ..rest] -> [first, ..list_append(rest, right)]
  }
}

fn manifest_claims_reason(manifest: types.Manifest) -> Bool {
  let types.Manifest(profile: manifest_profile, ..) = manifest
  let types.ProfileConfig(capabilities: capabilities, ..) = manifest_profile
  capability_in_list(capabilities, profile.StateTransitionsReasonCapability)
}

fn capability_in_list(
  capabilities: List(profile.Capability),
  target: profile.Capability,
) -> Bool {
  case capabilities {
    [] -> False
    [capability, ..rest] ->
      capability == target || capability_in_list(rest, target)
  }
}

fn first_task(tasks: List(task.Task)) -> task.Task {
  case tasks {
    [first, ..] -> first
    [] ->
      task.Task(
        ref: task.TaskRef(
          backend_kind: "missing-fixture",
          remote_id: "missing-fixture",
          key: None,
          url: None,
        ),
        title: "missing fixture",
        description: None,
        priority: None,
        state: task.TaskState(id: None, name: "Missing", category: task.Unknown),
        branch_hint: None,
        labels: [],
        blockers: [],
        blockers_complete: True,
        created_at: None,
        updated_at: None,
      )
  }
}
