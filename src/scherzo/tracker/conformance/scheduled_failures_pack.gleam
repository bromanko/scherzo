import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/case_support
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

const scheduled_failure_dedupe_key = "scheduled-failure-dedupe-key"

const scheduled_failure_marker = "[marker scheduled-failure] SECRET_TOKEN"

pub fn run(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  let _ = fixture_tasks
  let #(create_result, create_task) = run_create_case(manifest)
  let remembered_retry_result = run_remembered_retry_case(manifest, create_task)
  let dedupe_recovery_result = run_dedupe_recovery_case(manifest, create_task)
  [create_result, remembered_retry_result, dedupe_recovery_result]
}

fn run_create_case(
  manifest: types.Manifest,
) -> #(types.CaseResult, Option(task.TaskRef)) {
  let request_id = "req-scheduled-failures-create"
  let operation = "scheduled_failures.publish"
  let expected_summary =
    "scheduled failure create should return a normalized created receipt for the first visible failure task."
  let expected_backend_kind = manifest_adapter_kind(manifest)
  case driver.invoke(manifest, create_request(request_id)) {
    Error(failure) -> #(
      case_support.driver_failure_case_result(
        id: "scheduled_failures.publish.create",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      ),
      None,
    )
    Ok(invocation) -> {
      let case_result =
        scheduled_failure_receipt_case_result(
          id: "scheduled_failures.publish.create",
          expected_summary: expected_summary,
          expected_created: True,
          expected_task: ExpectedBackendKind(expected_backend_kind),
          retry_classification: "create",
          invocation: invocation,
        )
      #(
        case_result,
        receipt_task_from_invocation(invocation, expected_backend_kind),
      )
    }
  }
}

fn run_remembered_retry_case(
  manifest: types.Manifest,
  create_task: Option(task.TaskRef),
) -> types.CaseResult {
  let request_id = "req-scheduled-failures-remembered-retry"
  let operation = "scheduled_failures.publish"
  let expected_summary =
    "scheduled failure retry with previous_task_remote_id should update the existing visible failure task with created=false."
  let expected_backend_kind = manifest_adapter_kind(manifest)
  case
    driver.invoke(
      manifest,
      remembered_retry_request(request_id, task_remote_id(create_task)),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "scheduled_failures.publish.remembered_retry",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      scheduled_failure_receipt_case_result(
        id: "scheduled_failures.publish.remembered_retry",
        expected_summary: expected_summary,
        expected_created: False,
        expected_task: expected_retry_task(create_task, expected_backend_kind),
        retry_classification: "remembered_retry",
        invocation: invocation,
      )
  }
}

fn run_dedupe_recovery_case(
  manifest: types.Manifest,
  create_task: Option(task.TaskRef),
) -> types.CaseResult {
  let request_id = "req-scheduled-failures-dedupe-recovery"
  let operation = "scheduled_failures.publish"
  let expected_summary =
    "scheduled failure retry without a usable previous_task_remote_id should recover the same visible failure task by dedupe_key with created=false."
  let expected_backend_kind = manifest_adapter_kind(manifest)
  case driver.invoke(manifest, dedupe_recovery_request(request_id)) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "scheduled_failures.publish.dedupe_recovery",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      scheduled_failure_receipt_case_result(
        id: "scheduled_failures.publish.dedupe_recovery",
        expected_summary: expected_summary,
        expected_created: False,
        expected_task: expected_retry_task(create_task, expected_backend_kind),
        retry_classification: "dedupe_recovery",
        invocation: invocation,
      )
  }
}

fn create_request(request_id: String) -> types.DriverRequest {
  scheduled_failure_request(
    request_id: request_id,
    publication: types.ScheduledFailurePublicationPayload(
      job_id: "nightly-reconcile",
      workflow_id: "workflow:execplan-implementation",
      due_at_ms: 1_710_000_001_000,
      run_id: "scheduled-run-1",
      attempt: 1,
      max_attempts: 3,
      reason: "scheduled failure create SECRET_TOKEN",
      run_root: Some("workspace/main/SECRET_TOKEN"),
      session_id: Some("session-create-1"),
      dedupe_key: scheduled_failure_dedupe_key,
      title: "Scheduled failure create SECRET_TOKEN",
      body: scheduled_failure_marker <> " create attempt",
      labels: [
        "workflow:execplan-implementation",
        "scheduled-failure-marker",
      ],
      target_state_name: Some("Todo"),
      previous_task_remote_id: None,
    ),
  )
}

fn remembered_retry_request(
  request_id: String,
  previous_task_remote_id: Option(String),
) -> types.DriverRequest {
  scheduled_failure_request(
    request_id: request_id,
    publication: types.ScheduledFailurePublicationPayload(
      job_id: "nightly-reconcile",
      workflow_id: "workflow:execplan-implementation",
      due_at_ms: 1_710_000_001_000,
      run_id: "scheduled-run-1",
      attempt: 2,
      max_attempts: 3,
      reason: "scheduled failure remembered retry SECRET_TOKEN",
      run_root: Some("workspace/main/SECRET_TOKEN"),
      session_id: Some("session-create-1"),
      dedupe_key: scheduled_failure_dedupe_key,
      title: "Scheduled failure remembered retry SECRET_TOKEN",
      body: scheduled_failure_marker <> " remembered retry",
      labels: [
        "workflow:execplan-implementation",
        "scheduled-failure-marker",
      ],
      target_state_name: Some("Todo"),
      previous_task_remote_id: previous_task_remote_id,
    ),
  )
}

fn dedupe_recovery_request(request_id: String) -> types.DriverRequest {
  scheduled_failure_request(
    request_id: request_id,
    publication: types.ScheduledFailurePublicationPayload(
      job_id: "nightly-reconcile",
      workflow_id: "workflow:execplan-implementation",
      due_at_ms: 1_710_000_001_000,
      run_id: "scheduled-run-1",
      attempt: 3,
      max_attempts: 3,
      reason: "scheduled failure dedupe recovery SECRET_TOKEN",
      run_root: Some("workspace/recovered/SECRET_TOKEN"),
      session_id: Some("session-recovery-1"),
      dedupe_key: scheduled_failure_dedupe_key,
      title: "Scheduled failure dedupe recovery SECRET_TOKEN",
      body: scheduled_failure_marker <> " dedupe recovery",
      labels: [
        "workflow:execplan-implementation",
        "scheduled-failure-marker",
      ],
      target_state_name: Some("Todo"),
      previous_task_remote_id: None,
    ),
  )
}

fn scheduled_failure_request(
  request_id request_id: String,
  publication publication: types.ScheduledFailurePublicationPayload,
) -> types.DriverRequest {
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.ScheduledFailuresPublish,
    payload: types.ScheduledFailurePublishPayload(publication: publication),
  )
}

fn scheduled_failure_receipt_case_result(
  id id: String,
  expected_summary expected_summary: String,
  expected_created expected_created: Bool,
  expected_task expected_task: ScheduledFailureExpectedTask,
  retry_classification retry_classification: String,
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
      result: types.ScheduledFailureResult(receipt: receipt),
      ..,
    ) ->
      case
        validate_scheduled_failure_receipt(
          receipt,
          expected_created,
          expected_task,
          retry_classification,
        )
      {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: id,
            operation: "scheduled_failures.publish",
            request_id: request_id,
            message: "scheduled failure publication returned a normalized receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: actual_summary,
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        Error(validation_error) ->
          case_support.failed_case_result(
            id: id,
            operation: "scheduled_failures.publish",
            request_id: request_id,
            message: "scheduled failure publication returned a malformed or mismatched receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: describe_scheduled_failure_validation_error(
              validation_error,
            ),
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: id,
        operation: "scheduled_failures.publish",
        request_id: request_id,
        message: "scheduled failure publication returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-scheduled-failure success payload",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      case_support.failed_case_result(
        id: id,
        operation: "scheduled_failures.publish",
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

type ScheduledFailureExpectedTask {
  ExpectedBackendKind(String)
  ExpectedSameTask(task.TaskRef)
}

type ScheduledFailureReceiptValidationError {
  EmptyScheduledFailureTaskRemoteId
  ScheduledFailureTaskBackendKindMismatch
  ScheduledFailureTaskRefMismatch
  ScheduledFailureCreatedMismatch
}

fn validate_scheduled_failure_receipt(
  receipt: types.ScheduledFailureReceiptPayload,
  expected_created: Bool,
  expected_task: ScheduledFailureExpectedTask,
  retry_classification: String,
) -> Result(String, ScheduledFailureReceiptValidationError) {
  let types.ScheduledFailureReceiptPayload(
    task: receipt_task,
    created: created,
    comment_id: comment_id,
  ) = receipt
  let task.TaskRef(remote_id: remote_id, ..) = receipt_task
  case remote_id == "" {
    True -> Error(EmptyScheduledFailureTaskRemoteId)
    False ->
      case validate_expected_task(receipt_task, expected_task) {
        Error(error) -> Error(error)
        Ok(Nil) ->
          case created == expected_created {
            False -> Error(ScheduledFailureCreatedMismatch)
            True ->
              Ok(
                "scheduled_failure task_remote_id="
                <> remote_id
                <> " created="
                <> bool_string(created)
                <> comment_summary(comment_id)
                <> " retry_classification="
                <> retry_classification,
              )
          }
      }
  }
}

fn validate_expected_task(
  receipt_task: task.TaskRef,
  expected_task: ScheduledFailureExpectedTask,
) -> Result(Nil, ScheduledFailureReceiptValidationError) {
  case expected_task {
    ExpectedBackendKind(expected_backend_kind) -> {
      let task.TaskRef(backend_kind: backend_kind, ..) = receipt_task
      case backend_kind == expected_backend_kind {
        True -> Ok(Nil)
        False -> Error(ScheduledFailureTaskBackendKindMismatch)
      }
    }
    ExpectedSameTask(expected_ref) ->
      case case_support.same_ref(receipt_task, expected_ref) {
        True -> Ok(Nil)
        False -> Error(ScheduledFailureTaskRefMismatch)
      }
  }
}

fn describe_scheduled_failure_validation_error(
  error: ScheduledFailureReceiptValidationError,
) -> String {
  case error {
    EmptyScheduledFailureTaskRemoteId -> "receipt.task.remote_id was empty"
    ScheduledFailureTaskBackendKindMismatch ->
      "receipt.task.backend_kind did not match the manifest adapter_kind"
    ScheduledFailureTaskRefMismatch ->
      "receipt.task did not match the expected visible failure task identity"
    ScheduledFailureCreatedMismatch ->
      "receipt.created did not match the expected create-or-update phase"
  }
}

fn receipt_task_from_invocation(
  invocation: driver.DriverInvocation,
  expected_backend_kind: String,
) -> Option(task.TaskRef) {
  let driver.DriverInvocation(response: response, ..) = invocation
  case response {
    types.DriverResponseSuccess(
      result: types.ScheduledFailureResult(receipt: receipt),
      ..,
    ) -> valid_receipt_task(receipt, expected_backend_kind)
    _ -> None
  }
}

fn valid_receipt_task(
  receipt: types.ScheduledFailureReceiptPayload,
  expected_backend_kind: String,
) -> Option(task.TaskRef) {
  let types.ScheduledFailureReceiptPayload(task: receipt_task, ..) = receipt
  let task.TaskRef(backend_kind: backend_kind, remote_id: remote_id, ..) =
    receipt_task
  case remote_id != "" && backend_kind == expected_backend_kind {
    True -> Some(receipt_task)
    False -> None
  }
}

fn task_remote_id(task_ref: Option(task.TaskRef)) -> Option(String) {
  case task_ref {
    Some(task.TaskRef(remote_id: remote_id, ..)) -> Some(remote_id)
    None -> None
  }
}

fn expected_retry_task(
  create_task: Option(task.TaskRef),
  expected_backend_kind: String,
) -> ScheduledFailureExpectedTask {
  case create_task {
    Some(task_ref) -> ExpectedSameTask(task_ref)
    None -> ExpectedBackendKind(expected_backend_kind)
  }
}

fn manifest_adapter_kind(manifest: types.Manifest) -> String {
  let types.Manifest(adapter_kind: adapter_kind, ..) = manifest
  adapter_kind
}

fn comment_summary(comment_id: Option(String)) -> String {
  case comment_id {
    Some(id) -> " comment_id=" <> id
    None -> ""
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
