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
  [
    run_create_only_case(manifest, subject),
    run_update_existing_case(manifest, subject),
    run_update_missing_no_fallback_case(manifest, subject),
    run_update_missing_allow_create_fallback_case(manifest, subject),
  ]
}

fn run_create_only_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-comments-create-only"
  let operation = "comments.post_or_update"
  let expected_summary =
    "create_only comment writes should return a normalized created receipt for the fixture task."
  let request =
    comment_request(
      request_id: request_id,
      subject: subject,
      body: "[marker comments-create-only] create-only conformance body",
      mode: types.CreateOnlyComment,
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "comments.post_or_update.create_only",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      comment_receipt_case_result(
        id: "comments.post_or_update.create_only",
        operation: operation,
        expected_summary: expected_summary,
        expected_created: True,
        expected_comment_id: None,
        subject: subject,
        invocation: invocation,
      )
  }
}

fn run_update_existing_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-comments-update-existing"
  let operation = "comments.post_or_update"
  let expected_summary =
    "update_existing comment writes should keep the same comment id and mark created=false."
  let request =
    comment_request(
      request_id: request_id,
      subject: subject,
      body: "[marker comments-update-existing] update-existing conformance body",
      mode: types.UpdateExistingComment(
        comment_id: "comment-existing",
        allow_create_fallback: False,
      ),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "comments.post_or_update.update_existing",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      comment_receipt_case_result(
        id: "comments.post_or_update.update_existing",
        operation: operation,
        expected_summary: expected_summary,
        expected_created: False,
        expected_comment_id: Some("comment-existing"),
        subject: subject,
        invocation: invocation,
      )
  }
}

fn run_update_missing_no_fallback_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-comments-update-missing-no-fallback"
  let operation = "comments.post_or_update"
  let expected_summary =
    "update_existing without allow_create_fallback should return a normalized not_found error for a missing comment id."
  let request =
    comment_request(
      request_id: request_id,
      subject: subject,
      body: "[marker comments-update-missing-no-fallback] stale update body",
      mode: types.UpdateExistingComment(
        comment_id: "comment-missing",
        allow_create_fallback: False,
      ),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "comments.post_or_update.update_missing_no_fallback",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      expected_not_found_case_result(
        id: "comments.post_or_update.update_missing_no_fallback",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        invocation: invocation,
      )
  }
}

fn run_update_missing_allow_create_fallback_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-comments-update-missing-allow-create-fallback"
  let operation = "comments.post_or_update"
  let expected_summary =
    "update_existing with allow_create_fallback should return a normalized created receipt when the original comment id is missing."
  let request =
    comment_request(
      request_id: request_id,
      subject: subject,
      body: "[marker comments-update-missing-allow-create-fallback] stale update body",
      mode: types.UpdateExistingComment(
        comment_id: "comment-missing",
        allow_create_fallback: True,
      ),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "comments.post_or_update.update_missing_allow_create_fallback",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      comment_receipt_case_result(
        id: "comments.post_or_update.update_missing_allow_create_fallback",
        operation: operation,
        expected_summary: expected_summary,
        expected_created: True,
        expected_comment_id: None,
        subject: subject,
        invocation: invocation,
      )
  }
}

fn comment_request(
  request_id request_id: String,
  subject subject: task.Task,
  body body: String,
  mode mode: types.CommentWriteMode,
) -> types.DriverRequest {
  let task.Task(ref: ref, ..) = subject
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.CommentsPostOrUpdate,
    payload: types.CommentsPostOrUpdatePayload(
      comment: types.CommentRequestPayload(task: ref, body: body, mode: mode),
    ),
  )
}

fn comment_receipt_case_result(
  id id: String,
  operation operation: String,
  expected_summary expected_summary: String,
  expected_created expected_created: Bool,
  expected_comment_id expected_comment_id: Option(String),
  subject subject: task.Task,
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
      result: types.CommentResult(comment: receipt),
      ..,
    ) ->
      case
        validate_comment_receipt(
          subject,
          receipt,
          expected_created,
          expected_comment_id,
        )
      {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "comment write returned a normalized receipt",
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
            message: "comment write returned a malformed or mismatched receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: describe_comment_receipt_validation_error(
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
        message: "comment write returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-comment success payload",
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

fn expected_not_found_case_result(
  id id: String,
  operation operation: String,
  expected_summary expected_summary: String,
  subject subject: task.Task,
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
  let task.Task(ref: ref, ..) = subject
  case response {
    types.DriverResponseError(
      error: types.DriverError(kind: types.NotFoundError, ref: error_ref, ..),
      ..,
    ) ->
      case matches_optional_ref(error_ref, ref) {
        True ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "missing comment id returned normalized not_found",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "driver returned not_found for the fixture task and missing comment id",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          case_support.failed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "missing comment id returned not_found for the wrong task",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "driver returned not_found but the optional ref did not match the fixture task",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
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
        message: "missing comment id should not return a success receipt without allow_create_fallback",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned success for a stale update without fallback",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

type CommentReceiptValidationError {
  EmptyCommentReceiptId
  CommentReceiptTaskMismatch
  CommentReceiptCreatedMismatch
  CommentReceiptExistingIdMismatch
}

fn validate_comment_receipt(
  subject: task.Task,
  receipt: types.CommentReceiptPayload,
  expected_created: Bool,
  expected_comment_id: Option(String),
) -> Result(String, CommentReceiptValidationError) {
  let task.Task(ref: subject_ref, ..) = subject
  let types.CommentReceiptPayload(
    id: id,
    task: receipt_task,
    created: created,
    ..,
  ) = receipt
  case id == "" {
    True -> Error(EmptyCommentReceiptId)
    False ->
      case case_support.same_ref(subject_ref, receipt_task) {
        False -> Error(CommentReceiptTaskMismatch)
        True ->
          case created == expected_created {
            False -> Error(CommentReceiptCreatedMismatch)
            True ->
              case expected_comment_id {
                Some(expected_id) ->
                  case id == expected_id {
                    True ->
                      Ok(
                        "driver returned receipt id "
                        <> id
                        <> " for the fixture task with created="
                        <> bool_string(created),
                      )
                    False -> Error(CommentReceiptExistingIdMismatch)
                  }
                None ->
                  Ok(
                    "driver returned receipt id "
                    <> id
                    <> " for the fixture task with created="
                    <> bool_string(created),
                  )
              }
          }
      }
  }
}

fn describe_comment_receipt_validation_error(
  error: CommentReceiptValidationError,
) -> String {
  case error {
    EmptyCommentReceiptId -> "receipt.id was empty"
    CommentReceiptTaskMismatch -> "receipt.task did not match the fixture task"
    CommentReceiptCreatedMismatch ->
      "receipt.created did not match the expected write mode"
    CommentReceiptExistingIdMismatch ->
      "receipt.id did not keep the expected existing comment id"
  }
}

fn matches_optional_ref(
  error_ref: Option(task.TaskRef),
  subject_ref: task.TaskRef,
) -> Bool {
  case error_ref {
    Some(ref) -> case_support.same_ref(ref, subject_ref)
    None -> True
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

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
