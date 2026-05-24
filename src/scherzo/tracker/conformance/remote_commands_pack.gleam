import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/case_support
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/remote_command_event_validation
import scherzo/tracker/conformance/types

pub fn run(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  let refs = fixture_refs(fixture_tasks)
  let primary = first_task(fixture_tasks)
  [
    run_fetch_normalized_events_case(manifest, refs),
    run_fetch_since_event_ids_case(manifest, refs),
    run_fetch_limit_per_task_case(manifest, refs),
    run_post_ack_receipt_case(manifest, primary),
    run_post_ack_retry_case(manifest, primary),
    run_post_ack_failure_visibility_case(manifest, primary),
  ]
}

fn run_fetch_normalized_events_case(
  manifest: types.Manifest,
  refs: List(task.TaskRef),
) -> types.CaseResult {
  let request_id = "req-remote-fetch-normalized"
  let operation = "remote_commands.fetch_events"
  let expected_summary =
    "fetch_events should return normalized remote command events with stable ids, fixture task refs, command names, excerpts, and observed times."
  case
    driver.invoke(
      manifest,
      fetch_request(
        request_id: request_id,
        refs: refs,
        since_event_ids: [],
        limit_per_task: 5,
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.fetch.normalized_events",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      remote_events_case_result(
        id: "remote_commands.fetch.normalized_events",
        operation: operation,
        expected_summary: expected_summary,
        fixture_refs: refs,
        expect_filtered_event: None,
        max_per_task: None,
        invocation: invocation,
      )
  }
}

fn run_fetch_since_event_ids_case(
  manifest: types.Manifest,
  refs: List(task.TaskRef),
) -> types.CaseResult {
  let request_id = "req-remote-fetch-since"
  let operation = "remote_commands.fetch_events"
  let filtered = "event-card-1-0"
  let expected_summary =
    "fetch_events should honor since_event_ids and omit already-seen event ids from the result."
  case
    driver.invoke(
      manifest,
      fetch_request(
        request_id: request_id,
        refs: refs,
        since_event_ids: [filtered],
        limit_per_task: 5,
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.fetch.since_event_ids",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      remote_events_case_result(
        id: "remote_commands.fetch.since_event_ids",
        operation: operation,
        expected_summary: expected_summary,
        fixture_refs: refs,
        expect_filtered_event: Some(filtered),
        max_per_task: None,
        invocation: invocation,
      )
  }
}

fn run_fetch_limit_per_task_case(
  manifest: types.Manifest,
  refs: List(task.TaskRef),
) -> types.CaseResult {
  let request_id = "req-remote-fetch-limit"
  let operation = "remote_commands.fetch_events"
  let expected_summary =
    "fetch_events should honor limit_per_task and keep at most one event per fixture task when limit_per_task=1."
  case
    driver.invoke(
      manifest,
      fetch_request(
        request_id: request_id,
        refs: refs,
        since_event_ids: [],
        limit_per_task: 1,
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.fetch.limit_per_task",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      remote_events_case_result(
        id: "remote_commands.fetch.limit_per_task",
        operation: operation,
        expected_summary: expected_summary,
        fixture_refs: refs,
        expect_filtered_event: None,
        max_per_task: Some(1),
        invocation: invocation,
      )
  }
}

fn run_post_ack_receipt_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-remote-ack-receipt"
  let operation = "remote_commands.post_ack"
  let expected_summary =
    "post_ack should return a normalized visible acknowledgement receipt for the fixture task."
  case
    driver.invoke(
      manifest,
      ack_request(
        request_id: request_id,
        event: remote_event(
          event_id: "event-ack-receipt",
          subject: subject,
          command_name: "retry",
        ),
        body: "[marker remote-ack-receipt] ack receipt SECRET_TOKEN",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.post_ack.receipt",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      ack_receipt_case_result(
        id: "remote_commands.post_ack.receipt",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        invocation: invocation,
      )
  }
}

fn run_post_ack_retry_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let first_request_id = "req-remote-ack-retry-first"
  let second_request_id = "req-remote-ack-retry-second"
  let operation = "remote_commands.post_ack"
  let event =
    remote_event(
      event_id: "event-ack-retry",
      subject: subject,
      command_name: "retry",
    )
  let expected_summary =
    "retrying the same remote command acknowledgement should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.remote_command_ack."
  case
    driver.invoke(
      manifest,
      ack_request(
        request_id: first_request_id,
        event: event,
        body: "[marker remote-ack-retry] first ack SECRET_TOKEN",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.post_ack.same_event_retry",
        operation: operation,
        request_id: first_request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(first_invocation) ->
      case
        driver.invoke(
          manifest,
          ack_request(
            request_id: second_request_id,
            event: event,
            body: "[marker remote-ack-retry] second ack SECRET_TOKEN",
          ),
        )
      {
        Error(failure) ->
          case_support.driver_failure_case_result(
            id: "remote_commands.post_ack.same_event_retry",
            operation: operation,
            request_id: second_request_id,
            expected_summary: expected_summary,
            failure: failure,
          )
        Ok(second_invocation) ->
          ack_retry_case_result(
            expected_summary: expected_summary,
            subject: subject,
            first_invocation: first_invocation,
            second_invocation: second_invocation,
            retry_behavior: remote_ack_retry_behavior(manifest),
          )
      }
  }
}

fn run_post_ack_failure_visibility_case(
  manifest: types.Manifest,
  subject: task.Task,
) -> types.CaseResult {
  let request_id = "req-remote-ack-failure-visibility"
  let operation = "remote_commands.post_ack"
  let expected_summary =
    "post_ack should make acknowledgement failures visible in the conformance report instead of silently succeeding."
  case
    driver.invoke(
      manifest,
      ack_request(
        request_id: request_id,
        event: remote_event(
          event_id: "event-ack-failure-visible",
          subject: subject,
          command_name: "retry",
        ),
        body: "[marker remote-ack-failure-visible] ack SECRET_TOKEN",
      ),
    )
  {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "remote_commands.post_ack.failure_visibility",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      ack_receipt_case_result(
        id: "remote_commands.post_ack.failure_visibility",
        operation: operation,
        expected_summary: expected_summary,
        subject: subject,
        invocation: invocation,
      )
  }
}

fn remote_events_case_result(
  id id: String,
  operation operation: String,
  expected_summary expected_summary: String,
  fixture_refs fixture_refs: List(task.TaskRef),
  expect_filtered_event expect_filtered_event: Option(String),
  max_per_task max_per_task: Option(Int),
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
      result: types.RemoteCommandEventsResult(events: events),
      ..,
    ) ->
      case
        remote_command_event_validation.validate_remote_events(
          events,
          fixture_refs,
          expect_filtered_event,
          max_per_task,
        )
      {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "remote command fetch returned normalized events",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: actual_summary,
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        Error(actual_summary) ->
          case_support.failed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "remote command fetch returned malformed or mismatched events",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: actual_summary,
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: "remote command fetch returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-events success payload",
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

fn ack_receipt_case_result(
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
  case response {
    types.DriverResponseSuccess(
      result: types.RemoteCommandAckResult(comment: receipt),
      ..,
    ) ->
      case validate_ack_receipt(subject, receipt) {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "remote command acknowledgement returned a normalized receipt",
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
            message: "remote command acknowledgement returned a malformed receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: describe_ack_receipt_validation_error(
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
        message: "remote command acknowledgement returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-ack success payload",
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

fn ack_retry_case_result(
  expected_summary expected_summary: String,
  subject subject: task.Task,
  first_invocation first_invocation: driver.DriverInvocation,
  second_invocation second_invocation: driver.DriverInvocation,
  retry_behavior retry_behavior: types.RetryBehavior,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: first_response,
    diagnostics: first_diagnostics,
    request_transcript: request_transcript,
    response_transcript: first_response_transcript,
    ..,
  ) = first_invocation
  let driver.DriverInvocation(
    response: second_response,
    diagnostics: second_diagnostics,
    response_transcript: second_response_transcript,
    ..,
  ) = second_invocation
  let request_id = case_support.response_request_id(second_response)
  case first_response, second_response {
    types.DriverResponseSuccess(
      result: types.RemoteCommandAckResult(comment: first_receipt),
      ..,
    ),
      types.DriverResponseSuccess(
        result: types.RemoteCommandAckResult(comment: second_receipt),
        ..,
      )
    ->
      case
        validate_ack_retry(
          subject,
          first_receipt,
          second_receipt,
          retry_behavior,
        )
      {
        Ok(actual_summary) ->
          case_support.passed_case_result(
            id: "remote_commands.post_ack.same_event_retry",
            operation: "remote_commands.post_ack",
            request_id: request_id,
            message: "same-event acknowledgement retry returned normalized receipts",
            diagnostics: first_diagnostics <> second_diagnostics,
            expected_summary: expected_summary,
            actual_summary: actual_summary,
            request_transcript: request_transcript,
            response_transcript: Some(merge_transcripts(
              first_response_transcript,
              second_response_transcript,
            )),
          )
        Error(validation_error) ->
          case_support.failed_case_result(
            id: "remote_commands.post_ack.same_event_retry",
            operation: "remote_commands.post_ack",
            request_id: request_id,
            message: "same-event acknowledgement retry did not match the declared retry behavior",
            diagnostics: first_diagnostics <> second_diagnostics,
            expected_summary: expected_summary,
            actual_summary: describe_ack_retry_validation_error(
              validation_error,
            ),
            request_transcript: request_transcript,
            response_transcript: Some(merge_transcripts(
              first_response_transcript,
              second_response_transcript,
            )),
          )
      }
    _, _ ->
      case_support.failed_case_result(
        id: "remote_commands.post_ack.same_event_retry",
        operation: "remote_commands.post_ack",
        request_id: request_id,
        message: "same-event acknowledgement retry did not return two success receipts",
        diagnostics: first_diagnostics <> second_diagnostics,
        expected_summary: expected_summary,
        actual_summary: "one or both retry responses were non-ack payloads or normalized errors",
        request_transcript: request_transcript,
        response_transcript: Some(merge_transcripts(
          first_response_transcript,
          second_response_transcript,
        )),
      )
  }
}

fn fetch_request(
  request_id request_id: String,
  refs refs: List(task.TaskRef),
  since_event_ids since_event_ids: List(String),
  limit_per_task limit_per_task: Int,
) -> types.DriverRequest {
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.RemoteCommandsFetchEvents,
    payload: types.RemoteCommandsFetchPayload(
      fetch: types.RemoteCommandFetchPayload(
        task_refs: refs,
        since_event_ids: since_event_ids,
        limit_per_task: limit_per_task,
      ),
    ),
  )
}

fn ack_request(
  request_id request_id: String,
  event event: types.RemoteCommandEventPayload,
  body body: String,
) -> types.DriverRequest {
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.RemoteCommandsPostAck,
    payload: types.RemoteCommandsPostAckPayload(
      ack: types.RemoteCommandAckPayload(event: event, body: body),
    ),
  )
}

fn remote_event(
  event_id event_id: String,
  subject subject: task.Task,
  command_name command_name: String,
) -> types.RemoteCommandEventPayload {
  let task.Task(ref: ref, ..) = subject
  types.RemoteCommandEventPayload(
    event_id: event_id,
    task: ref,
    author_id: "operator-1",
    body: "/" <> command_name <> " SECRET_TOKEN",
    command_name: command_name,
    excerpt: command_name <> " excerpt SECRET_TOKEN",
    observed_at_ms: 123,
  )
}

type AckReceiptValidationError {
  EmptyAckReceiptId
  AckReceiptTaskMismatch
}

type AckRetryValidationError {
  AckRetryFirstReceiptInvalid(reason: AckReceiptValidationError)
  AckRetrySecondReceiptInvalid(reason: AckReceiptValidationError)
  AckRetryExpectedSameReceiptId
  AckRetryExpectedDifferentReceiptIds
}

fn validate_ack_receipt(
  subject: task.Task,
  receipt: types.CommentReceiptPayload,
) -> Result(String, AckReceiptValidationError) {
  let task.Task(ref: subject_ref, ..) = subject
  let types.CommentReceiptPayload(id: id, task: receipt_task, ..) = receipt
  case id == "" {
    True -> Error(EmptyAckReceiptId)
    False ->
      case case_support.same_ref(subject_ref, receipt_task) {
        False -> Error(AckReceiptTaskMismatch)
        True -> Ok("driver returned acknowledgement receipt " <> id)
      }
  }
}

fn validate_ack_retry(
  subject: task.Task,
  first_receipt: types.CommentReceiptPayload,
  second_receipt: types.CommentReceiptPayload,
  retry_behavior: types.RetryBehavior,
) -> Result(String, AckRetryValidationError) {
  case validate_ack_receipt(subject, first_receipt) {
    Error(reason) -> Error(AckRetryFirstReceiptInvalid(reason))
    Ok(_) ->
      case validate_ack_receipt(subject, second_receipt) {
        Error(reason) -> Error(AckRetrySecondReceiptInvalid(reason))
        Ok(_) -> {
          let types.CommentReceiptPayload(id: first_id, ..) = first_receipt
          let types.CommentReceiptPayload(id: second_id, ..) = second_receipt
          case retry_behavior {
            types.IdempotentUpdateOrDedupe ->
              case first_id == second_id {
                True ->
                  Ok(
                    "retry reused acknowledgement receipt "
                    <> second_id
                    <> " as expected for idempotent_update_or_dedupe",
                  )
                False -> Error(AckRetryExpectedSameReceiptId)
              }
            types.DuplicateVisible ->
              case first_id != second_id {
                True ->
                  Ok(
                    "retry returned distinct acknowledgement receipt ids under duplicate_visible",
                  )
                False -> Error(AckRetryExpectedDifferentReceiptIds)
              }
          }
        }
      }
  }
}

fn describe_ack_receipt_validation_error(
  error: AckReceiptValidationError,
) -> String {
  case error {
    EmptyAckReceiptId -> "receipt.id was empty"
    AckReceiptTaskMismatch -> "receipt.task did not match the fixture task"
  }
}

fn describe_ack_retry_validation_error(
  error: AckRetryValidationError,
) -> String {
  case error {
    AckRetryFirstReceiptInvalid(reason) ->
      "the first retry receipt was invalid: "
      <> describe_ack_receipt_validation_error(reason)
    AckRetrySecondReceiptInvalid(reason) ->
      "the second retry receipt was invalid: "
      <> describe_ack_receipt_validation_error(reason)
    AckRetryExpectedSameReceiptId ->
      "retry returned a different acknowledgement receipt id under idempotent_update_or_dedupe"
    AckRetryExpectedDifferentReceiptIds ->
      "retry reused the same acknowledgement receipt id under duplicate_visible"
  }
}

fn remote_ack_retry_behavior(manifest: types.Manifest) -> types.RetryBehavior {
  let types.Manifest(profile: manifest_profile, ..) = manifest
  let types.ProfileConfig(retry_behavior: retry_behavior, ..) = manifest_profile
  case retry_behavior {
    Some(types.RetryBehaviorConfig(remote_command_ack: Some(value), ..)) ->
      value
    _ -> types.IdempotentUpdateOrDedupe
  }
}

fn merge_transcripts(
  first: types.TranscriptEvidence,
  second: types.TranscriptEvidence,
) -> types.TranscriptEvidence {
  let types.TranscriptEvidence(
    body: first_body,
    truncated: first_truncated,
    original_chars: first_chars,
  ) = first
  let types.TranscriptEvidence(
    body: second_body,
    truncated: second_truncated,
    original_chars: second_chars,
  ) = second
  types.TranscriptEvidence(
    body: first_body <> "\n--- retry ---\n" <> second_body,
    truncated: first_truncated || second_truncated,
    original_chars: first_chars + second_chars,
  )
}

fn fixture_refs(tasks: List(task.Task)) -> List(task.TaskRef) {
  case tasks {
    [] -> []
    [task.Task(ref: ref, ..), ..rest] -> [ref, ..fixture_refs(rest)]
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
