import gleam/option.{None, Some}
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
  let task.Task(ref: subject_ref, ..) = subject
  [
    run_handoff_case(
      manifest,
      "handoff.report.claim",
      "req-handoff-claim",
      types.HandoffClaimEvent(
        task: subject_ref,
        workspace_path: "workspace/main/SECRET_TOKEN",
        run_id: "run-handoff-1",
      ),
      "handoff.report should acknowledge generic claim events.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.success",
      "req-handoff-success",
      types.HandoffSuccessEvent(
        task: subject_ref,
        run_id: "run-handoff-1",
        summary: "summary SECRET_TOKEN",
      ),
      "handoff.report should acknowledge generic success events.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.failure",
      "req-handoff-failure",
      types.HandoffFailureEvent(
        task: subject_ref,
        run_id: "run-handoff-1",
        reason: "reason SECRET_TOKEN",
      ),
      "handoff.report should acknowledge generic failure events.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.park",
      "req-handoff-park",
      types.HandoffParkEvent(
        task: subject_ref,
        reason: "park reason SECRET_TOKEN",
        release_policy: "release policy SECRET_TOKEN",
      ),
      "handoff.report should acknowledge generic park events.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.legacy_claim",
      "req-handoff-legacy-claim",
      types.LegacyHandoffClaimEvent(
        issue_identifier: issue_identifier(subject_ref),
        workspace_path: "workspace/legacy/SECRET_TOKEN",
        run_id: "run-legacy-1",
      ),
      "handoff.report should acknowledge legacy claim events while the adapter spec still exposes them.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.legacy_success",
      "req-handoff-legacy-success",
      types.LegacyHandoffSuccessEvent(
        issue_identifier: issue_identifier(subject_ref),
        success: "legacy success SECRET_TOKEN",
        run_id: "run-legacy-1",
        workflow_id: "workflow:execplan-implementation",
      ),
      "handoff.report should acknowledge legacy success events while the adapter spec still exposes them.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.legacy_failure",
      "req-handoff-legacy-failure",
      types.LegacyHandoffFailureEvent(
        issue_identifier: issue_identifier(subject_ref),
        failure: "legacy failure SECRET_TOKEN",
        run_id: "run-legacy-1",
        workflow_id: "workflow:execplan-implementation",
      ),
      "handoff.report should acknowledge legacy failure events while the adapter spec still exposes them.",
    ),
    run_handoff_case(
      manifest,
      "handoff.report.legacy_park",
      "req-handoff-legacy-park",
      types.LegacyHandoffParkEvent(
        task: subject_ref,
        issue_identifier: issue_identifier(subject_ref),
        reason: "legacy park SECRET_TOKEN",
        release_policy: Some("release policy SECRET_TOKEN"),
        run_id: Some("run-legacy-1"),
      ),
      "handoff.report should acknowledge legacy park events while the adapter spec still exposes them.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.claim",
      "req-handoff-retry-claim-first",
      "req-handoff-retry-claim-second",
      types.HandoffClaimEvent(
        task: subject_ref,
        workspace_path: "workspace/retry/claim/SECRET_TOKEN",
        run_id: "run-handoff-retry-claim",
      ),
      "retrying the same generic handoff claim should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.success",
      "req-handoff-retry-success-first",
      "req-handoff-retry-success-second",
      types.HandoffSuccessEvent(
        task: subject_ref,
        run_id: "run-handoff-retry-success",
        summary: "retry success SECRET_TOKEN",
      ),
      "retrying the same generic handoff success should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.failure",
      "req-handoff-retry-failure-first",
      "req-handoff-retry-failure-second",
      types.HandoffFailureEvent(
        task: subject_ref,
        run_id: "run-handoff-retry-failure",
        reason: "retry failure SECRET_TOKEN",
      ),
      "retrying the same generic handoff failure should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.park",
      "req-handoff-retry-park-first",
      "req-handoff-retry-park-second",
      types.HandoffParkEvent(
        task: subject_ref,
        reason: "retry park SECRET_TOKEN",
        release_policy: "retry release policy SECRET_TOKEN",
      ),
      "retrying the same generic handoff park should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.legacy_claim",
      "req-handoff-retry-legacy-claim-first",
      "req-handoff-retry-legacy-claim-second",
      types.LegacyHandoffClaimEvent(
        issue_identifier: issue_identifier(subject_ref),
        workspace_path: "workspace/retry/legacy-claim/SECRET_TOKEN",
        run_id: "run-handoff-retry-legacy-claim",
      ),
      "retrying the same legacy handoff claim should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.legacy_success",
      "req-handoff-retry-legacy-success-first",
      "req-handoff-retry-legacy-success-second",
      types.LegacyHandoffSuccessEvent(
        issue_identifier: issue_identifier(subject_ref),
        success: "retry legacy success SECRET_TOKEN",
        run_id: "run-handoff-retry-legacy-success",
        workflow_id: "workflow:execplan-implementation",
      ),
      "retrying the same legacy handoff success should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.legacy_failure",
      "req-handoff-retry-legacy-failure-first",
      "req-handoff-retry-legacy-failure-second",
      types.LegacyHandoffFailureEvent(
        issue_identifier: issue_identifier(subject_ref),
        failure: "retry legacy failure SECRET_TOKEN",
        run_id: "run-handoff-retry-legacy-failure",
        workflow_id: "workflow:execplan-implementation",
      ),
      "retrying the same legacy handoff failure should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
    run_handoff_retry_case(
      manifest,
      "handoff.report.retry.legacy_park",
      "req-handoff-retry-legacy-park-first",
      "req-handoff-retry-legacy-park-second",
      types.LegacyHandoffParkEvent(
        task: subject_ref,
        issue_identifier: issue_identifier(subject_ref),
        reason: "retry legacy park SECRET_TOKEN",
        release_policy: Some("retry legacy release policy SECRET_TOKEN"),
        run_id: Some("run-handoff-retry-legacy-park"),
      ),
      "retrying the same legacy handoff park should return normalized receipts and leave duplicate handling visible according to profile.retry_behavior.handoff_report.",
    ),
  ]
}

fn run_handoff_case(
  manifest: types.Manifest,
  id: String,
  request_id: String,
  event: types.HandoffEventPayload,
  expected_summary: String,
) -> types.CaseResult {
  let operation = "handoff.report"
  case driver.invoke(manifest, handoff_request(request_id, event)) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      handoff_receipt_case_result(
        id: id,
        operation: operation,
        expected_summary: expected_summary,
        invocation: invocation,
      )
  }
}

fn run_handoff_retry_case(
  manifest: types.Manifest,
  id: String,
  first_request_id: String,
  second_request_id: String,
  event: types.HandoffEventPayload,
  expected_summary: String,
) -> types.CaseResult {
  let operation = "handoff.report"
  case driver.invoke(manifest, handoff_request(first_request_id, event)) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: id,
        operation: operation,
        request_id: first_request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(first_invocation) ->
      case driver.invoke(manifest, handoff_request(second_request_id, event)) {
        Error(failure) ->
          case_support.driver_failure_case_result(
            id: id,
            operation: operation,
            request_id: second_request_id,
            expected_summary: expected_summary,
            failure: failure,
          )
        Ok(second_invocation) ->
          handoff_retry_case_result(
            id: id,
            expected_summary: expected_summary,
            retry_behavior: handoff_retry_behavior(manifest),
            first_invocation: first_invocation,
            second_invocation: second_invocation,
          )
      }
  }
}

fn handoff_receipt_case_result(
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
    types.DriverResponseSuccess(
      result: types.HandoffReportResult(receipt: receipt),
      ..,
    ) ->
      case receipt {
        types.HandoffReportReceiptPayload(reported: True) ->
          case_support.passed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "handoff report returned a normalized receipt",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "driver reported the handoff event successfully",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        _ ->
          case_support.failed_case_result(
            id: id,
            operation: operation,
            request_id: request_id,
            message: "handoff report returned reported=false",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "driver returned a handoff receipt but did not mark it reported",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: id,
        operation: operation,
        request_id: request_id,
        message: "handoff report returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-handoff success payload",
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

fn handoff_retry_case_result(
  id id: String,
  expected_summary expected_summary: String,
  retry_behavior retry_behavior: types.RetryBehavior,
  first_invocation first_invocation: driver.DriverInvocation,
  second_invocation second_invocation: driver.DriverInvocation,
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
      result: types.HandoffReportResult(receipt: types.HandoffReportReceiptPayload(
        reported: True,
      )),
      ..,
    ),
      types.DriverResponseSuccess(
        result: types.HandoffReportResult(receipt: types.HandoffReportReceiptPayload(
          reported: True,
        )),
        ..,
      )
    ->
      case_support.passed_case_result(
        id: id,
        operation: "handoff.report",
        request_id: request_id,
        message: "same-run handoff retry returned normalized receipts",
        diagnostics: first_diagnostics <> second_diagnostics,
        expected_summary: expected_summary,
        actual_summary: retry_behavior_summary(retry_behavior),
        request_transcript: request_transcript,
        response_transcript: Some(merge_transcripts(
          first_response_transcript,
          second_response_transcript,
        )),
      )
    _, _ ->
      case_support.failed_case_result(
        id: id,
        operation: "handoff.report",
        request_id: request_id,
        message: "same-run handoff retry did not return two success receipts",
        diagnostics: first_diagnostics <> second_diagnostics,
        expected_summary: expected_summary,
        actual_summary: "one or both retry responses were non-handoff payloads or normalized errors",
        request_transcript: request_transcript,
        response_transcript: Some(merge_transcripts(
          first_response_transcript,
          second_response_transcript,
        )),
      )
  }
}

fn handoff_request(
  request_id: String,
  event: types.HandoffEventPayload,
) -> types.DriverRequest {
  types.DriverRequest(
    schema_version: types.schema_version,
    request_id: request_id,
    operation: profile.HandoffReport,
    payload: types.HandoffReportPayload(event: event),
  )
}

fn retry_behavior_summary(retry_behavior: types.RetryBehavior) -> String {
  case retry_behavior {
    types.IdempotentUpdateOrDedupe ->
      "driver accepted both retry attempts; probes should confirm idempotent_update_or_dedupe visibility"
    types.DuplicateVisible ->
      "driver accepted both retry attempts; probes should confirm duplicate_visible visibility"
  }
}

fn handoff_retry_behavior(manifest: types.Manifest) -> types.RetryBehavior {
  let types.Manifest(profile: manifest_profile, ..) = manifest
  let types.ProfileConfig(retry_behavior: retry_behavior, ..) = manifest_profile
  case retry_behavior {
    Some(types.RetryBehaviorConfig(handoff_report: Some(value), ..)) -> value
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

fn issue_identifier(ref: task.TaskRef) -> String {
  let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
  case key {
    Some(value) -> value
    None -> remote_id
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
