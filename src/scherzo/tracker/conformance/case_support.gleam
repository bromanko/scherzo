import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/types

pub const adapter_recovery_guidance = "Inspect the adapter implementation for this public operation; setup, probe, and cleanup support paths are reported separately."

pub fn response_request_id(response: types.DriverResponse) -> String {
  case response {
    types.DriverResponseSuccess(request_id: request_id, ..) -> request_id
    types.DriverResponseError(request_id: request_id, ..) -> request_id
  }
}

pub fn driver_error_actual_summary(error: types.DriverError) -> String {
  let types.DriverError(kind: kind, message: message, ..) = error
  "driver returned " <> driver_error_kind_name(kind) <> ": " <> message
}

pub fn driver_error_message(error: types.DriverError) -> String {
  let types.DriverError(message: message, ..) = error
  message
}

pub fn driver_error_kind_name(kind: types.DriverErrorKind) -> String {
  case kind {
    types.UnauthorizedError -> "unauthorized"
    types.NotFoundError -> "not_found"
    types.TransientError -> "transient"
    types.PermanentError -> "permanent"
    types.UnsupportedCapabilityError -> "unsupported_capability"
    types.DecodeFailedError -> "decode_failed"
  }
}

pub fn driver_failure_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  expected_summary expected_summary: String,
  failure failure: driver.DriverFailure,
) -> types.CaseResult {
  let driver.DriverFailure(
    message: message,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    stdout: stdout,
    exit_status: exit_status,
    ..,
  ) = failure
  let details =
    message <> stdout_details(stdout) <> exit_status_details(exit_status)
  failed_case_result(
    id: id,
    operation: operation,
    request_id: request_id,
    message: details,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: "driver transport failed before a usable conformance response was accepted",
    request_transcript: request_transcript,
    response_transcript: response_transcript,
  )
}

pub fn passed_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  case_result(
    id: id,
    operation: operation,
    status: types.PassedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
  )
}

pub fn failed_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  case_result(
    id: id,
    operation: operation,
    status: types.FailedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
  )
}

pub fn same_ref(left: task.TaskRef, right: task.TaskRef) -> Bool {
  let task.TaskRef(
    backend_kind: left_backend_kind,
    remote_id: left_remote_id,
    ..,
  ) = left
  let task.TaskRef(
    backend_kind: right_backend_kind,
    remote_id: right_remote_id,
    ..,
  ) = right
  left_backend_kind == right_backend_kind && left_remote_id == right_remote_id
}

fn case_result(
  id id: String,
  operation operation: String,
  status status: types.CaseStatus,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  types.CaseResult(
    id: id,
    operation: operation,
    status: status,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    recovery_guidance: adapter_recovery_guidance,
  )
}

fn stdout_details(stdout: Option(String)) -> String {
  case stdout {
    Some(stdout) -> "; stdout=" <> stdout
    None -> ""
  }
}

fn exit_status_details(exit_status: Option(Int)) -> String {
  case exit_status {
    Some(status) -> "; exit_status=" <> int.to_string(status)
    None -> ""
  }
}
