import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/log
import scherzo/path
import scherzo/result_artifact
import scherzo/structured_output_metadata
import scherzo/template
import scherzo/workflow_dag

pub type StepStatus {
  StepSucceeded
  StepFailed
}

pub type StructuredOutputRetryDiagnostic {
  StructuredOutputRetryDiagnostic(
    attempt: Int,
    status: String,
    failure_code: Option(String),
    message: String,
  )
}

pub type StructuredOutputRetryInfo {
  StructuredOutputRetryInfo(
    max_retries: Int,
    attempts: Int,
    outcome: String,
    diagnostics: List(StructuredOutputRetryDiagnostic),
  )
}

pub type StructuredOutputErrorDetails {
  StructuredOutputErrorDetails(
    code: String,
    retryable: Bool,
    validator_name: Option(String),
    validator_type: Option(String),
    diagnostic_summary: String,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
  )
}

pub type StructuredOutputMetadata {
  StructuredOutputMetadata(
    artifact_name: String,
    format: String,
    ref: String,
    path: String,
    sha256: String,
    bytes: Int,
    schema_status: String,
    source_type: String,
    source_tool_name: Option(String),
    source_parameters_schema_path: Option(String),
    source_parameters_schema_sha256: Option(String),
    source_receipt_json: Option(String),
    baseline_required_keys: List(String),
    validators: List(structured_output_metadata.ValidatorSummary),
    retry: Option(StructuredOutputRetryInfo),
  )
}

pub type StructuredOutputOutcome {
  StructuredOutputValid(StructuredOutputMetadata)
  StructuredOutputAbsent(
    artifact_name: String,
    format: String,
    schema_status: String,
  )
  StructuredOutputError(
    artifact_name: String,
    format: String,
    message: String,
    details: Option(StructuredOutputErrorDetails),
    retry: Option(StructuredOutputRetryInfo),
  )
}

pub type StepArtifact {
  StepArtifact(
    step_id: String,
    status: StepStatus,
    final_response: Option(String),
    exit_code: Option(Int),
    command: Option(String),
    duration_ms: Option(Int),
    diagnostic_path: Option(String),
    failure_code: Option(String),
    stdout: String,
    stderr: String,
    timed_out: Bool,
    final_response_truncated: Bool,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
    summary_text: String,
    structured_output: Option(StructuredOutputOutcome),
  )
}

pub fn status_to_string(status: StepStatus) -> String {
  case status {
    StepSucceeded -> "success"
    StepFailed -> "failure"
  }
}

pub fn status_from_exit(
  exit_code: Int,
  timed_out timed_out: Bool,
) -> StepStatus {
  case exit_code == 0 && !timed_out {
    True -> StepSucceeded
    False -> StepFailed
  }
}

pub fn succeeded(status: StepStatus) -> Bool {
  case status {
    StepSucceeded -> True
    StepFailed -> False
  }
}

pub fn to_json(artifact: StepArtifact) -> json.Json {
  json.object([
    #("step_id", json.string(artifact.step_id)),
    #("status", json.string(status_to_string(artifact.status))),
    #("final_response", option_string_to_json(artifact.final_response)),
    #("exit_code", option_int_to_json(artifact.exit_code)),
    #("command", option_string_to_json(artifact.command)),
    #("duration_ms", option_int_to_json(artifact.duration_ms)),
    #("diagnostic_path", option_string_to_json(artifact.diagnostic_path)),
    #("failure_code", option_string_to_json(artifact.failure_code)),
    #("stdout", json.string(artifact.stdout)),
    #("stderr", json.string(artifact.stderr)),
    #("timed_out", json.bool(artifact.timed_out)),
    #("final_response_truncated", json.bool(artifact.final_response_truncated)),
    #("stdout_truncated", json.bool(artifact.stdout_truncated)),
    #("stderr_truncated", json.bool(artifact.stderr_truncated)),
    #("summary_text", json.string(artifact.summary_text)),
    #(
      "structured_output",
      option_structured_output_to_json(artifact.structured_output),
    ),
  ])
}

pub fn to_string(artifact: StepArtifact) -> String {
  artifact |> to_json |> json.to_string
}

pub fn decode_string(contents: String) -> Result(StepArtifact, String) {
  case json.parse(contents, decoder()) {
    Ok(artifact) -> Ok(artifact)
    Error(_) -> Error("invalid_step_artifact_json")
  }
}

pub fn status_from_string(status: String) -> Result(StepStatus, String) {
  case status {
    "success" -> Ok(StepSucceeded)
    "failure" -> Ok(StepFailed)
    _ -> Error("unknown_step_status:" <> status)
  }
}

pub fn decoder() -> decode.Decoder(StepArtifact) {
  use step_id <- decode.field("step_id", decode.string)
  use status <- decode.field("status", status_decoder())
  use final_response <- decode.optional_field(
    "final_response",
    None,
    decode.optional(decode.string),
  )
  use exit_code <- decode.optional_field(
    "exit_code",
    None,
    decode.optional(decode.int),
  )
  use command <- decode.optional_field(
    "command",
    None,
    decode.optional(decode.string),
  )
  use duration_ms <- decode.optional_field(
    "duration_ms",
    None,
    decode.optional(decode.int),
  )
  use diagnostic_path <- decode.optional_field(
    "diagnostic_path",
    None,
    decode.optional(decode.string),
  )
  use failure_code <- decode.optional_field(
    "failure_code",
    None,
    decode.optional(decode.string),
  )
  use stdout <- decode.field("stdout", decode.string)
  use stderr <- decode.field("stderr", decode.string)
  use timed_out <- decode.field("timed_out", decode.bool)
  use final_response_truncated <- decode.field(
    "final_response_truncated",
    decode.bool,
  )
  use stdout_truncated <- decode.field("stdout_truncated", decode.bool)
  use stderr_truncated <- decode.field("stderr_truncated", decode.bool)
  use summary_text <- decode.field("summary_text", decode.string)
  use structured_output <- decode.optional_field(
    "structured_output",
    None,
    decode.optional(structured_output_decoder()),
  )
  decode.success(StepArtifact(
    step_id: step_id,
    status: status,
    final_response: final_response,
    exit_code: exit_code,
    command: command,
    duration_ms: duration_ms,
    diagnostic_path: diagnostic_path,
    failure_code: failure_code,
    stdout: stdout,
    stderr: stderr,
    timed_out: timed_out,
    final_response_truncated: final_response_truncated,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
    summary_text: summary_text,
    structured_output: structured_output,
  ))
}

fn status_decoder() -> decode.Decoder(StepStatus) {
  use status_text <- decode.then(decode.string)
  case status_from_string(status_text) {
    Ok(status) -> decode.success(status)
    Error(_) -> decode.failure(StepFailed, expected: "StepStatus")
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn option_structured_output_to_json(
  value: Option(StructuredOutputOutcome),
) -> json.Json {
  case value {
    Some(outcome) -> structured_output_to_json(outcome)
    None -> json.null()
  }
}

fn structured_output_to_json(outcome: StructuredOutputOutcome) -> json.Json {
  case outcome {
    StructuredOutputValid(metadata) ->
      json.object([
        #("status", json.string("valid")),
        #("artifact_name", json.string(metadata.artifact_name)),
        #("format", json.string(metadata.format)),
        #("ref", json.string(metadata.ref)),
        #("path", json.string(metadata.path)),
        #("sha256", json.string(metadata.sha256)),
        #("bytes", json.int(metadata.bytes)),
        #("schema_status", json.string(metadata.schema_status)),
        #("source_type", json.string(metadata.source_type)),
        #("source_tool_name", option_string_to_json(metadata.source_tool_name)),
        #(
          "source_parameters_schema_path",
          option_string_to_json(metadata.source_parameters_schema_path),
        ),
        #(
          "source_parameters_schema_sha256",
          option_string_to_json(metadata.source_parameters_schema_sha256),
        ),
        #(
          "source_receipt_json",
          option_string_to_json(metadata.source_receipt_json),
        ),
        #(
          "baseline_required_keys",
          json.array(metadata.baseline_required_keys, of: json.string),
        ),
        #(
          "validators",
          json.array(
            metadata.validators,
            of: structured_output_metadata.summary_to_json,
          ),
        ),
        #("retry", option_retry_info_to_json(metadata.retry)),
      ])
    StructuredOutputAbsent(artifact_name, format, schema_status) ->
      json.object([
        #("status", json.string("absent")),
        #("artifact_name", json.string(artifact_name)),
        #("format", json.string(format)),
        #("schema_status", json.string(schema_status)),
      ])
    StructuredOutputError(artifact_name, format, message, details, retry) ->
      json.object([
        #("status", json.string("error")),
        #("artifact_name", json.string(artifact_name)),
        #("format", json.string(format)),
        #("error", json.string(message)),
        #("failure", option_error_details_to_json(details)),
        #("retry", option_retry_info_to_json(retry)),
      ])
  }
}

fn option_retry_info_to_json(
  retry: Option(StructuredOutputRetryInfo),
) -> json.Json {
  case retry {
    Some(info) -> retry_info_to_json(info)
    None -> json.null()
  }
}

fn option_error_details_to_json(
  details: Option(StructuredOutputErrorDetails),
) -> json.Json {
  case details {
    Some(details) -> error_details_to_json(details)
    None -> json.null()
  }
}

fn error_details_to_json(details: StructuredOutputErrorDetails) -> json.Json {
  json.object([
    #("code", json.string(details.code)),
    #("retryable", json.bool(details.retryable)),
    #("validator_name", option_string_to_json(details.validator_name)),
    #("validator_type", option_string_to_json(details.validator_type)),
    #("diagnostic_summary", json.string(details.diagnostic_summary)),
    #("stdout_truncated", json.bool(details.stdout_truncated)),
    #("stderr_truncated", json.bool(details.stderr_truncated)),
  ])
}

fn retry_info_to_json(info: StructuredOutputRetryInfo) -> json.Json {
  json.object([
    #("max_retries", json.int(info.max_retries)),
    #("attempts", json.int(info.attempts)),
    #("outcome", json.string(info.outcome)),
    #("diagnostics", json.array(info.diagnostics, of: retry_diagnostic_to_json)),
  ])
}

fn retry_diagnostic_to_json(
  diagnostic: StructuredOutputRetryDiagnostic,
) -> json.Json {
  json.object([
    #("attempt", json.int(diagnostic.attempt)),
    #("status", json.string(diagnostic.status)),
    #("failure_code", option_string_to_json(diagnostic.failure_code)),
    #("message", json.string(diagnostic.message)),
  ])
}

fn structured_output_decoder() -> decode.Decoder(StructuredOutputOutcome) {
  use status <- decode.field("status", decode.string)
  case status {
    "valid" -> {
      use artifact_name <- decode.field("artifact_name", decode.string)
      use format <- decode.field("format", decode.string)
      use ref <- decode.field("ref", decode.string)
      use path <- decode.field("path", decode.string)
      use sha256 <- decode.field("sha256", decode.string)
      use bytes <- decode.field("bytes", decode.int)
      use schema_status <- decode.field("schema_status", decode.string)
      use source_type <- decode.optional_field(
        "source_type",
        "final_response",
        decode.string,
      )
      use source_tool_name <- decode.optional_field(
        "source_tool_name",
        None,
        decode.optional(decode.string),
      )
      use source_parameters_schema_path <- decode.optional_field(
        "source_parameters_schema_path",
        None,
        decode.optional(decode.string),
      )
      use source_parameters_schema_sha256 <- decode.optional_field(
        "source_parameters_schema_sha256",
        None,
        decode.optional(decode.string),
      )
      use source_receipt_json <- decode.optional_field(
        "source_receipt_json",
        None,
        decode.optional(decode.string),
      )
      use baseline_required_keys <- decode.optional_field(
        "baseline_required_keys",
        [],
        decode.list(decode.string),
      )
      use validators <- decode.optional_field(
        "validators",
        [],
        decode.list(structured_output_metadata.summary_decoder()),
      )
      use retry <- decode.optional_field(
        "retry",
        None,
        decode.optional(retry_info_decoder()),
      )
      decode.success(
        StructuredOutputValid(StructuredOutputMetadata(
          artifact_name: artifact_name,
          format: format,
          ref: ref,
          path: path,
          sha256: sha256,
          bytes: bytes,
          schema_status: schema_status,
          source_type: source_type,
          source_tool_name: source_tool_name,
          source_parameters_schema_path: source_parameters_schema_path,
          source_parameters_schema_sha256: source_parameters_schema_sha256,
          source_receipt_json: source_receipt_json,
          baseline_required_keys: baseline_required_keys,
          validators: validators,
          retry: retry,
        )),
      )
    }
    "absent" -> {
      use artifact_name <- decode.field("artifact_name", decode.string)
      use format <- decode.field("format", decode.string)
      use schema_status <- decode.field("schema_status", decode.string)
      decode.success(StructuredOutputAbsent(
        artifact_name,
        format,
        schema_status,
      ))
    }
    "error" -> {
      use artifact_name <- decode.field("artifact_name", decode.string)
      use format <- decode.field("format", decode.string)
      use message <- decode.field("error", decode.string)
      use details <- decode.optional_field(
        "failure",
        None,
        decode.optional(error_details_decoder()),
      )
      use retry <- decode.optional_field(
        "retry",
        None,
        decode.optional(retry_info_decoder()),
      )
      decode.success(StructuredOutputError(
        artifact_name,
        format,
        message,
        details,
        retry,
      ))
    }
    _ ->
      decode.failure(
        StructuredOutputError("", "", "", None, None),
        expected: "StructuredOutputOutcome",
      )
  }
}

fn error_details_decoder() -> decode.Decoder(StructuredOutputErrorDetails) {
  use code <- decode.field("code", decode.string)
  use retryable <- decode.field("retryable", decode.bool)
  use validator_name <- decode.optional_field(
    "validator_name",
    None,
    decode.optional(decode.string),
  )
  use validator_type <- decode.optional_field(
    "validator_type",
    None,
    decode.optional(decode.string),
  )
  use diagnostic_summary <- decode.optional_field(
    "diagnostic_summary",
    "",
    decode.string,
  )
  use stdout_truncated <- decode.optional_field(
    "stdout_truncated",
    False,
    decode.bool,
  )
  use stderr_truncated <- decode.optional_field(
    "stderr_truncated",
    False,
    decode.bool,
  )
  decode.success(StructuredOutputErrorDetails(
    code: code,
    retryable: retryable,
    validator_name: validator_name,
    validator_type: validator_type,
    diagnostic_summary: diagnostic_summary,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
  ))
}

fn retry_info_decoder() -> decode.Decoder(StructuredOutputRetryInfo) {
  use max_retries <- decode.field("max_retries", decode.int)
  use attempts <- decode.field("attempts", decode.int)
  use outcome <- decode.field("outcome", decode.string)
  use diagnostics <- decode.field(
    "diagnostics",
    decode.list(of: retry_diagnostic_decoder()),
  )
  decode.success(StructuredOutputRetryInfo(
    max_retries: max_retries,
    attempts: attempts,
    outcome: outcome,
    diagnostics: diagnostics,
  ))
}

fn retry_diagnostic_decoder() -> decode.Decoder(StructuredOutputRetryDiagnostic) {
  use attempt <- decode.field("attempt", decode.int)
  use status <- decode.field("status", decode.string)
  use failure_code <- decode.optional_field(
    "failure_code",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.field("message", decode.string)
  decode.success(StructuredOutputRetryDiagnostic(
    attempt: attempt,
    status: status,
    failure_code: failure_code,
    message: message,
  ))
}

pub fn from_agent_success(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> StepArtifact {
  let #(final_response, final_truncated) =
    cap_optional(
      success.result.final_response,
      secrets,
      limits.template_field_max_chars,
    )
  let summary = step_id <> " success agent"
  StepArtifact(
    step_id: step_id,
    status: StepSucceeded,
    final_response: final_response,
    exit_code: None,
    command: None,
    duration_ms: None,
    diagnostic_path: None,
    failure_code: None,
    stdout: "",
    stderr: "",
    timed_out: False,
    final_response_truncated: final_truncated || success.result.truncated,
    stdout_truncated: False,
    stderr_truncated: False,
    summary_text: summary,
    structured_output: None,
  )
}

pub fn from_agent_success_with_valid_structured_output(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  metadata: StructuredOutputMetadata,
) -> StepArtifact {
  from_agent_success_with_structured_output(
    step_id,
    success,
    secrets,
    limits,
    StructuredOutputValid(metadata),
  )
}

pub fn from_agent_success_with_absent_structured_output(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  artifact_name: String,
  format: String,
  schema_status: String,
) -> StepArtifact {
  from_agent_success_with_structured_output(
    step_id,
    success,
    secrets,
    limits,
    StructuredOutputAbsent(artifact_name, format, schema_status),
  )
}

pub fn from_agent_success_with_structured_output(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  outcome: StructuredOutputOutcome,
) -> StepArtifact {
  StepArtifact(
    ..from_agent_success(step_id, success, secrets, limits),
    structured_output: Some(outcome),
  )
}

pub fn from_agent_structured_output_error(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  failure_code: String,
  message: String,
  artifact_name: String,
  format: String,
) -> StepArtifact {
  from_agent_structured_output_error_with_details(
    step_id,
    success,
    secrets,
    limits,
    failure_code,
    message,
    artifact_name,
    format,
    None,
  )
}

pub fn from_agent_structured_output_error_with_details(
  step_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  failure_code: String,
  message: String,
  artifact_name: String,
  format: String,
  details: Option(StructuredOutputErrorDetails),
) -> StepArtifact {
  let base = from_agent_success(step_id, success, secrets, limits)
  StepArtifact(
    ..base,
    status: StepFailed,
    failure_code: Some(failure_code),
    stderr: message,
    summary_text: step_id
      <> " failure agent"
      <> failure_code_inline(Some(failure_code))
      <> " "
      <> inline(message, 80),
    structured_output: Some(StructuredOutputError(
      artifact_name,
      format,
      message,
      details,
      None,
    )),
  )
}

pub fn with_structured_output_retry_info(
  artifact: StepArtifact,
  retry: StructuredOutputRetryInfo,
) -> StepArtifact {
  let structured_output = case artifact.structured_output {
    Some(StructuredOutputValid(metadata)) ->
      Some(StructuredOutputValid(
        StructuredOutputMetadata(..metadata, retry: Some(retry)),
      ))
    Some(StructuredOutputError(artifact_name, format, message, details, _)) ->
      Some(StructuredOutputError(
        artifact_name,
        format,
        message,
        details,
        Some(retry),
      ))
    other -> other
  }
  StepArtifact(
    ..artifact,
    summary_text: summary_with_retry(artifact.summary_text, retry),
    structured_output: structured_output,
  )
}

fn summary_with_retry(
  summary: String,
  retry: StructuredOutputRetryInfo,
) -> String {
  summary
  <> " structured_output_retry="
  <> retry.outcome
  <> " attempts="
  <> int.to_string(retry.attempts)
}

pub fn from_command_result(
  step_id: String,
  exit_code: Int,
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> StepArtifact {
  from_command_result_with_metadata(
    step_id,
    None,
    exit_code,
    None,
    None,
    stdout,
    stderr,
    timed_out,
    secrets,
    limits,
    False,
    False,
  )
}

pub fn from_command_result_with_truncation(
  step_id: String,
  exit_code: Int,
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout_already_truncated: Bool,
  stderr_already_truncated: Bool,
) -> StepArtifact {
  from_command_result_with_metadata(
    step_id,
    None,
    exit_code,
    None,
    None,
    stdout,
    stderr,
    timed_out,
    secrets,
    limits,
    stdout_already_truncated,
    stderr_already_truncated,
  )
}

pub fn from_command_result_with_metadata(
  step_id: String,
  command: Option(String),
  exit_code: Int,
  duration_ms: Option(Int),
  diagnostic_path: Option(String),
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout_already_truncated: Bool,
  stderr_already_truncated: Bool,
) -> StepArtifact {
  let status = status_from_exit(exit_code, timed_out: timed_out)
  let failure_code = case status {
    StepSucceeded -> None
    StepFailed -> failure_code_from_streams(stdout, stderr)
  }
  let #(stdout, stdout_truncated) =
    cap_with_truncation(
      stdout,
      secrets,
      limits.command_stream_max_chars,
      stdout_already_truncated,
    )
  let #(stderr, stderr_truncated) =
    cap_with_truncation(
      stderr,
      secrets,
      limits.command_stream_max_chars,
      stderr_already_truncated,
    )
  let #(command, _) =
    cap_optional(command, secrets, limits.template_field_max_chars)
  let status_text = status_to_string(status)
  let summary =
    step_id
    <> " "
    <> status_text
    <> " command"
    <> failure_code_inline(failure_code)
    <> " exit_code="
    <> int_to_string(exit_code)
    <> case timed_out {
      True -> " timed_out=true"
      False -> ""
    }
  StepArtifact(
    step_id: step_id,
    status: status,
    final_response: None,
    exit_code: Some(exit_code),
    command: command,
    duration_ms: duration_ms,
    diagnostic_path: diagnostic_path,
    failure_code: failure_code,
    stdout: stdout,
    stderr: stderr,
    timed_out: timed_out,
    final_response_truncated: False,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
    summary_text: summary,
    structured_output: None,
  )
}

pub fn command_failure_summary(artifact: StepArtifact) -> Option(String) {
  case artifact.status {
    StepSucceeded -> None
    StepFailed -> {
      let head =
        "command step failed: step="
        <> artifact.step_id
        <> failure_code_inline(artifact.failure_code)
        <> command_inline(artifact.command)
        <> exit_inline(artifact.exit_code)
        <> duration_inline(artifact.duration_ms)
        <> timeout_inline(artifact.timed_out)
      Some(
        head
        <> stream_inline("stdout", artifact.stdout, artifact.stdout_truncated)
        <> stream_inline("stderr", artifact.stderr, artifact.stderr_truncated)
        <> artifact_inline(artifact.diagnostic_path),
      )
    }
  }
}

pub fn command_failure_details(artifact: StepArtifact) -> String {
  let metadata =
    "step: "
    <> artifact.step_id
    <> failure_code_detail(artifact.failure_code)
    <> "\ncommand: "
    <> option.unwrap(artifact.command, "<not recorded>")
    <> "\n"
    <> exit_detail(artifact.exit_code)
    <> "\n"
    <> duration_detail(artifact.duration_ms)
    <> timeout_detail(artifact.timed_out)
    <> truncation_detail(artifact)
  let stdout = stream_detail("stdout", artifact.stdout)
  let stderr = stream_detail("stderr", artifact.stderr)
  metadata <> stdout <> stderr
}

const failure_code_prefix = "SCHERZO_FAILURE_CODE="

pub fn failure_code_from_streams(
  stdout: String,
  stderr: String,
) -> Option(String) {
  case failure_code_from_stream(stderr) {
    Some(code) -> Some(code)
    None -> failure_code_from_stream(stdout)
  }
}

fn failure_code_from_stream(stream: String) -> Option(String) {
  stream
  |> string.split("\n")
  |> first_failure_code_line
}

fn first_failure_code_line(lines: List(String)) -> Option(String) {
  case lines {
    [] -> None
    [line, ..rest] -> {
      let line = string.trim(line)
      case string.starts_with(line, failure_code_prefix) {
        True ->
          case
            line
            |> string.drop_start(string.length(failure_code_prefix))
            |> sanitized_failure_code
          {
            Some(code) -> Some(code)
            None -> first_failure_code_line(rest)
          }
        False -> first_failure_code_line(rest)
      }
    }
  }
}

fn sanitized_failure_code(value: String) -> Option(String) {
  let code = string.trim(value)
  case code != "" && safe_failure_code(code) {
    True -> Some(code)
    False -> None
  }
}

fn safe_failure_code(code: String) -> Bool {
  code
  |> string.to_graphemes
  |> list.all(is_failure_code_grapheme)
}

fn is_failure_code_grapheme(grapheme: String) -> Bool {
  string.length(grapheme) == 1
  && string.contains("abcdefghijklmnopqrstuvwxyz0123456789_", grapheme)
}

fn failure_code_inline(failure_code: Option(String)) -> String {
  case failure_code {
    Some(code) -> " failure_code=" <> code
    None -> ""
  }
}

fn failure_code_detail(failure_code: Option(String)) -> String {
  case failure_code {
    Some(code) -> "\nfailure_code: " <> code
    None -> ""
  }
}

fn command_inline(command: Option(String)) -> String {
  case command {
    Some(command) -> " command=\"" <> inline(command, 80) <> "\""
    None -> " command=<not recorded>"
  }
}

fn exit_inline(exit_code: Option(Int)) -> String {
  case exit_code {
    Some(exit_code) -> " exit_code=" <> int.to_string(exit_code)
    None -> " exit_status=<not recorded>"
  }
}

fn duration_inline(duration_ms: Option(Int)) -> String {
  case duration_ms {
    Some(duration_ms) -> " duration_ms=" <> int.to_string(duration_ms)
    None -> ""
  }
}

fn timeout_inline(timed_out: Bool) -> String {
  case timed_out {
    True -> " timed_out=true"
    False -> ""
  }
}

fn stream_inline(label: String, value: String, truncated: Bool) -> String {
  case value == "" {
    True ->
      case truncated {
        True -> " " <> label <> "=<truncated empty excerpt>"
        False -> ""
      }
    False -> {
      let suffix = case truncated {
        True -> " [truncated]"
        False -> ""
      }
      " " <> label <> "=\"" <> inline(value, 160) <> suffix <> "\""
    }
  }
}

fn artifact_inline(path: Option(String)) -> String {
  case path {
    Some(path) -> " artifact=" <> display_path(path)
    None -> ""
  }
}

fn display_path(value: String) -> String {
  case string.starts_with(value, "/") {
    False -> value
    True ->
      case repo_relative_path(value) {
        Some(relative) -> relative
        None ->
          case scherzo_workspace_relative_path(value) {
            Some(relative) -> relative
            None -> "<absolute path hidden>"
          }
      }
  }
}

fn repo_relative_path(value: String) -> Option(String) {
  case path.env("SCHERZO_REPO_ROOT") {
    Some(root) ->
      case relative_to_root(value, root) {
        Some(relative) -> Some(relative)
        None -> cwd_relative_path(value)
      }
    None -> cwd_relative_path(value)
  }
}

fn cwd_relative_path(value: String) -> Option(String) {
  case path.absolute(".") {
    Ok(root) -> relative_to_root(value, root)
    Error(_) -> None
  }
}

fn relative_to_root(value: String, root: String) -> Option(String) {
  let root_abs = path.absolute(root) |> result.unwrap(root)
  let root_abs = trim_trailing_slash(root_abs)
  case path.contains(root_abs, value) {
    True ->
      case value == root_abs {
        True -> Some(".")
        False -> Some(string.drop_start(value, string.length(root_abs) + 1))
      }
    False -> None
  }
}

fn scherzo_workspace_relative_path(value: String) -> Option(String) {
  case string.split_once(value, on: "/.scherzo/workspaces/") {
    Ok(#(_, rest)) -> Some(".scherzo/workspaces/" <> rest)
    Error(_) -> None
  }
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

fn inline(value: String, max_chars: Int) -> String {
  let compact =
    value
    |> string.replace(each: "\r\n", with: "\n")
    |> string.replace(each: "\n", with: " ⏎ ")
  case string.length(compact) > max_chars {
    True -> string.slice(compact, 0, max_chars) <> "…"
    False -> compact
  }
}

fn exit_detail(exit_code: Option(Int)) -> String {
  "exit_code: "
  <> case exit_code {
    Some(exit_code) -> int.to_string(exit_code)
    None -> "<not recorded>"
  }
}

fn duration_detail(duration_ms: Option(Int)) -> String {
  "duration_ms: "
  <> case duration_ms {
    Some(duration_ms) -> int.to_string(duration_ms)
    None -> "<not recorded>"
  }
}

fn timeout_detail(timed_out: Bool) -> String {
  case timed_out {
    True -> "\ntimed_out: true"
    False -> ""
  }
}

fn truncation_detail(artifact: StepArtifact) -> String {
  let stdout =
    stream_truncation_detail(
      "stdout",
      artifact.stdout_truncated,
      artifact.diagnostic_path,
    )
  let stderr =
    stream_truncation_detail(
      "stderr",
      artifact.stderr_truncated,
      artifact.diagnostic_path,
    )
  stdout <> stderr
}

fn stream_truncation_detail(
  label: String,
  truncated: Bool,
  diagnostic_path: Option(String),
) -> String {
  case truncated {
    False -> ""
    True ->
      "\n"
      <> label
      <> "_truncated: true"
      <> case diagnostic_path {
        Some(path) -> " (full retained artifact: " <> display_path(path) <> ")"
        None -> " (full retained artifact unavailable)"
      }
  }
}

fn stream_detail(label: String, value: String) -> String {
  case value == "" {
    True -> "\n" <> label <> ": <empty>"
    False -> "\n" <> label <> ":\n" <> value
  }
}

pub fn to_template_locals(
  artifacts: Dict(String, StepArtifact),
) -> List(#(String, template.Value)) {
  let locals =
    dict.to_list(artifacts)
    |> list.flat_map(fn(entry) {
      let #(step_id, artifact) = entry
      artifact_locals(step_id, artifact)
    })

  case dict.get(artifacts, "prepare_context") {
    Ok(artifact) ->
      list.append(artifact_locals("source_preparation", artifact), locals)
    Error(_) ->
      case dict.get(artifacts, "prepare_plan") {
        Ok(artifact) ->
          list.append(artifact_locals("source_preparation", artifact), locals)
        Error(_) -> locals
      }
  }
}

pub fn workflow_result_artifact(
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, StepArtifact),
  limits: config_types.ArtifactLimits,
) -> result_artifact.ResultArtifact {
  let primary = primary_text(dag, artifacts)
  let summary = summary_for_dag(dag.steps, artifacts, [])
  let text = case primary, summary {
    "", "" -> ""
    _, "" -> primary
    "", _ -> "Workflow step summary:\n" <> summary
    _, _ -> primary <> "\n\nWorkflow step summary:\n" <> summary
  }
  let truncated =
    string.length(text) > limits.workflow_summary_max_chars
    || any_truncated(dict.values(artifacts))
  let final_response = case text == "" {
    True -> None
    False -> Some(log.truncate(text, limits.workflow_summary_max_chars))
  }
  result_artifact.from_final_response(final_response, truncated, "workflow_dag")
}

fn artifact_locals(
  step_id: String,
  artifact: StepArtifact,
) -> List(#(String, template.Value)) {
  let prefix = "steps." <> step_id <> "."
  list.append(
    [
      #(prefix <> "status", template.VString(status_to_string(artifact.status))),
      #(
        prefix <> "final_response",
        option_string_value(artifact.final_response),
      ),
      #(prefix <> "exit_code", option_int_value(artifact.exit_code)),
      #(prefix <> "command", option_string_value(artifact.command)),
      #(prefix <> "duration_ms", option_int_value(artifact.duration_ms)),
      #(
        prefix <> "diagnostic_path",
        option_string_value(artifact.diagnostic_path),
      ),
      #(prefix <> "failure_code", option_string_value(artifact.failure_code)),
      #(prefix <> "stdout", template.VString(artifact.stdout)),
      #(prefix <> "stderr", template.VString(artifact.stderr)),
      #(prefix <> "timed_out", template.VBool(artifact.timed_out)),
      #(
        prefix <> "final_response_truncated",
        template.VBool(artifact.final_response_truncated),
      ),
      #(prefix <> "stdout_truncated", template.VBool(artifact.stdout_truncated)),
      #(prefix <> "stderr_truncated", template.VBool(artifact.stderr_truncated)),
      #(prefix <> "summary", template.VString(artifact.summary_text)),
    ],
    structured_output_locals(prefix, artifact.structured_output),
  )
}

fn structured_output_locals(
  prefix: String,
  outcome: Option(StructuredOutputOutcome),
) -> List(#(String, template.Value)) {
  let prefix = prefix <> "structured_output."
  case outcome {
    Some(StructuredOutputValid(metadata)) ->
      list.append(
        [
          #(prefix <> "status", template.VString("valid")),
          #(prefix <> "artifact_name", template.VString(metadata.artifact_name)),
          #(prefix <> "format", template.VString(metadata.format)),
          #(prefix <> "ref", template.VString(metadata.ref)),
          #(prefix <> "path", template.VString(metadata.path)),
          #(prefix <> "sha256", template.VString(metadata.sha256)),
          #(prefix <> "bytes", template.VInt(metadata.bytes)),
          #(prefix <> "schema_status", template.VString(metadata.schema_status)),
          #(prefix <> "error", template.VNil),
        ],
        retry_info_locals(prefix, metadata.retry),
      )
    Some(StructuredOutputAbsent(artifact_name, format, schema_status)) ->
      list.append(
        [
          #(prefix <> "status", template.VString("absent")),
          #(prefix <> "artifact_name", template.VString(artifact_name)),
          #(prefix <> "format", template.VString(format)),
          #(prefix <> "ref", template.VNil),
          #(prefix <> "path", template.VNil),
          #(prefix <> "sha256", template.VNil),
          #(prefix <> "bytes", template.VNil),
          #(prefix <> "schema_status", template.VString(schema_status)),
          #(prefix <> "error", template.VNil),
        ],
        retry_info_locals(prefix, None),
      )
    Some(StructuredOutputError(artifact_name, format, message, details, retry)) ->
      list.append(
        list.append(
          [
            #(prefix <> "status", template.VString("error")),
            #(prefix <> "artifact_name", template.VString(artifact_name)),
            #(prefix <> "format", template.VString(format)),
            #(prefix <> "ref", template.VNil),
            #(prefix <> "path", template.VNil),
            #(prefix <> "sha256", template.VNil),
            #(prefix <> "bytes", template.VNil),
            #(prefix <> "schema_status", template.VNil),
            #(prefix <> "error", template.VString(message)),
          ],
          error_details_locals(prefix, details),
        ),
        retry_info_locals(prefix, retry),
      )
    None ->
      list.append(
        [
          #(prefix <> "status", template.VString("not_configured")),
          #(prefix <> "artifact_name", template.VNil),
          #(prefix <> "format", template.VNil),
          #(prefix <> "ref", template.VNil),
          #(prefix <> "path", template.VNil),
          #(prefix <> "sha256", template.VNil),
          #(prefix <> "bytes", template.VNil),
          #(prefix <> "schema_status", template.VNil),
          #(prefix <> "error", template.VNil),
        ],
        retry_info_locals(prefix, None),
      )
  }
}

fn error_details_locals(
  prefix: String,
  details: Option(StructuredOutputErrorDetails),
) -> List(#(String, template.Value)) {
  case details {
    Some(details) -> [
      #(prefix <> "failure_code", template.VString(details.code)),
      #(prefix <> "failure_retryable", template.VBool(details.retryable)),
      #(
        prefix <> "failure_validator_name",
        option_string_value(details.validator_name),
      ),
      #(
        prefix <> "failure_validator_type",
        option_string_value(details.validator_type),
      ),
      #(
        prefix <> "failure_diagnostic_summary",
        template.VString(details.diagnostic_summary),
      ),
      #(
        prefix <> "failure_stdout_truncated",
        template.VBool(details.stdout_truncated),
      ),
      #(
        prefix <> "failure_stderr_truncated",
        template.VBool(details.stderr_truncated),
      ),
    ]
    None -> []
  }
}

fn retry_info_locals(
  prefix: String,
  retry: Option(StructuredOutputRetryInfo),
) -> List(#(String, template.Value)) {
  case retry {
    Some(info) -> [
      #(prefix <> "retry_outcome", template.VString(info.outcome)),
      #(prefix <> "retry_attempts", template.VInt(info.attempts)),
      #(prefix <> "retry_max_retries", template.VInt(info.max_retries)),
      #(prefix <> "retry_error", retry_error_value(info.diagnostics)),
    ]
    None -> [
      #(prefix <> "retry_outcome", template.VNil),
      #(prefix <> "retry_attempts", template.VNil),
      #(prefix <> "retry_max_retries", template.VNil),
      #(prefix <> "retry_error", template.VNil),
    ]
  }
}

fn retry_error_value(
  diagnostics: List(StructuredOutputRetryDiagnostic),
) -> template.Value {
  case diagnostics {
    [] -> template.VNil
    [diagnostic, ..] -> template.VString(diagnostic.message)
  }
}

fn primary_text(
  dag: workflow_dag.WorkflowDag,
  artifacts: Dict(String, StepArtifact),
) -> String {
  case workflow_dag.terminal_step(dag) {
    None -> ""
    Some(step) ->
      case dict.get(artifacts, step.id) {
        Error(_) -> ""
        Ok(artifact) -> artifact_primary_text(artifact)
      }
  }
}

fn artifact_primary_text(artifact: StepArtifact) -> String {
  case artifact.final_response {
    Some(text) -> text
    None ->
      case artifact.stdout != "" {
        True -> artifact.stdout
        False -> artifact.stderr
      }
  }
}

fn summary_for_dag(
  steps: List(workflow_dag.WorkflowStep),
  artifacts: Dict(String, StepArtifact),
  acc: List(String),
) -> String {
  case steps {
    [] -> string.join(list.reverse(acc), with: "\n")
    [step, ..rest] -> {
      let acc = case dict.get(artifacts, step.id) {
        Ok(artifact) -> [artifact.summary_text, ..acc]
        Error(_) -> acc
      }
      summary_for_dag(rest, artifacts, acc)
    }
  }
}

fn any_truncated(artifacts: List(StepArtifact)) -> Bool {
  case artifacts {
    [] -> False
    [artifact, ..rest] ->
      artifact.final_response_truncated
      || artifact.stdout_truncated
      || artifact.stderr_truncated
      || any_truncated(rest)
  }
}

fn cap_optional(
  value: Option(String),
  secrets: List(String),
  max_chars: Int,
) -> #(Option(String), Bool) {
  case value {
    None -> #(None, False)
    Some(value) -> {
      let #(value, truncated) = cap(value, secrets, max_chars)
      #(Some(value), truncated)
    }
  }
}

fn cap(
  value: String,
  secrets: List(String),
  max_chars: Int,
) -> #(String, Bool) {
  cap_with_truncation(value, secrets, max_chars, False)
}

fn cap_with_truncation(
  value: String,
  secrets: List(String),
  max_chars: Int,
  already_truncated: Bool,
) -> #(String, Bool) {
  let redacted = log.redact("step_artifact", value, secrets)
  let truncated = already_truncated || string.length(redacted) > max_chars
  case truncated {
    True -> #(string.slice(redacted, 0, max_chars) <> "...", True)
    False -> #(redacted, False)
  }
}

fn option_string_value(value: Option(String)) -> template.Value {
  case value {
    Some(value) -> template.VString(value)
    None -> template.VNil
  }
}

fn option_int_value(value: Option(Int)) -> template.Value {
  case value {
    Some(value) -> template.VInt(value)
    None -> template.VNil
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
