import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/path
import scherzo/result_artifact
import scherzo/structured_output_command_validator
import scherzo/structured_output_json_schema
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile

pub type StructuredOutputValidation {
  StructuredOutputPresent(payload_json: String)
  StructuredOutputAbsent
}

pub type StructuredOutputError {
  StructuredOutputMissing(message: String, retryable: Bool)
  StructuredOutputTruncated(message: String, retryable: Bool)
  StructuredOutputInvalidJson(message: String, retryable: Bool)
  StructuredOutputSchemaInvalid(message: String, retryable: Bool)
  StructuredOutputValidatorFailed(
    failure: structured_output_validator.ValidatorFailure,
  )
  StructuredOutputToolSourceInvalid(
    code: String,
    message: String,
    retryable: Bool,
  )
}

pub type ValidatorRunner =
  fn(workflow_dag.StructuredOutputValidator, json_value.JsonValue, String, Int) ->
    Result(
      structured_output_validator.ValidatorPass,
      structured_output_validator.ValidatorFailure,
    )

pub fn validate_agent_result(
  spec: workflow_dag.StructuredOutputSpec,
  result: result_artifact.ResultArtifact,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case spec.source {
    structured_output_source.FinalResponseSource ->
      validate_final_response(
        spec,
        result.structured_response,
        result.structured_response_truncated,
        secrets,
        validator_runner,
      )
    structured_output_source.PiToolCallSource(tool_name, _) ->
      validate_tool_call_source(
        spec,
        result,
        tool_name,
        secrets,
        validator_runner,
      )
  }
}

pub fn validate_final_response(
  spec: workflow_dag.StructuredOutputSpec,
  final_response: Option(String),
  capture_truncated: Bool,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case capture_truncated {
    True ->
      Error(StructuredOutputTruncated(
        "structured output capture was truncated before validation; cannot validate structured JSON",
        spec.required,
      ))
    False ->
      validate_nontruncated_response(
        spec,
        final_response,
        secrets,
        validator_runner,
      )
  }
}

pub fn source_type_to_string(
  source: structured_output_source.StructuredOutputSource,
) -> String {
  structured_output_source.type_to_string(source)
}

pub fn source_tool_name(
  source: structured_output_source.StructuredOutputSource,
) -> Option(String) {
  structured_output_source.tool_name(source)
}

pub fn noop_validator_runner(
  _validator: workflow_dag.StructuredOutputValidator,
  _value: json_value.JsonValue,
  _redacted_payload_json: String,
  _index: Int,
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  Ok(structured_output_validator.ValidatorPass)
}

pub fn default_validator_runner(
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> ValidatorRunner {
  fn(validator, value, redacted_payload_json, index) {
    let validator_context =
      structured_output_validator.for_validator(context, validator, index)
    case validator {
      workflow_dag.JsonSchemaValidator(..) ->
        structured_output_json_schema.run_json_schema_validator(
          validator,
          value,
          validator_context,
          secrets,
        )
      workflow_dag.CommandValidator(..) ->
        structured_output_command_validator.run_command_validator(
          validator,
          redacted_payload_json,
          validator_context,
          secrets,
        )
    }
  }
}

pub fn default_validator_context(
  config_dir: String,
  run_root: String,
  workflow_id: String,
  workflow_bundle_dir: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_path: String,
  artifact_name: String,
  format: String,
  source: structured_output_source.StructuredOutputSource,
) -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    config_dir,
    validator_repo_root(config_dir, workspace_path),
    run_root,
    workflow_id,
    workflow_bundle_dir,
    run_id,
    step_id,
    attempt_index,
    workspace_path,
    artifact_name,
    format,
    source_type_to_string(source),
    source_tool_name(source),
  )
}

pub fn validator_repo_root(
  config_dir: String,
  workspace_path: String,
) -> String {
  let config_parent = case path.dirname(config_dir) {
    Ok(parent) -> parent
    Error(Nil) -> config_dir
  }
  choose_validator_repo_root([config_parent, workspace_path, "."])
}

pub fn error_code(error: StructuredOutputError) -> String {
  case error {
    StructuredOutputMissing(_, _) -> "structured_output_missing"
    StructuredOutputTruncated(_, _) -> "structured_output_truncated"
    StructuredOutputInvalidJson(_, _) -> "structured_output_invalid_json"
    StructuredOutputSchemaInvalid(_, _) -> "structured_output_schema_invalid"
    StructuredOutputValidatorFailed(failure) -> failure.code
    StructuredOutputToolSourceInvalid(code, _, _) -> code
  }
}

pub fn error_message(error: StructuredOutputError) -> String {
  case error {
    StructuredOutputMissing(message, _)
    | StructuredOutputTruncated(message, _)
    | StructuredOutputInvalidJson(message, _)
    | StructuredOutputSchemaInvalid(message, _)
    | StructuredOutputToolSourceInvalid(_, message, _) -> message
    StructuredOutputValidatorFailed(failure) ->
      validator_failure_message(failure)
  }
}

pub fn error_retryable(error: StructuredOutputError) -> Bool {
  case error {
    StructuredOutputMissing(_, retryable)
    | StructuredOutputTruncated(_, retryable)
    | StructuredOutputInvalidJson(_, retryable)
    | StructuredOutputSchemaInvalid(_, retryable)
    | StructuredOutputToolSourceInvalid(_, _, retryable) -> retryable
    StructuredOutputValidatorFailed(failure) -> failure.retryable
  }
}

pub fn error_validator_name(error: StructuredOutputError) -> Option(String) {
  case error {
    StructuredOutputValidatorFailed(failure) -> Some(failure.validator_name)
    _ -> None
  }
}

pub fn error_validator_type(error: StructuredOutputError) -> Option(String) {
  case error {
    StructuredOutputValidatorFailed(failure) -> Some(failure.validator_type)
    _ -> None
  }
}

pub fn error_diagnostic_summary(error: StructuredOutputError) -> String {
  case error {
    StructuredOutputValidatorFailed(failure) -> failure.diagnostic_summary
    _ -> error_message(error)
  }
}

pub fn error_stdout_truncated(error: StructuredOutputError) -> Bool {
  case error {
    StructuredOutputValidatorFailed(failure) -> failure.stdout_truncated
    _ -> False
  }
}

pub fn error_stderr_truncated(error: StructuredOutputError) -> Bool {
  case error {
    StructuredOutputValidatorFailed(failure) -> failure.stderr_truncated
    _ -> False
  }
}

pub fn error_message_for_step(
  error: StructuredOutputError,
  step_id: String,
) -> String {
  "step " <> step_id <> " " <> error_message(error)
}

fn validate_nontruncated_response(
  spec: workflow_dag.StructuredOutputSpec,
  final_response: Option(String),
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case final_response {
    None -> missing_or_absent(spec)
    Some(response) -> {
      let trimmed = string.trim(response)
      case trimmed == "" {
        True -> missing_or_absent(spec)
        False ->
          validate_present_response(spec, trimmed, secrets, validator_runner)
      }
    }
  }
}

fn missing_or_absent(
  spec: workflow_dag.StructuredOutputSpec,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case spec.required {
    True ->
      Error(StructuredOutputMissing(
        "required a JSON final response but the agent returned none",
        True,
      ))
    False -> Ok(StructuredOutputAbsent)
  }
}

fn validate_present_response(
  spec: workflow_dag.StructuredOutputSpec,
  trimmed: String,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  use value <- result.try(parse_present_json(trimmed, spec.required))
  validate_present_value(spec, value, secrets, validator_runner)
}

fn validate_tool_call_source(
  spec: workflow_dag.StructuredOutputSpec,
  agent_result: result_artifact.ResultArtifact,
  tool_name: String,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  let matching = matching_tool_calls(agent_result.tool_calls, tool_name)
  case matching {
    [] -> missing_tool_call_result(spec, tool_name, agent_result.tool_calls)
    [call] ->
      validate_single_tool_call(
        spec,
        call,
        tool_name,
        secrets,
        validator_runner,
      )
    [_, ..] ->
      Error(tool_source_error(
        spec,
        "structured_output_tool_call_multiple",
        "expected exactly one successful Pi tool call named `"
          <> tool_name
          <> "` for structured artifact `"
          <> spec.artifact_name
          <> "`, but multiple matching calls were observed",
      ))
  }
}

fn validate_single_tool_call(
  spec: workflow_dag.StructuredOutputSpec,
  call: result_artifact.ToolCallSubmission,
  tool_name: String,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case call.sibling_count > 1 {
    True ->
      Error(tool_source_error(
        spec,
        "structured_output_tool_call_sibling",
        "Pi tool call `"
          <> tool_name
          <> "` for structured artifact `"
          <> spec.artifact_name
          <> "` was submitted in the same assistant batch as another tool call",
      ))
    False ->
      case successful_tool_status(call.status) {
        False ->
          Error(tool_source_error(
            spec,
            "structured_output_tool_call_failed",
            "Pi tool call `"
              <> tool_name
              <> "` for structured artifact `"
              <> spec.artifact_name
              <> "` did not report successful completion",
          ))
        True ->
          validate_tool_call_arguments(
            spec,
            call,
            tool_name,
            secrets,
            validator_runner,
          )
      }
  }
}

fn validate_tool_call_arguments(
  spec: workflow_dag.StructuredOutputSpec,
  call: result_artifact.ToolCallSubmission,
  tool_name: String,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case call.arguments_json {
    None ->
      Error(tool_source_error(
        spec,
        "structured_output_tool_call_arguments_invalid",
        "Pi tool call `"
          <> tool_name
          <> "` for structured artifact `"
          <> spec.artifact_name
          <> "` did not include JSON arguments",
      ))
    Some(arguments_json) -> {
      use value <- result.try(parse_tool_arguments(
        arguments_json,
        tool_name,
        spec.required,
      ))
      use value <- result.try(require_tool_arguments_object(
        value,
        tool_name,
        spec.artifact_name,
        spec.required,
      ))
      validate_present_value(spec, value, secrets, validator_runner)
    }
  }
}

fn validate_present_value(
  spec: workflow_dag.StructuredOutputSpec,
  value: json_value.JsonValue,
  secrets: List(String),
  validator_runner: ValidatorRunner,
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  use value <- result.try(validate_baseline_schema(
    spec.schema,
    value,
    spec.required,
  ))
  let redacted = redact_value(value, secrets)
  let redacted_payload_json = json_value.to_string(redacted)
  use Nil <- result.try(validate_configured_validators(
    spec.validators,
    value,
    redacted_payload_json,
    validator_runner,
    0,
  ))
  Ok(StructuredOutputPresent(redacted_payload_json))
}

fn validate_configured_validators(
  validators: List(workflow_dag.StructuredOutputValidator),
  value: json_value.JsonValue,
  redacted_payload_json: String,
  validator_runner: ValidatorRunner,
  index: Int,
) -> Result(Nil, StructuredOutputError) {
  case validators {
    [] -> Ok(Nil)
    [validator, ..rest] ->
      case validator_runner(validator, value, redacted_payload_json, index) {
        Ok(structured_output_validator.ValidatorPass) ->
          validate_configured_validators(
            rest,
            value,
            redacted_payload_json,
            validator_runner,
            index + 1,
          )
        Error(failure) -> Error(StructuredOutputValidatorFailed(failure))
      }
  }
}

fn matching_tool_calls(
  calls: List(result_artifact.ToolCallSubmission),
  tool_name: String,
) -> List(result_artifact.ToolCallSubmission) {
  list.filter(calls, fn(call) { call.name == tool_name })
}

fn missing_tool_call_result(
  spec: workflow_dag.StructuredOutputSpec,
  tool_name: String,
  calls: List(result_artifact.ToolCallSubmission),
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case calls, spec.required {
    [], False -> Ok(StructuredOutputAbsent)
    [], True ->
      Error(tool_source_error(
        spec,
        "structured_output_tool_call_missing",
        "required Pi tool call `"
          <> tool_name
          <> "` for structured artifact `"
          <> spec.artifact_name
          <> "`, but no Pi tool calls were observed",
      ))
    _, _ ->
      Error(tool_source_error(
        spec,
        "structured_output_tool_call_wrong_name",
        "required Pi tool call `"
          <> tool_name
          <> "` for structured artifact `"
          <> spec.artifact_name
          <> "`, but only different tool names were observed",
      ))
  }
}

fn successful_tool_status(status: Option(String)) -> Bool {
  case status {
    Some(value) -> {
      let normalized = string.lowercase(string.trim(value))
      normalized == "success" || normalized == "succeeded"
    }
    None -> False
  }
}

fn parse_tool_arguments(
  arguments_json: String,
  tool_name: String,
  retryable: Bool,
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case json_value.parse(arguments_json) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(StructuredOutputToolSourceInvalid(
        "structured_output_tool_call_arguments_invalid",
        "Pi tool call `" <> tool_name <> "` arguments were not valid JSON",
        retryable,
      ))
  }
}

fn require_tool_arguments_object(
  value: json_value.JsonValue,
  tool_name: String,
  artifact_name: String,
  retryable: Bool,
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case value {
    json_value.JObject(_) -> Ok(value)
    _ ->
      Error(StructuredOutputToolSourceInvalid(
        "structured_output_tool_call_arguments_invalid",
        "Pi tool call `"
          <> tool_name
          <> "` for structured artifact `"
          <> artifact_name
          <> "` arguments must be a JSON object",
        retryable,
      ))
  }
}

fn tool_source_error(
  spec: workflow_dag.StructuredOutputSpec,
  code: String,
  message: String,
) -> StructuredOutputError {
  StructuredOutputToolSourceInvalid(code, message, spec.required)
}

fn parse_present_json(
  trimmed: String,
  retryable: Bool,
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case json_value.parse(trimmed) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(StructuredOutputInvalidJson(
        "required a JSON-only final response",
        retryable,
      ))
  }
}

fn validate_baseline_schema(
  schema: workflow_dag.StructuredOutputSchema,
  value: json_value.JsonValue,
  retryable: Bool,
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case schema, value {
    workflow_dag.StructuredObjectSchema(required_keys),
      json_value.JObject(entries)
    -> {
      let missing = missing_required_keys(required_keys, entries, [])
      case missing {
        [] -> Ok(value)
        _ ->
          Error(StructuredOutputSchemaInvalid(
            "schema invalid; missing required keys: "
              <> string.join(missing, with: ", "),
            retryable,
          ))
      }
    }
    workflow_dag.StructuredObjectSchema(_), _ ->
      Error(StructuredOutputSchemaInvalid(
        "schema invalid; top-level JSON value must be an object",
        retryable,
      ))
  }
}

fn choose_validator_repo_root(candidates: List(String)) -> String {
  case candidates {
    [] -> "."
    [candidate, ..rest] ->
      case has_scherzo_review_script(candidate) {
        True -> candidate
        False -> choose_validator_repo_root(rest)
      }
  }
}

fn has_scherzo_review_script(candidate: String) -> Bool {
  case
    simplifile.is_file(path.join(
      candidate,
      ".scherzo/workflows/scripts/scherzo-review",
    ))
  {
    Ok(True) -> True
    _ -> False
  }
}

fn validator_failure_message(
  failure: structured_output_validator.ValidatorFailure,
) -> String {
  let base =
    "validator "
    <> failure.validator_name
    <> " ("
    <> failure.validator_type
    <> ") failed: "
    <> string.trim(failure.message)
  case string.trim(failure.diagnostic_summary) {
    "" -> base
    diagnostic -> base <> ": " <> diagnostic
  }
}

fn missing_required_keys(
  required_keys: List(String),
  entries: List(#(String, json_value.JsonValue)),
  acc: List(String),
) -> List(String) {
  case required_keys {
    [] -> list.reverse(acc)
    [key, ..rest] ->
      case json_value.object_has_key(entries, key) {
        True -> missing_required_keys(rest, entries, acc)
        False -> missing_required_keys(rest, entries, [key, ..acc])
      }
  }
}

fn redact_value(
  value: json_value.JsonValue,
  secrets: List(String),
) -> json_value.JsonValue {
  case value {
    json_value.JObject(entries) ->
      json_value.JObject(
        list.map(entries, fn(entry) {
          let #(key, child) = entry
          #(key, redact_value(child, secrets))
        }),
      )
    json_value.JArray(values) ->
      json_value.JArray(list.map(values, redact_value(_, secrets)))
    json_value.JString(value) ->
      json_value.JString(log.redact("structured_output", value, secrets))
    json_value.JInt(_)
    | json_value.JFloat(_)
    | json_value.JBool(_)
    | json_value.JNull -> value
  }
}
