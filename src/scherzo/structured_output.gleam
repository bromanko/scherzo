import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/path
import scherzo/port
import scherzo/workflow_dag
import simplifile

pub type StructuredOutputValidation {
  StructuredOutputPresent(payload_json: String)
  StructuredOutputAbsent
}

pub type StructuredOutputError {
  StructuredOutputMissing(message: String)
  StructuredOutputTruncated(message: String)
  StructuredOutputInvalidJson(message: String)
  StructuredOutputSchemaInvalid(message: String)
}

pub type NamedValidatorError {
  NamedValidatorError(message: String)
}

pub fn validate_final_response(
  spec: workflow_dag.StructuredOutputSpec,
  final_response: Option(String),
  capture_truncated: Bool,
  secrets: List(String),
  validator_runner: fn(
    workflow_dag.StructuredOutputValidator,
    json_value.JsonValue,
  ) -> Result(Nil, NamedValidatorError),
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case capture_truncated {
    True ->
      Error(StructuredOutputTruncated(
        "structured output capture was truncated before validation; cannot validate structured JSON",
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

pub fn noop_validator_runner(
  _validator: workflow_dag.StructuredOutputValidator,
  _value: json_value.JsonValue,
) -> Result(Nil, NamedValidatorError) {
  Ok(Nil)
}

pub fn scherzo_review_validator_runner(
  repo_root: String,
) -> fn(workflow_dag.StructuredOutputValidator, json_value.JsonValue) ->
  Result(Nil, NamedValidatorError) {
  fn(validator, value) {
    run_scherzo_review_validator(repo_root, validator, value)
  }
}

pub fn default_validator_runner(
  config_dir: String,
  workspace_path: String,
) -> fn(workflow_dag.StructuredOutputValidator, json_value.JsonValue) ->
  Result(Nil, NamedValidatorError) {
  scherzo_review_validator_runner(validator_repo_root(
    config_dir,
    workspace_path,
  ))
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
    StructuredOutputMissing(_) -> "structured_output_missing"
    StructuredOutputTruncated(_) -> "structured_output_truncated"
    StructuredOutputInvalidJson(_) -> "structured_output_invalid_json"
    StructuredOutputSchemaInvalid(_) -> "structured_output_schema_invalid"
  }
}

pub fn error_message(error: StructuredOutputError) -> String {
  case error {
    StructuredOutputMissing(message)
    | StructuredOutputTruncated(message)
    | StructuredOutputInvalidJson(message)
    | StructuredOutputSchemaInvalid(message) -> message
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
  validator_runner: fn(
    workflow_dag.StructuredOutputValidator,
    json_value.JsonValue,
  ) -> Result(Nil, NamedValidatorError),
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
      ))
    False -> Ok(StructuredOutputAbsent)
  }
}

fn validate_present_response(
  spec: workflow_dag.StructuredOutputSpec,
  trimmed: String,
  secrets: List(String),
  validator_runner: fn(
    workflow_dag.StructuredOutputValidator,
    json_value.JsonValue,
  ) -> Result(Nil, NamedValidatorError),
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  use value <- result.try(parse_present_json(trimmed))
  use value <- result.try(validate_schema(spec.schema, value))
  use value <- result.try(validate_named_validator(
    spec.validator,
    value,
    validator_runner,
  ))
  let redacted = redact_value(value, secrets)
  Ok(StructuredOutputPresent(json_value.to_string(redacted)))
}

fn parse_present_json(
  trimmed: String,
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case json_value.parse(trimmed) {
    Ok(value) -> Ok(value)
    Error(Nil) ->
      Error(StructuredOutputInvalidJson("required a JSON-only final response"))
  }
}

fn validate_schema(
  schema: workflow_dag.StructuredOutputSchema,
  value: json_value.JsonValue,
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
          ))
      }
    }
    workflow_dag.StructuredObjectSchema(_), _ ->
      Error(StructuredOutputSchemaInvalid(
        "schema invalid; top-level JSON value must be an object",
      ))
  }
}

fn validate_named_validator(
  validator: Option(workflow_dag.StructuredOutputValidator),
  value: json_value.JsonValue,
  validator_runner: fn(
    workflow_dag.StructuredOutputValidator,
    json_value.JsonValue,
  ) -> Result(Nil, NamedValidatorError),
) -> Result(json_value.JsonValue, StructuredOutputError) {
  case validator {
    None -> Ok(value)
    Some(validator) ->
      case validator_runner(validator, value) {
        Ok(Nil) -> Ok(value)
        Error(NamedValidatorError(message)) ->
          Error(StructuredOutputSchemaInvalid(
            "validator "
            <> workflow_dag.structured_output_validator_to_string(validator)
            <> " rejected structured output: "
            <> string.trim(message),
          ))
      }
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
  case simplifile.is_file(path.join(candidate, "scripts/scherzo-review")) {
    Ok(True) -> True
    _ -> False
  }
}

fn run_scherzo_review_validator(
  repo_root: String,
  validator: workflow_dag.StructuredOutputValidator,
  value: json_value.JsonValue,
) -> Result(Nil, NamedValidatorError) {
  let validator_name =
    workflow_dag.structured_output_validator_to_string(validator)
  case
    port.start_argv(
      "python3",
      [
        "scripts/scherzo-review",
        "validate-structured-output",
        "--validator",
        validator_name,
      ],
      repo_root,
      [],
    )
  {
    Error(error) ->
      Error(NamedValidatorError(
        "could not start structured-output validator "
        <> validator_name
        <> ": "
        <> port.port_error_to_string(error),
      ))
    Ok(process) ->
      run_started_scherzo_review_validator(
        process,
        validator_name,
        json_value.to_string(value),
      )
  }
}

fn run_started_scherzo_review_validator(
  process: port.Process,
  validator_name: String,
  payload_json: String,
) -> Result(Nil, NamedValidatorError) {
  case port.send_line(process, payload_json) {
    Error(error) -> {
      let diagnostics = validator_diagnostics(process)
      let _cleanup_result = port.terminate(process)
      Error(NamedValidatorError(
        "could not send payload to structured-output validator "
        <> validator_name
        <> ": "
        <> port.port_error_to_string(error)
        <> diagnostics_suffix(diagnostics),
      ))
    }
    Ok(Nil) -> await_scherzo_review_validator(process, validator_name)
  }
}

fn await_scherzo_review_validator(
  process: port.Process,
  validator_name: String,
) -> Result(Nil, NamedValidatorError) {
  case port.await_exit(process, 30_000) {
    Ok(0) -> {
      let _cleanup_result = port.terminate(process)
      Ok(Nil)
    }
    Ok(status) -> {
      let diagnostics = validator_diagnostics(process)
      let _cleanup_result = port.terminate(process)
      Error(
        NamedValidatorError(validator_exit_message(
          validator_name,
          status,
          diagnostics,
        )),
      )
    }
    Error(error) -> {
      let diagnostics = validator_diagnostics(process)
      let _cleanup_result = port.terminate(process)
      Error(NamedValidatorError(
        "structured-output validator "
        <> validator_name
        <> " failed: "
        <> port.port_error_to_string(error)
        <> diagnostics_suffix(diagnostics),
      ))
    }
  }
}

fn validator_diagnostics(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(diagnostics) -> string.trim(diagnostics)
    Error(error) ->
      "could not read validator diagnostics: "
      <> port.port_error_to_string(error)
  }
}

fn validator_exit_message(
  validator_name: String,
  status: Int,
  diagnostics: String,
) -> String {
  case diagnostics == "" {
    True ->
      "structured-output validator "
      <> validator_name
      <> " exited "
      <> int.to_string(status)
    False -> diagnostics
  }
}

fn diagnostics_suffix(diagnostics: String) -> String {
  case diagnostics == "" {
    True -> ""
    False -> ": " <> diagnostics
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
