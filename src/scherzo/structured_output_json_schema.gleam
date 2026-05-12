import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/path
import scherzo/port
import scherzo/structured_output_validator
import scherzo/workflow_dag

const helper_timeout_ms = 30_000

type HelperDiagnostic {
  HelperDiagnostic(
    status: String,
    code: String,
    message: String,
    instance_path: String,
    schema_path: String,
    schema_file: String,
    draft: String,
  )
}

pub fn run_json_schema_validator(
  declaration: workflow_dag.StructuredOutputValidator,
  value: json_value.JsonValue,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case declaration {
    workflow_dag.JsonSchemaValidator(path: schema_path, draft: draft, ..) -> {
      let effective_draft = draft |> option_string("2020-12")
      use Nil <- result.try(validate_schema_declaration(
        context,
        schema_path,
        effective_draft,
        secrets,
      ))
      run_helper(
        context,
        schema_path,
        effective_draft,
        json_value.to_string(value) <> "\n",
        secrets,
      )
    }
    workflow_dag.CommandValidator(..) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "internal error: command validator passed to JSON Schema runner",
        False,
        "",
        False,
        False,
        secrets,
      ))
  }
}

fn validate_schema_declaration(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  draft: String,
  secrets: List(String),
) -> Result(Nil, structured_output_validator.ValidatorFailure) {
  case draft == "2020-12" {
    False ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "unsupported JSON Schema draft: " <> draft,
        False,
        "",
        False,
        False,
        secrets,
      ))
    True ->
      case valid_repository_relative_path(schema_path) {
        True -> Ok(Nil)
        False ->
          Error(failure(
            context,
            "structured_output_json_schema_config_error",
            "schema path must be repository-relative and confined to the repository: "
              <> schema_path,
            False,
            "",
            False,
            False,
            secrets,
          ))
      }
  }
}

fn run_helper(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  draft: String,
  payload_json: String,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case
    port.start_argv_with_input(
      "python3",
      [
        "scripts/scherzo-json-schema-validate",
        "--schema",
        schema_path,
        "--draft",
        draft,
      ],
      context.repository_root,
      allowlisted_parent_env(),
      payload_json,
    )
  {
    Error(error) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "could not start JSON Schema validator helper: "
          <> port.port_error_to_string(error),
        False,
        "",
        False,
        False,
        secrets,
      ))
    Ok(process) -> read_helper_result(process, context, schema_path, secrets)
  }
}

fn read_helper_result(
  process: port.Process,
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case port.read_stdout_line(process, helper_timeout_ms) {
    Ok(line) -> {
      let status = await_status(process)
      finish_helper_result(status, line, context, schema_path, secrets)
    }
    Error(port.ProcessExited(status)) ->
      finish_helper_result(status, "", context, schema_path, secrets)
    Error(port.ReadTimeout) -> {
      let diagnostics = read_stderr(process)
      let _terminate_result = port.terminate(process)
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "JSON Schema validator helper timed out",
        False,
        diagnostics,
        False,
        False,
        secrets,
      ))
    }
    Error(error) -> {
      let diagnostics = read_stderr(process)
      let _terminate_result = port.terminate(process)
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "JSON Schema validator helper failed: "
          <> port.port_error_to_string(error),
        False,
        diagnostics,
        False,
        False,
        secrets,
      ))
    }
  }
}

fn await_status(process: port.Process) -> Int {
  case port.await_exit(process, helper_timeout_ms) {
    Ok(status) -> status
    Error(await_error) -> {
      let _reason = port.port_error_to_string(await_error)
      2
    }
  }
}

fn finish_helper_result(
  status: Int,
  line: String,
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case decode_helper_diagnostic(line) {
    Ok(diagnostic) ->
      helper_diagnostic_result(status, diagnostic, context, secrets)
    Error(Nil) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "JSON Schema validator helper returned malformed diagnostics for "
          <> schema_path
          <> " with exit status "
          <> int.to_string(status),
        False,
        line,
        False,
        False,
        secrets,
      ))
  }
}

fn helper_diagnostic_result(
  status: Int,
  diagnostic: HelperDiagnostic,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case status, diagnostic.status {
    0, "accepted" -> Ok(structured_output_validator.ValidatorPass)
    1, _ ->
      Error(failure(
        context,
        "structured_output_json_schema_rejected",
        json_schema_rejected_message(diagnostic),
        True,
        json_schema_diagnostic_summary(diagnostic),
        False,
        False,
        secrets,
      ))
    _, _ ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        diagnostic.message,
        False,
        json_schema_diagnostic_summary(diagnostic),
        False,
        False,
        secrets,
      ))
  }
}

fn json_schema_rejected_message(diagnostic: HelperDiagnostic) -> String {
  let instance = case diagnostic.instance_path == "" {
    True -> ""
    False -> " at " <> diagnostic.instance_path
  }
  "JSON Schema rejected payload" <> instance <> ": " <> diagnostic.message
}

fn json_schema_diagnostic_summary(diagnostic: HelperDiagnostic) -> String {
  let instance = case diagnostic.instance_path == "" {
    True -> ""
    False -> " instance_path=" <> diagnostic.instance_path
  }
  let schema = case diagnostic.schema_path == "" {
    True -> ""
    False -> " schema_path=" <> diagnostic.schema_path
  }
  "schema_file="
  <> diagnostic.schema_file
  <> instance
  <> schema
  <> " message="
  <> diagnostic.message
}

fn decode_helper_diagnostic(line: String) -> Result(HelperDiagnostic, Nil) {
  json.parse(line, helper_diagnostic_decoder())
  |> result.replace_error(Nil)
}

fn helper_diagnostic_decoder() -> decode.Decoder(HelperDiagnostic) {
  use status <- decode.field("status", decode.string)
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  use instance_path <- decode.field("instance_path", decode.string)
  use schema_path <- decode.field("schema_path", decode.string)
  use schema_file <- decode.field("schema_file", decode.string)
  use draft <- decode.field("draft", decode.string)
  decode.success(HelperDiagnostic(
    status: status,
    code: code,
    message: message,
    instance_path: instance_path,
    schema_path: schema_path,
    schema_file: schema_file,
    draft: draft,
  ))
}

fn read_stderr(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(value) -> value
    Error(error) -> port.port_error_to_string(error)
  }
}

fn allowlisted_parent_env() -> List(#(String, String)) {
  allowlisted_parent_env_loop(["PATH", "LANG", "LC_ALL", "TMPDIR"], [])
}

fn allowlisted_parent_env_loop(
  keys: List(String),
  acc: List(#(String, String)),
) -> List(#(String, String)) {
  case keys {
    [] -> list.reverse(acc)
    [key, ..rest] ->
      case path.env(key) {
        Some(value) -> allowlisted_parent_env_loop(rest, [#(key, value), ..acc])
        None -> allowlisted_parent_env_loop(rest, acc)
      }
  }
}

fn failure(
  context: structured_output_validator.ValidatorContext,
  code: String,
  message: String,
  retryable: Bool,
  diagnostic_summary: String,
  stdout_truncated: Bool,
  stderr_truncated: Bool,
  secrets: List(String),
) -> structured_output_validator.ValidatorFailure {
  structured_output_validator.ValidatorFailure(
    validator_name: context.validator_name,
    validator_type: "json_schema",
    code: code,
    message: redact(message, secrets),
    retryable: retryable,
    diagnostic_summary: redact_summary(diagnostic_summary, secrets),
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
  )
}

fn redact(value: String, secrets: List(String)) -> String {
  log.redact("structured_output_json_schema", value, secrets)
}

fn redact_summary(value: String, secrets: List(String)) -> String {
  value
  |> redact(secrets)
  |> log.truncate(1000)
}

fn option_string(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn valid_repository_relative_path(value: String) -> Bool {
  value != "" && !string.starts_with(value, "/") && !has_parent_segment(value)
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}
