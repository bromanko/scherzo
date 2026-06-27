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

const default_helper_timeout_ms = 30_000

const helper_timeout_env = "SCHERZO_JSON_SCHEMA_HELPER_TIMEOUT_MS"

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
  run_json_schema_validator_with_env(
    declaration,
    value,
    context,
    secrets,
    path.env,
  )
}

pub fn run_json_schema_validator_with_env(
  declaration: workflow_dag.StructuredOutputValidator,
  value: json_value.JsonValue,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
  env_reader: fn(String) -> Option(String),
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
        env_reader,
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
    True -> {
      use Nil <- result.try(validate_schema_path_string(
        context,
        schema_path,
        secrets,
      ))
      validate_schema_path_candidate(context, schema_path, secrets)
    }
  }
}

fn validate_schema_path_string(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
) -> Result(Nil, structured_output_validator.ValidatorFailure) {
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

// Structured-output JSON Schemas are checked lexically first so absolute
// paths and parent traversal remain rejected before invoking the helper.
// The workflow-bundle sharing contract intentionally allows schemas below
// .scherzo/workflows/schemas/ to resolve through repository-local symlinks to
// shared schema directories. Other schema paths keep the older resolved-target
// confinement.
fn validate_schema_path_candidate(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
) -> Result(Nil, structured_output_validator.ValidatorFailure) {
  use resolved_repository_root <- result.try(resolve_repository_root(
    context,
    secrets,
  ))
  use candidate_path <- result.try(validate_schema_path_lexically_confined(
    context,
    schema_path,
    secrets,
  ))

  case repo_local_shared_schema_path(schema_path) {
    True -> Ok(Nil)
    False ->
      validate_schema_path_resolved(
        context,
        schema_path,
        candidate_path,
        resolved_repository_root,
        secrets,
      )
  }
}

fn resolve_repository_root(
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(String, structured_output_validator.ValidatorFailure) {
  case path.realpath(context.repository_root) {
    Error(Nil) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "could not resolve repository root for JSON Schema validation",
        False,
        "repository_root=" <> context.repository_root,
        False,
        False,
        secrets,
      ))
    Ok(repository_root) -> Ok(repository_root)
  }
}

fn validate_schema_path_lexically_confined(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
) -> Result(String, structured_output_validator.ValidatorFailure) {
  let candidate = path.join(context.repository_root, schema_path)
  case path.absolute(context.repository_root), path.absolute(candidate) {
    Ok(repository_root), Ok(candidate_path) ->
      case
        path.contains(
          strip_trailing_current_dir(repository_root),
          candidate_path,
        )
      {
        True -> Ok(candidate_path)
        False ->
          Error(failure(
            context,
            "structured_output_json_schema_config_error",
            "schema path is not lexically confined to the repository: "
              <> schema_path,
            False,
            "schema_file="
              <> schema_path
              <> " candidate_schema_path="
              <> candidate_path
              <> " repository_root="
              <> repository_root,
            False,
            False,
            secrets,
          ))
      }
    _, _ ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "could not normalize schema path for JSON Schema validation: "
          <> schema_path,
        False,
        "schema_file=" <> schema_path,
        False,
        False,
        secrets,
      ))
  }
}

fn validate_schema_path_resolved(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  candidate_path: String,
  repository_root: String,
  secrets: List(String),
) -> Result(Nil, structured_output_validator.ValidatorFailure) {
  case path.realpath(candidate_path) {
    Error(Nil) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "schema file not found or could not be resolved: " <> schema_path,
        False,
        "schema_file=" <> schema_path,
        False,
        False,
        secrets,
      ))
    Ok(resolved_schema_path) ->
      case path.contains(repository_root, resolved_schema_path) {
        True -> Ok(Nil)
        False ->
          Error(failure(
            context,
            "structured_output_json_schema_config_error",
            "schema path resolves outside the repository: " <> schema_path,
            False,
            "schema_file="
              <> schema_path
              <> " resolved_schema_path="
              <> resolved_schema_path
              <> " repository_root="
              <> repository_root,
            False,
            False,
            secrets,
          ))
      }
  }
}

fn repo_local_shared_schema_path(schema_path: String) -> Bool {
  string.starts_with(schema_path, ".scherzo/workflows/schemas/")
}

fn strip_trailing_current_dir(value: String) -> String {
  case value == "/." {
    True -> "/"
    False ->
      case string.ends_with(value, "/.") {
        True -> string.drop_end(value, 2)
        False -> value
      }
  }
}

fn run_helper(
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  draft: String,
  payload_json: String,
  secrets: List(String),
  env_reader: fn(String) -> Option(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  let helper = helper_executable(env_reader)
  case
    port.start_argv_with_input(
      helper,
      ["--schema", schema_path, "--draft", draft],
      context.repository_root,
      allowlisted_parent_env(env_reader),
      payload_json,
    )
  {
    Error(error) ->
      Error(failure(
        context,
        "structured_output_json_schema_config_error",
        "could not start JSON Schema validator helper `"
          <> helper
          <> "`: "
          <> port.port_error_to_string(error),
        False,
        "",
        False,
        False,
        secrets,
      ))
    Ok(process) ->
      read_helper_result(process, context, schema_path, secrets, env_reader)
  }
}

fn read_helper_result(
  process: port.Process,
  context: structured_output_validator.ValidatorContext,
  schema_path: String,
  secrets: List(String),
  env_reader: fn(String) -> Option(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  let timeout_ms = helper_timeout(env_reader)
  case port.read_stdout_line(process, timeout_ms) {
    Ok(line) -> {
      let status = await_status(process, env_reader)
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

fn await_status(
  process: port.Process,
  env_reader: fn(String) -> Option(String),
) -> Int {
  case port.await_exit(process, helper_timeout(env_reader)) {
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

fn helper_executable(env_reader: fn(String) -> Option(String)) -> String {
  case env_reader("SCHERZO_JSON_SCHEMA_HELPER") {
    Some(value) -> value
    None -> "scripts/scherzo-json-schema-validate"
  }
}

fn helper_timeout(env_reader: fn(String) -> Option(String)) -> Int {
  case env_reader(helper_timeout_env) {
    Some(raw) ->
      case int.parse(raw) {
        Ok(value) ->
          case value > 0 {
            True -> value
            False -> default_helper_timeout_ms
          }
        Error(Nil) -> default_helper_timeout_ms
      }
    None -> default_helper_timeout_ms
  }
}

fn allowlisted_parent_env(
  env_reader: fn(String) -> Option(String),
) -> List(#(String, String)) {
  allowlisted_parent_env_loop(
    ["PATH", "LANG", "LC_ALL", "TMPDIR"],
    [],
    env_reader,
  )
}

fn allowlisted_parent_env_loop(
  keys: List(String),
  acc: List(#(String, String)),
  env_reader: fn(String) -> Option(String),
) -> List(#(String, String)) {
  case keys {
    [] -> list.reverse(acc)
    [key, ..rest] ->
      case env_reader(key) {
        Some(value) ->
          allowlisted_parent_env_loop(rest, [#(key, value), ..acc], env_reader)
        None -> allowlisted_parent_env_loop(rest, acc, env_reader)
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
