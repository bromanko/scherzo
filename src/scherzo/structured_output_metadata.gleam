import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/hash
import scherzo/path
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

pub type BaselineValidationMetadata {
  BaselineValidationMetadata(schema_type: String, required_keys: List(String))
}

pub type ValidatorValidationMetadata {
  JsonSchemaValidationMetadata(
    name: String,
    status: String,
    schema_path: String,
    schema_sha256: String,
    draft: String,
  )
  CommandValidationMetadata(
    name: String,
    status: String,
    argv: List(String),
    argv_sha256: String,
    timeout_ms: Int,
    working_directory: String,
    env_keys: List(String),
  )
}

pub type ValidationMetadata {
  ValidationMetadata(
    source_type: String,
    source_tool_name: Option(String),
    source_parameters_schema_path: Option(String),
    source_parameters_schema_sha256: Option(String),
    source_receipt_json: Option(String),
    baseline: BaselineValidationMetadata,
    validators: List(ValidatorValidationMetadata),
  )
}

pub type ValidatorSummary {
  ValidatorSummary(name: String, validator_type: String, status: String)
}

pub fn baseline_only(required_keys: List(String)) -> ValidationMetadata {
  ValidationMetadata(
    source_type: "final_response",
    source_tool_name: None,
    source_parameters_schema_path: None,
    source_parameters_schema_sha256: None,
    source_receipt_json: None,
    baseline: BaselineValidationMetadata(
      schema_type: "object",
      required_keys: required_keys,
    ),
    validators: [],
  )
}

pub fn from_spec(
  spec: workflow_dag.StructuredOutputSpec,
  repository_root: String,
) -> ValidationMetadata {
  from_spec_with_receipt(spec, repository_root, None)
}

pub fn from_spec_with_receipt(
  spec: workflow_dag.StructuredOutputSpec,
  repository_root: String,
  receipt_json: Option(String),
) -> ValidationMetadata {
  let workflow_dag.StructuredObjectSchema(required_keys) = spec.schema
  let schema_path = structured_output_source.parameters_schema_path(spec.source)
  ValidationMetadata(
    source_type: structured_output_source.type_to_string(spec.source),
    source_tool_name: structured_output_source.tool_name(spec.source),
    source_parameters_schema_path: schema_path,
    source_parameters_schema_sha256: option_map(schema_path, fn(path_value) {
      schema_sha256(repository_root, path_value)
    }),
    source_receipt_json: receipt_json,
    baseline: BaselineValidationMetadata(
      schema_type: "object",
      required_keys: required_keys,
    ),
    validators: list.map(spec.validators, validator_from_declaration(
      _,
      repository_root,
    )),
  )
}

pub fn required_keys(metadata: ValidationMetadata) -> List(String) {
  metadata.baseline.required_keys
}

pub fn validator_summaries(
  metadata: ValidationMetadata,
) -> List(ValidatorSummary) {
  list.map(metadata.validators, validator_summary)
}

pub fn to_json(metadata: ValidationMetadata) -> json.Json {
  json.object([
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
    #("baseline", baseline_to_json(metadata.baseline)),
    #("validators", json.array(metadata.validators, of: validator_to_json)),
  ])
}

pub fn decoder() -> decode.Decoder(ValidationMetadata) {
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
  use baseline <- decode.field("baseline", baseline_decoder())
  use validators <- decode.field("validators", decode.list(validator_decoder()))
  decode.success(ValidationMetadata(
    source_type: source_type,
    source_tool_name: source_tool_name,
    source_parameters_schema_path: source_parameters_schema_path,
    source_parameters_schema_sha256: source_parameters_schema_sha256,
    source_receipt_json: source_receipt_json,
    baseline: baseline,
    validators: validators,
  ))
}

pub fn summary_to_json(summary: ValidatorSummary) -> json.Json {
  json.object([
    #("name", json.string(summary.name)),
    #("type", json.string(summary.validator_type)),
    #("status", json.string(summary.status)),
  ])
}

pub fn summary_decoder() -> decode.Decoder(ValidatorSummary) {
  use name <- decode.field("name", decode.string)
  use validator_type <- decode.field("type", decode.string)
  use status <- decode.field("status", decode.string)
  decode.success(ValidatorSummary(
    name: name,
    validator_type: validator_type,
    status: status,
  ))
}

fn validator_from_declaration(
  validator: workflow_dag.StructuredOutputValidator,
  repository_root: String,
) -> ValidatorValidationMetadata {
  case validator {
    workflow_dag.JsonSchemaValidator(
      name: name,
      path: schema_path,
      draft: draft,
    ) ->
      JsonSchemaValidationMetadata(
        name: name,
        status: "passed",
        schema_path: schema_path,
        schema_sha256: schema_sha256(repository_root, schema_path),
        draft: option_string(draft, "2020-12"),
      )
    workflow_dag.CommandValidator(
      name: name,
      argv: argv,
      timeout_ms: timeout_ms,
      working_directory: working_directory,
      env: env,
    ) ->
      CommandValidationMetadata(
        name: name,
        status: "passed",
        argv: argv,
        argv_sha256: argv_sha256(argv),
        timeout_ms: timeout_ms,
        working_directory: workflow_dag.validator_working_directory_to_string(
          working_directory,
        ),
        env_keys: env_keys(env),
      )
  }
}

fn validator_summary(
  metadata: ValidatorValidationMetadata,
) -> ValidatorSummary {
  case metadata {
    JsonSchemaValidationMetadata(name: name, status: status, ..) ->
      ValidatorSummary(
        name: name,
        validator_type: "json_schema",
        status: status,
      )
    CommandValidationMetadata(name: name, status: status, ..) ->
      ValidatorSummary(name: name, validator_type: "command", status: status)
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn baseline_to_json(metadata: BaselineValidationMetadata) -> json.Json {
  json.object([
    #("schema_type", json.string(metadata.schema_type)),
    #("required_keys", json.array(metadata.required_keys, of: json.string)),
  ])
}

fn validator_to_json(metadata: ValidatorValidationMetadata) -> json.Json {
  case metadata {
    JsonSchemaValidationMetadata(
      name,
      status,
      schema_path,
      schema_sha256,
      draft,
    ) ->
      json.object([
        #("name", json.string(name)),
        #("type", json.string("json_schema")),
        #("status", json.string(status)),
        #("schema_path", json.string(schema_path)),
        #("schema_sha256", json.string(schema_sha256)),
        #("draft", json.string(draft)),
      ])
    CommandValidationMetadata(
      name,
      status,
      argv,
      argv_sha256,
      timeout_ms,
      working_directory,
      env_keys,
    ) ->
      json.object([
        #("name", json.string(name)),
        #("type", json.string("command")),
        #("status", json.string(status)),
        #("argv", json.array(argv, of: json.string)),
        #("argv_sha256", json.string(argv_sha256)),
        #("timeout_ms", json.int(timeout_ms)),
        #("working_directory", json.string(working_directory)),
        #("env_keys", json.array(env_keys, of: json.string)),
      ])
  }
}

fn baseline_decoder() -> decode.Decoder(BaselineValidationMetadata) {
  use schema_type <- decode.field("schema_type", decode.string)
  use required_keys <- decode.field("required_keys", decode.list(decode.string))
  decode.success(BaselineValidationMetadata(
    schema_type: schema_type,
    required_keys: required_keys,
  ))
}

fn validator_decoder() -> decode.Decoder(ValidatorValidationMetadata) {
  use validator_type <- decode.field("type", decode.string)
  case validator_type {
    "json_schema" -> json_schema_validator_decoder()
    "command" -> command_validator_decoder()
    _ ->
      decode.failure(
        JsonSchemaValidationMetadata("", "", "", "", ""),
        expected: "structured-output validator metadata",
      )
  }
}

fn json_schema_validator_decoder() -> decode.Decoder(
  ValidatorValidationMetadata,
) {
  use name <- decode.field("name", decode.string)
  use status <- decode.field("status", decode.string)
  use schema_path <- decode.field("schema_path", decode.string)
  use schema_sha256 <- decode.optional_field("schema_sha256", "", decode.string)
  use draft <- decode.optional_field("draft", "2020-12", decode.string)
  decode.success(JsonSchemaValidationMetadata(
    name: name,
    status: status,
    schema_path: schema_path,
    schema_sha256: schema_sha256,
    draft: draft,
  ))
}

fn command_validator_decoder() -> decode.Decoder(ValidatorValidationMetadata) {
  use name <- decode.field("name", decode.string)
  use status <- decode.field("status", decode.string)
  use argv <- decode.optional_field("argv", [], decode.list(decode.string))
  use argv_sha256 <- decode.optional_field("argv_sha256", "", decode.string)
  use timeout_ms <- decode.optional_field("timeout_ms", 30_000, decode.int)
  use working_directory <- decode.optional_field(
    "working_directory",
    "workspace",
    decode.string,
  )
  use env_keys <- decode.optional_field(
    "env_keys",
    [],
    decode.list(decode.string),
  )
  decode.success(CommandValidationMetadata(
    name: name,
    status: status,
    argv: argv,
    argv_sha256: argv_sha256,
    timeout_ms: timeout_ms,
    working_directory: working_directory,
    env_keys: env_keys,
  ))
}

fn option_string(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn option_map(value: Option(a), mapper: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(mapper(value))
    None -> None
  }
}

fn schema_sha256(repository_root: String, schema_path: String) -> String {
  case simplifile.read(path.join(repository_root, schema_path)) {
    Ok(contents) -> hash.sha256_hex(contents)
    Error(read_error) -> {
      let _reason = simplifile.describe_error(read_error)
      ""
    }
  }
}

fn argv_sha256(argv: List(String)) -> String {
  argv
  |> json.array(of: json.string)
  |> json.to_string
  |> hash.sha256_hex
}

fn env_keys(env: List(#(String, String))) -> List(String) {
  env
  |> list.map(fn(entry) {
    let #(key, _) = entry
    key
  })
  |> list.sort(by: string.compare)
}
