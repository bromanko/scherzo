import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/json_value
import scherzo/path
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

pub const spec_env_var = "SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH"

const artifact_type = "scherzo_structured_output_tool_spec"

pub type ToolSpec {
  ToolSpec(
    workflow_id: String,
    run_id: String,
    step_id: String,
    attempt_index: Int,
    artifact_name: String,
    tool_name: String,
    label: String,
    description: String,
    prompt_snippet: String,
    prompt_guidelines: List(String),
    parameters_schema_path: String,
    parameters_schema_sha256: String,
    parameters_schema: json_value.JsonValue,
    require_single: Bool,
    reject_sibling_tool_calls: Bool,
    terminate: Bool,
  )
}

pub type WrittenToolSpec {
  WrittenToolSpec(env_path: String, run_root_relative_path: String)
}

pub type BuildInput {
  BuildInput(
    workflow_id: String,
    run_id: String,
    step_id: String,
    attempt_index: Int,
    repository_root: String,
    spec: workflow_dag.StructuredOutputSpec,
  )
}

pub type ToolSpecError {
  ToolSpecError(code: String, message: String)
}

pub fn for_step(input: BuildInput) -> Result(ToolSpec, ToolSpecError) {
  case input.spec.source {
    structured_output_source.PiToolCallSource(
      tool_name,
      require_single,
      reject_sibling_tool_calls,
      Some(schema_path),
    ) ->
      build_for_pi_tool_call(
        input,
        tool_name,
        require_single,
        reject_sibling_tool_calls,
        schema_path,
      )
    structured_output_source.PiToolCallSource(_, _, _, None) ->
      Error(ToolSpecError(
        "structured_output_tool_spec_missing_schema_path",
        "generic structured-output tool spec requires structured_output.source.parameters_schema_path",
      ))
    structured_output_source.FinalResponseSource ->
      Error(ToolSpecError(
        "structured_output_tool_spec_not_pi_tool_call",
        "generic structured-output tool spec can only be built for pi_tool_call sources",
      ))
  }
}

fn build_for_pi_tool_call(
  input: BuildInput,
  tool_name: String,
  require_single: Bool,
  reject_sibling_tool_calls: Bool,
  schema_path: String,
) -> Result(ToolSpec, ToolSpecError) {
  use Nil <- result.try(validate_schema_path(schema_path))
  use Nil <- result.try(validate_supported_policy(
    require_single,
    reject_sibling_tool_calls,
  ))
  use schema <- result.try(read_schema(input.repository_root, schema_path))
  let digest = schema.sha256
  Ok(ToolSpec(
    workflow_id: input.workflow_id,
    run_id: input.run_id,
    step_id: input.step_id,
    attempt_index: input.attempt_index,
    artifact_name: input.spec.artifact_name,
    tool_name: tool_name,
    label: label_for_artifact(input.spec.artifact_name),
    description: description_for_artifact(input.spec.artifact_name),
    prompt_snippet: prompt_snippet_for_tool(tool_name, input.spec.artifact_name),
    prompt_guidelines: prompt_guidelines_for_tool(tool_name),
    parameters_schema_path: schema_path,
    parameters_schema_sha256: digest,
    parameters_schema: schema.value,
    require_single: require_single,
    reject_sibling_tool_calls: reject_sibling_tool_calls,
    terminate: True,
  ))
}

fn validate_supported_policy(
  require_single: Bool,
  reject_sibling_tool_calls: Bool,
) -> Result(Nil, ToolSpecError) {
  case require_single, reject_sibling_tool_calls {
    True, True -> Ok(Nil)
    False, _ ->
      Error(ToolSpecError(
        "structured_output_tool_spec_unsupported_require_single",
        "generic structured-output tool specs require require_single: true",
      ))
    _, False ->
      Error(ToolSpecError(
        "structured_output_tool_spec_unsupported_reject_sibling_tool_calls",
        "generic structured-output tool specs require reject_sibling_tool_calls: true",
      ))
  }
}

pub fn validate_schema_path(schema_path: String) -> Result(Nil, ToolSpecError) {
  case structured_output_source.valid_parameters_schema_path(schema_path) {
    True -> Ok(Nil)
    False ->
      Error(ToolSpecError(
        "structured_output_parameters_schema_path_invalid",
        "parameters_schema_path must be repository-relative and confined to the repository: "
          <> schema_path,
      ))
  }
}

type LoadedSchema {
  LoadedSchema(value: json_value.JsonValue, sha256: String)
}

fn read_schema(
  repository_root: String,
  schema_path: String,
) -> Result(LoadedSchema, ToolSpecError) {
  let full_path = path.join(repository_root, schema_path)
  use contents <- result.try(
    simplifile.read(full_path)
    |> result.map_error(fn(error) {
      ToolSpecError(
        "structured_output_tool_spec_schema_read_failed",
        "could not read parameters schema `"
          <> schema_path
          <> "`: "
          <> simplifile.describe_error(error),
      )
    }),
  )
  case json_value.parse(contents) {
    Ok(value) ->
      case value {
        json_value.JObject(_) ->
          Ok(LoadedSchema(value: value, sha256: hash.sha256_hex(contents)))
        _ ->
          Error(ToolSpecError(
            "structured_output_tool_spec_schema_not_object",
            "parameters schema must be a JSON object: " <> schema_path,
          ))
      }
    Error(Nil) ->
      Error(ToolSpecError(
        "structured_output_tool_spec_schema_malformed_json",
        "parameters schema is not valid JSON: " <> schema_path,
      ))
  }
}

fn label_for_artifact(artifact_name: String) -> String {
  artifact_name
  |> string.replace(each: "_", with: " ")
  |> title_case_words
}

fn title_case_words(value: String) -> String {
  value
}

fn description_for_artifact(artifact_name: String) -> String {
  "Submit the final `"
  <> artifact_name
  <> "` structured-output artifact. This tool has no side effects."
}

fn prompt_snippet_for_tool(tool_name: String, artifact_name: String) -> String {
  "Submit `"
  <> artifact_name
  <> "` by calling `"
  <> tool_name
  <> "` exactly once"
}

fn prompt_guidelines_for_tool(tool_name: String) -> List(String) {
  [
    "Use "
      <> tool_name
      <> " exactly once as the final structured-output action.",
    "Do not print the structured-output object as final assistant JSON; pass it as "
      <> tool_name
      <> " arguments instead.",
    "Do not call sibling tools in the same assistant tool-call batch as "
      <> tool_name
      <> ".",
  ]
}

pub fn to_json(tool_spec: ToolSpec) -> json.Json {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string(artifact_type)),
    #("workflow_id", json.string(tool_spec.workflow_id)),
    #("run_id", json.string(tool_spec.run_id)),
    #("step_id", json.string(tool_spec.step_id)),
    #("attempt_index", json.int(tool_spec.attempt_index)),
    #("artifact_name", json.string(tool_spec.artifact_name)),
    #("tool_name", json.string(tool_spec.tool_name)),
    #("label", json.string(tool_spec.label)),
    #("description", json.string(tool_spec.description)),
    #("prompt_snippet", json.string(tool_spec.prompt_snippet)),
    #(
      "prompt_guidelines",
      json.array(tool_spec.prompt_guidelines, of: json.string),
    ),
    #("parameters_schema_path", json.string(tool_spec.parameters_schema_path)),
    #(
      "parameters_schema_sha256",
      json.string(tool_spec.parameters_schema_sha256),
    ),
    #("parameters_schema", json_value.to_json(tool_spec.parameters_schema)),
    #("require_single", json.bool(tool_spec.require_single)),
    #(
      "reject_sibling_tool_calls",
      json.bool(tool_spec.reject_sibling_tool_calls),
    ),
    #("terminate", json.bool(tool_spec.terminate)),
  ])
}

pub fn to_string(tool_spec: ToolSpec) -> String {
  tool_spec |> to_json |> json.to_string
}

pub fn run_root_relative_path(step_id: String, attempt_index: Int) -> String {
  "artifacts/structured-output-specs/"
  <> step_id
  <> "-attempt-"
  <> int.to_string(attempt_index)
  <> ".json"
}

pub fn write(
  tool_spec: ToolSpec,
  run_root: String,
) -> Result(WrittenToolSpec, ToolSpecError) {
  let relative =
    run_root_relative_path(tool_spec.step_id, tool_spec.attempt_index)
  let output_path = path.join(run_root, relative)
  let directory = case path.dirname(output_path) {
    Ok(value) -> value
    Error(Nil) -> run_root
  }
  use Nil <- result.try(
    simplifile.create_directory_all(directory)
    |> result.map_error(fn(error) {
      ToolSpecError(
        "structured_output_tool_spec_write_failed",
        "could not create structured-output spec directory: "
          <> simplifile.describe_error(error),
      )
    }),
  )
  use Nil <- result.try(
    simplifile.write(output_path, to_string(tool_spec) <> "\n")
    |> result.map_error(fn(error) {
      ToolSpecError(
        "structured_output_tool_spec_write_failed",
        "could not write structured-output tool spec: "
          <> simplifile.describe_error(error),
      )
    }),
  )
  Ok(WrittenToolSpec(env_path: output_path, run_root_relative_path: relative))
}

pub fn env_pair(written: WrittenToolSpec) -> #(String, String) {
  #(spec_env_var, written.env_path)
}

pub fn schema_path_for_source(
  source: structured_output_source.StructuredOutputSource,
) -> Option(String) {
  structured_output_source.parameters_schema_path(source)
}
