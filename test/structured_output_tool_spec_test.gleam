import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/json_value
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag
import simplifile

fn spec(schema_path: String) -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_lane_draft",
    required: True,
    source: structured_output_source.PiToolCallSource(
      tool_name: "submit_structured_output",
      parameters_schema_path: Some(schema_path),
    ),
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
    ]),
    validators: [
      workflow_dag.JsonSchemaValidator(
        name: "review_lane_draft_schema",
        path: schema_path,
        draft: Some("2020-12"),
      ),
    ],
    validation_retries: 1,
  )
}

fn build(
  schema_path: String,
) -> Result(
  structured_output_tool_spec.ToolSpec,
  structured_output_tool_spec.ToolSpecError,
) {
  structured_output_tool_spec.for_step(structured_output_tool_spec.BuildInput(
    workflow_id: "implementation",
    run_id: "run-1",
    step_id: "lane_correctness",
    attempt_index: 2,
    repository_root: ".",
    spec: spec(schema_path),
  ))
}

fn assert_error_code(
  result: Result(a, structured_output_tool_spec.ToolSpecError),
  code: String,
) -> Nil {
  let assert Error(error) = result
  assert error.code == code
}

fn assert_error_contains(
  result: Result(a, structured_output_tool_spec.ToolSpecError),
  code: String,
  text: String,
) -> Nil {
  let assert Error(error) = result
  assert error.code == code
  assert string.contains(error.message, text)
}

fn json_object_field(
  value: json_value.JsonValue,
  key: String,
) -> Option(json_value.JsonValue) {
  case value {
    json_value.JObject(entries) -> json_object_field_loop(entries, key)
    _ -> None
  }
}

fn json_object_field_loop(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> json_object_field_loop(rest, key)
      }
  }
}

pub fn structured_output_tool_spec_builds_provider_schema_spec_test() {
  let assert Ok(tool_spec) =
    build(
      ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )

  assert tool_spec.workflow_id == "implementation"
  assert tool_spec.run_id == "run-1"
  assert tool_spec.step_id == "lane_correctness"
  assert tool_spec.attempt_index == 2
  assert tool_spec.artifact_name == "review_lane_draft"
  assert tool_spec.tool_name == "submit_structured_output"
  assert tool_spec.parameters_schema_path
    == ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json"
  assert string.length(tool_spec.parameters_schema_sha256) == 64
  let assert json_value.JObject(_) = tool_spec.parameters_schema
  assert json_object_field(tool_spec.parameters_schema, "type")
    == Some(json_value.JString("object"))
  assert json_object_field(tool_spec.parameters_schema, "oneOf") == None
  assert json_object_field(tool_spec.parameters_schema, "anyOf") == None
  assert json_object_field(tool_spec.parameters_schema, "allOf") == None
  assert json_object_field(tool_spec.parameters_schema, "enum") == None
  assert json_object_field(tool_spec.parameters_schema, "not") == None
  assert tool_spec.terminate

  let encoded = structured_output_tool_spec.to_string(tool_spec)
  let assert Ok(_) = json_value.parse(encoded)
  assert string.contains(encoded, "\"parameters_schema\":")
  assert string.contains(encoded, "\"terminate\":true")
  assert !string.contains(encoded, "\"require_single\"")
  assert !string.contains(encoded, "\"reject_sibling_tool_calls\"")
}

pub fn structured_output_tool_spec_rejects_unsafe_schema_paths_test() {
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("/tmp/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("../schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("$SCHEMA/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("C:/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path(
      "<absolute-local-path>/schema.json",
    ),
    "structured_output_parameters_schema_path_invalid",
  )
}

pub fn structured_output_tool_spec_reports_malformed_schema_json_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/malformed.schema.json"
  let assert Ok(Nil) = simplifile.write(schema_path, "{ not json\n")

  assert_error_code(
    build(schema_path),
    "structured_output_tool_spec_schema_malformed_json",
  )
}

pub fn structured_output_tool_spec_rejects_provider_incompatible_schema_type_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/array.schema.json"
  let assert Ok(Nil) = simplifile.write(schema_path, "{\"type\":\"array\"}\n")

  assert_error_code(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
  )
}

pub fn structured_output_tool_spec_rejects_provider_incompatible_top_level_keywords_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/all-of.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"allOf\":[{\"type\":\"object\"}]}\n",
    )

  assert_error_code(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
  )
}

pub fn structured_output_tool_spec_accepts_nested_provider_safe_keywords_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/nested-ok.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"draft_findings\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"description\":\"finding\",\"required\":[\"severity\"],\"properties\":{\"severity\":{\"type\":\"string\",\"minLength\":1,\"maxLength\":20}}}}},\"additionalProperties\":false}\n",
    )

  let assert Ok(_) = build(schema_path)
}

pub fn structured_output_tool_spec_rejects_nested_enum_keyword_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/nested-enum.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"draft_findings\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"severity\":{\"type\":\"string\",\"enum\":[\"low\"]}}}}}}\n",
    )

  assert_error_contains(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
    "/properties/draft_findings/items/properties/severity/enum",
  )
}

pub fn structured_output_tool_spec_rejects_nested_const_keyword_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/nested-const.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"self_check\":{\"type\":\"object\",\"properties\":{\"summary\":{\"type\":\"string\",\"const\":\"ok\"}}}}}\n",
    )

  assert_error_contains(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
    "/properties/self_check/properties/summary/const",
  )
}

pub fn structured_output_tool_spec_rejects_nested_all_of_keyword_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/nested-all-of.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"target\":{\"type\":\"object\",\"allOf\":[{\"type\":\"object\"}]}}}\n",
    )

  assert_error_contains(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
    "/properties/target/allOf",
  )
}

pub fn structured_output_tool_spec_rejects_nested_ref_keyword_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/nested-ref.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"target\":{\"$ref\":\"#/$defs/Target\"}}}\n",
    )

  assert_error_contains(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
    "/properties/target/$ref",
  )
}

pub fn structured_output_tool_spec_rejects_union_type_array_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/union-type.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"summary\":{\"type\":[\"string\",\"null\"]}}}\n",
    )

  assert_error_contains(
    build(schema_path),
    "structured_output_tool_spec_provider_incompatible_schema",
    "/properties/summary/type",
  )
}

pub fn structured_output_tool_spec_writes_retained_path_test() {
  let run_root = "test/tmp/structured-output-tool-spec-write/run-root"
  let _ = simplifile.delete(run_root)
  let assert Ok(tool_spec) =
    build(
      ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )
  let assert Ok(written) =
    structured_output_tool_spec.write(tool_spec, run_root)

  assert written.run_root_relative_path
    == "artifacts/structured-output-specs/lane_correctness-attempt-2.json"
  assert string.contains(
    written.env_path,
    "artifacts/structured-output-specs/lane_correctness-attempt-2.json",
  )
  let assert Ok(contents) = simplifile.read(written.env_path)
  assert string.contains(contents, "submit_structured_output")
  assert structured_output_tool_spec.env_pair(written).0
    == structured_output_tool_spec.spec_env_var
}

pub fn structured_output_tool_spec_requires_generic_schema_path_test() {
  let legacy_spec =
    workflow_dag.StructuredOutputSpec(
      artifact_name: "review_lane_draft",
      required: True,
      source: structured_output_source.PiToolCallSource(
        tool_name: "submit_structured_output",
        parameters_schema_path: None,
      ),
      format: workflow_dag.StructuredJson,
      schema: workflow_dag.StructuredObjectSchema([]),
      validators: [],
      validation_retries: 1,
    )
  assert_error_code(
    structured_output_tool_spec.for_step(structured_output_tool_spec.BuildInput(
      workflow_id: "implementation",
      run_id: "run-1",
      step_id: "lane_correctness",
      attempt_index: 1,
      repository_root: ".",
      spec: legacy_spec,
    )),
    "structured_output_tool_spec_missing_schema_path",
  )
}

pub fn structured_output_tool_spec_builds_every_workflow_parameters_schema_test() {
  workflow_files(".scherzo/workflows")
  |> list.each(validate_workflow_parameters_schemas)
}

fn workflow_files(root: String) -> List(String) {
  let assert Ok(entries) = simplifile.read_directory(root)

  entries
  |> list.sort(by: string.compare)
  |> list.fold([], fn(paths, entry) {
    let path = root <> "/" <> entry
    let assert Ok(is_directory) = simplifile.is_directory(path)

    case is_directory {
      True -> list.append(workflow_files(path), paths)
      False ->
        case string.ends_with(path, ".yaml") || string.ends_with(path, ".yml") {
          True -> [path, ..paths]
          False -> paths
        }
    }
  })
}

fn validate_workflow_parameters_schemas(path: String) -> Nil {
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(dag) = workflow_dag.parse(contents)

  workflow_dag.steps(dag)
  |> list.each(fn(step) {
    case step.kind {
      workflow_dag.AgentStep(_, Some(spec)) ->
        validate_structured_output_parameters_schema(
          workflow_dag.id(dag),
          step.id,
          spec,
        )
      _ -> Nil
    }
  })
}

fn validate_structured_output_parameters_schema(
  workflow_id: String,
  step_id: String,
  spec: workflow_dag.StructuredOutputSpec,
) -> Nil {
  case structured_output_source.parameters_schema_path(spec.source) {
    Some(_) -> {
      let assert Ok(_) =
        structured_output_tool_spec.for_step(
          structured_output_tool_spec.BuildInput(
            workflow_id: workflow_id,
            run_id: "schema-guardrail",
            step_id: step_id,
            attempt_index: 1,
            repository_root: ".",
            spec: spec,
          ),
        )
      Nil
    }
    None -> Nil
  }
}
