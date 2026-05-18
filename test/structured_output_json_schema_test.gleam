import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/hash
import scherzo/json_value
import scherzo/path as scherzo_path
import scherzo/structured_output_json_schema
import scherzo/structured_output_metadata
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile

const helper_env = "SCHERZO_JSON_SCHEMA_HELPER"

const helper_timeout_env = "SCHERZO_JSON_SCHEMA_HELPER_TIMEOUT_MS"

fn payload(path: String) -> json_value.JsonValue {
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(value) = json_value.parse(contents)
  value
}

fn context(
  validator: workflow_dag.StructuredOutputValidator,
) -> structured_output_validator.ValidatorContext {
  context_with_repo(validator, ".")
}

fn context_with_repo(
  validator: workflow_dag.StructuredOutputValidator,
  repository_root: String,
) -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    repository_root,
    "test/tmp/run-root",
    "test_workflow",
    "run-1",
    "review",
    1,
    repository_root,
    "review_lane_draft",
    "json",
    "final_response",
    None,
  )
  |> structured_output_validator.for_validator(validator, 0)
}

fn schema_validator(path: String) -> workflow_dag.StructuredOutputValidator {
  schema_validator_with_draft(path, Some("2020-12"))
}

fn schema_validator_with_draft(
  path: String,
  draft: Option(String),
) -> workflow_dag.StructuredOutputValidator {
  workflow_dag.JsonSchemaValidator(
    name: "review_lane_shape",
    path: path,
    draft: draft,
  )
}

fn valid_payload() -> json_value.JsonValue {
  payload("test/fixtures/structured_output/review_lane_payload_valid.json")
}

fn run_validator(
  validator: workflow_dag.StructuredOutputValidator,
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  structured_output_json_schema.run_json_schema_validator(
    validator,
    valid_payload(),
    context(validator),
    [],
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn with_env(key: String, value: String, run: fn() -> a) -> a {
  let previous = scherzo_path.env(key)
  let assert Ok(Nil) = scherzo_path.set_env(key, value)
  let result = run()
  restore_env(key, previous)
  result
}

fn with_two_envs(
  first_key: String,
  first_value: String,
  second_key: String,
  second_value: String,
  run: fn() -> a,
) -> a {
  let first_previous = scherzo_path.env(first_key)
  let second_previous = scherzo_path.env(second_key)
  let assert Ok(Nil) = scherzo_path.set_env(first_key, first_value)
  let assert Ok(Nil) = scherzo_path.set_env(second_key, second_value)
  let result = run()
  restore_env(second_key, second_previous)
  restore_env(first_key, first_previous)
  result
}

fn restore_env(key: String, previous: Option(String)) -> Nil {
  case previous {
    Some(value) -> {
      let assert Ok(Nil) = scherzo_path.set_env(key, value)
      Nil
    }
    None -> {
      let assert Ok(Nil) = scherzo_path.unset_env(key)
      Nil
    }
  }
}

pub fn json_schema_accepts_valid_payload_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  assert structured_output_json_schema.run_json_schema_validator(
      validator,
      valid_payload(),
      context(validator),
      [],
    )
    == Ok(structured_output_validator.ValidatorPass)
}

pub fn json_schema_omitted_draft_defaults_to_2020_12_test() {
  let validator =
    schema_validator_with_draft(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
      None,
    )
  assert run_validator(validator)
    == Ok(structured_output_validator.ValidatorPass)
}

pub fn json_schema_rejects_invalid_payload_with_instance_path_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload(
        "test/fixtures/structured_output/review_lane_payload_invalid.json",
      ),
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert error.retryable
  assert error.validator_name == "review_lane_shape"
  assert error.validator_type == "json_schema"
  assert string.contains(error.diagnostic_summary, "instance_path=/findings")
}

pub fn review_lane_draft_schema_rejects_absolute_input_ref_paths_test() {
  let validator =
    schema_validator(
      ".scherzo/workflows/schemas/review-lane-draft.v1.schema.json",
    )
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/structured_output/review_lane_draft_absolute_path.json",
    )
  let absolute_input_ref_contents =
    contents
    |> string.replace(
      each: "<absolute-local-path>/src/example.gleam",
      with: "src/example.gleam",
    )
  let assert Ok(absolute_input_ref_payload) =
    json_value.parse(absolute_input_ref_contents)
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      absolute_input_ref_payload,
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert error.retryable
  assert string.contains(error.diagnostic_summary, "/input_refs/0/path")
}

pub fn review_lane_draft_schema_rejects_env_placeholder_input_ref_paths_test() {
  let validator =
    schema_validator(
      ".scherzo/workflows/schemas/review-lane-draft.v1.schema.json",
    )
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/structured_output/review_lane_draft_absolute_path.json",
    )
  let env_placeholder_contents =
    contents
    |> string.replace(
      each: "/absolute-local-path/artifacts/review/prepare_review/diff.patch",
      with: "$SCHERZO_RUN_ROOT/artifacts/review/prepare_review/diff.patch",
    )
    |> string.replace(
      each: "<absolute-local-path>/src/example.gleam",
      with: "src/example.gleam",
    )
  let assert Ok(env_placeholder_payload) =
    json_value.parse(env_placeholder_contents)
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      env_placeholder_payload,
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert error.retryable
  assert string.contains(error.diagnostic_summary, "/input_refs/0/path")
}

pub fn json_schema_missing_file_is_non_retryable_config_error_test() {
  let validator =
    schema_validator("test/fixtures/structured_output/missing.schema.json")
  let assert Error(error) = run_validator(validator)

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
}

pub fn json_schema_invalid_schema_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/invalid_schema.schema.json",
    )
  let assert Error(error) = run_validator(validator)

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
}

pub fn json_schema_unsupported_draft_is_non_retryable_config_error_test() {
  let validator =
    schema_validator_with_draft(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
      Some("draft-07"),
    )
  let assert Error(error) = run_validator(validator)

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
  assert string.contains(error.message, "unsupported JSON Schema draft")
}

pub fn json_schema_rejects_absolute_or_traversal_paths_test() {
  let assert Ok(absolute_path) =
    scherzo_path.absolute(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let absolute = schema_validator(absolute_path)
  let traversal = schema_validator("../review_lane_draft.schema.json")

  let assert Error(absolute_error) = run_validator(absolute)
  let assert Error(traversal_error) = run_validator(traversal)

  assert absolute_error.code == "structured_output_json_schema_config_error"
  assert traversal_error.code == "structured_output_json_schema_config_error"
}

pub fn json_schema_accepts_repo_local_symlinked_workflow_schema_test() {
  let root = "test/tmp/structured-output-json-schema-workflow-symlink-validator"
  let #(repo, schema_path, _) = setup_symlinked_workflow_schema_fixture(root)
  let assert Ok(helper_path) =
    scherzo_path.absolute("scripts/scherzo-json-schema-validate")
  let validator = schema_validator(schema_path)
  let #(valid_result, missing_required_result) =
    with_env(helper_env, helper_path, fn() {
      #(
        structured_output_json_schema.run_json_schema_validator(
          validator,
          valid_payload(),
          context_with_repo(validator, repo),
          [],
        ),
        structured_output_json_schema.run_json_schema_validator(
          validator,
          json_value.JObject([]),
          context_with_repo(validator, repo),
          [],
        ),
      )
    })

  assert valid_result == Ok(structured_output_validator.ValidatorPass)
  let assert Error(error) = missing_required_result
  assert error.code == "structured_output_json_schema_rejected"
  let _ = simplifile.delete(root)
}

pub fn json_schema_hashes_repo_local_symlinked_workflow_schema_contents_test() {
  let root = "test/tmp/structured-output-json-schema-workflow-symlink-metadata"
  let #(repo, schema_path, schema_contents) =
    setup_symlinked_workflow_schema_fixture(root)
  let validator = schema_validator(schema_path)

  let metadata =
    structured_output_metadata.from_spec(
      workflow_dag.StructuredOutputSpec(
        artifact_name: "review_lane_draft",
        required: True,
        source: structured_output_source.FinalResponseSource,
        format: workflow_dag.StructuredJson,
        schema: workflow_dag.StructuredObjectSchema(["schema_version"]),
        validators: [validator],
        validation_retries: 1,
      ),
      repo,
    )
  let assert [
    structured_output_metadata.JsonSchemaValidationMetadata(
      schema_sha256: schema_sha256,
      ..,
    ),
  ] = metadata.validators
  assert schema_sha256 == hash.sha256_hex(schema_contents)
  let _ = simplifile.delete(root)
}

pub fn json_schema_rejects_non_workflow_schema_symlink_escape_before_helper_test() {
  let root =
    "test/tmp/structured-output-json-schema-non-workflow-schema-symlink"
  let repo = root <> "/repo"
  let outside = root <> "/outside"
  reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/schemas")
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let outside_schema = outside <> "/escaped.schema.json"
  let assert Ok(Nil) =
    simplifile.write(outside_schema, symlinked_workflow_schema_contents())
  let assert Ok(absolute_target) = scherzo_path.absolute(outside_schema)
  let assert Ok(Nil) =
    scherzo_path.symlink(
      absolute_target,
      repo <> "/schemas/escaped.schema.json",
    )

  let validator = schema_validator("schemas/escaped.schema.json")
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      valid_payload(),
      context_with_repo(validator, repo),
      [],
    )

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
  assert string.contains(error.message, "outside the repository")
  let _ = simplifile.delete(root)
}

fn setup_symlinked_workflow_schema_fixture(
  root: String,
) -> #(String, String, String) {
  let repo = root <> "/repo"
  let shared_schemas = root <> "/sibling-scherzo/.scherzo/workflows/schemas"
  let schema_path = symlinked_workflow_schema_path()
  let schema_contents = symlinked_workflow_schema_contents()
  reset_dir(root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(repo <> "/.scherzo/workflows")
  let assert Ok(Nil) = simplifile.create_directory_all(shared_schemas)
  let assert Ok(Nil) =
    simplifile.write(
      shared_schemas <> "/" <> symlinked_workflow_schema_filename(),
      schema_contents,
    )
  let assert Ok(absolute_target) = scherzo_path.absolute(shared_schemas)
  let assert Ok(Nil) =
    scherzo_path.symlink(absolute_target, repo <> "/.scherzo/workflows/schemas")
  #(repo, schema_path, schema_contents)
}

fn symlinked_workflow_schema_path() -> String {
  ".scherzo/workflows/schemas/" <> symlinked_workflow_schema_filename()
}

fn symlinked_workflow_schema_filename() -> String {
  "review-lane-draft.correctness.v1.schema.json"
}

fn symlinked_workflow_schema_contents() -> String {
  "{\"type\":\"object\",\"properties\":{\"schema_version\":{\"type\":\"integer\"}},"
  <> "\"required\":[\"schema_version\"]}\n"
}

pub fn json_schema_redacts_secret_in_diagnostics_test() {
  let dir = "test/tmp/structured-output-json-schema-redaction"
  reset_dir(dir)
  let schema_path = dir <> "/secret.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"token\":{\"type\":\"integer\"}},\"required\":[\"token\"]}\n",
    )
  let validator = schema_validator(schema_path)
  let secret = "super-secret-token"
  let value = json_value.JObject([#("token", json_value.JString(secret))])
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      value,
      context(validator),
      [secret],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert !string.contains(error.message, secret)
  assert !string.contains(error.diagnostic_summary, secret)
  let _ = simplifile.delete(dir)
}

pub fn json_schema_helper_start_failure_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    with_env(helper_env, "test/fixtures/structured_output/does-not-exist", fn() {
      run_validator(validator)
    })
  let assert Error(error) = result

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
}

pub fn json_schema_helper_timeout_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    with_two_envs(
      helper_env,
      "test/fixtures/structured_output/json_schema_helper_timeout.sh",
      helper_timeout_env,
      "25",
      fn() { run_validator(validator) },
    )
  let assert Error(error) = result

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
  assert string.contains(error.message, "timed out")
}

pub fn json_schema_helper_malformed_diagnostic_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    with_env(
      helper_env,
      "test/fixtures/structured_output/json_schema_helper_malformed.py",
      fn() { run_validator(validator) },
    )
  let assert Error(error) = result

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
  assert string.contains(error.message, "malformed diagnostics")
}

pub fn json_schema_helper_import_failure_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    with_env(
      helper_env,
      "test/fixtures/structured_output/json_schema_helper_import_failure.py",
      fn() { run_validator(validator) },
    )
  let assert Error(error) = result

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
  assert string.contains(error.message, "jsonschema import failed")
}

fn review_lane_payload_with_lane(lane_id: String) -> json_value.JsonValue {
  let contents =
    "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-13T00:00:00Z\",\"producer\":{\"name\":\"schema-test\",\"version\":\"1\",\"mode\":\"test\"},\"lane\":{\"id\":\""
    <> lane_id
    <> "\",\"name\":\"Lane\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[{\"artifact_type\":\"diff\",\"path\":\"artifacts/review/prepare_review/diff.patch\"}],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
  let assert Ok(value) = json_value.parse(contents)
  value
}

pub fn review_lane_base_schema_rejects_unknown_lane_id_test() {
  let validator =
    schema_validator(
      ".scherzo/workflows/schemas/review-lane-draft.v1.schema.json",
    )
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      review_lane_payload_with_lane("unknown-lane"),
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert string.contains(error.diagnostic_summary, "/lane/id")
}

pub fn review_lane_overlay_schema_accepts_matching_and_rejects_wrong_lane_test() {
  let correctness =
    schema_validator(
      ".scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json",
    )
  assert structured_output_json_schema.run_json_schema_validator(
      correctness,
      review_lane_payload_with_lane("correctness"),
      context(correctness),
      [],
    )
    == Ok(structured_output_validator.ValidatorPass)
  let assert Error(wrong_lane) =
    structured_output_json_schema.run_json_schema_validator(
      correctness,
      review_lane_payload_with_lane("test-quality"),
      context(correctness),
      [],
    )
  assert string.contains(wrong_lane.diagnostic_summary, "/lane/id")

  let overlays = [
    #(
      ".scherzo/workflows/schemas/review-lane-draft.test-quality.v1.schema.json",
      "test-quality",
    ),
    #(
      ".scherzo/workflows/schemas/review-lane-draft.idioms-maintainability.v1.schema.json",
      "idioms-maintainability",
    ),
    #(
      ".scherzo/workflows/schemas/review-lane-draft.security-performance.v1.schema.json",
      "security-performance",
    ),
  ]
  list.each(overlays, fn(entry) {
    let #(schema_path, lane_id) = entry
    let validator = schema_validator(schema_path)
    assert structured_output_json_schema.run_json_schema_validator(
        validator,
        review_lane_payload_with_lane(lane_id),
        context(validator),
        [],
      )
      == Ok(structured_output_validator.ValidatorPass)
  })
}

fn provider_review_lane_schema_paths() -> List(String) {
  [
    ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    ".scherzo/workflows/schemas/provider/review-lane-draft.test-quality.v1.schema.json",
    ".scherzo/workflows/schemas/provider/review-lane-draft.idioms-maintainability.v1.schema.json",
    ".scherzo/workflows/schemas/provider/review-lane-draft.security-performance.v1.schema.json",
  ]
}

fn provider_review_lane_submission_with_note_kind(
  kind: String,
) -> json_value.JsonValue {
  let contents =
    "{\"draft_findings\":[],\"review_notes\":[{\"id\":\"note-1\",\"kind\":\""
    <> kind
    <> "\",\"category\":\"testing\",\"severity\":\"info\",\"locations\":[],\"summary\":\"No findings.\",\"details\":\"No concrete test findings were identified.\",\"suggested_action\":\"Proceed.\"}],\"evidence_requests\":[],\"self_check\":{\"summary\":\"ok\"}}"
  let assert Ok(value) = json_value.parse(contents)
  value
}

pub fn provider_review_lane_schemas_accept_canonical_review_note_kind_test() {
  list.each(provider_review_lane_schema_paths(), fn(schema_path) {
    let validator = schema_validator(schema_path)
    assert structured_output_json_schema.run_json_schema_validator(
        validator,
        provider_review_lane_submission_with_note_kind("review_note"),
        context(validator),
        [],
      )
      == Ok(structured_output_validator.ValidatorPass)
  })
}

pub fn provider_review_lane_schemas_reject_noncanonical_review_note_kind_test() {
  list.each(provider_review_lane_schema_paths(), fn(schema_path) {
    let validator = schema_validator(schema_path)
    let assert Error(error) =
      structured_output_json_schema.run_json_schema_validator(
        validator,
        provider_review_lane_submission_with_note_kind("review_summary"),
        context(validator),
        [],
      )

    assert error.code == "structured_output_json_schema_rejected"
    assert string.contains(error.diagnostic_summary, "/review_notes/0/kind")
  })
}
