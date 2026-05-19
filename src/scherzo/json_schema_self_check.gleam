import gleam/option.{None, Some}
import gleam/result
import scherzo/json_value
import scherzo/path
import scherzo/structured_output_json_schema
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile

pub type SelfCheckError {
  SelfCheckError(code: String, message: String)
}

pub fn run(
  repository_root: String,
  schema_path: String,
  payload_path: String,
) -> Result(Nil, SelfCheckError) {
  use payload_text <- result.try(
    simplifile.read(payload_path)
    |> result.map_error(fn(error) {
      SelfCheckError(
        "json_schema_self_check_payload_read_failed",
        simplifile.describe_error(error),
      )
    }),
  )
  use payload <- result.try(
    json_value.parse(payload_text)
    |> result.replace_error(SelfCheckError(
      "json_schema_self_check_payload_invalid",
      "payload file is not valid JSON",
    )),
  )
  let validator =
    workflow_dag.JsonSchemaValidator(
      name: "json_schema_self_check",
      path: schema_path,
      draft: Some("2020-12"),
    )
  let context =
    structured_output_validator.base_context(
      path.join(repository_root, ".scherzo"),
      repository_root,
      path.join(repository_root, "tmp/scherzo-json-schema-self-check"),
      "json_schema_self_check",
      "",
      "install-check",
      "json_schema_self_check",
      0,
      repository_root,
      "json_schema_self_check",
      "json",
      "final_response",
      None,
    )
    |> structured_output_validator.for_validator(validator, 0)

  case
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload,
      context,
      [],
    )
  {
    Ok(structured_output_validator.ValidatorPass) -> Ok(Nil)
    Error(failure) ->
      Error(SelfCheckError(
        failure.code,
        failure.message <> ": " <> failure.diagnostic_summary,
      ))
  }
}
