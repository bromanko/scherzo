import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/json_value
import scherzo/structured_output
import scherzo/workflow_dag

fn spec(required: Bool) -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_result",
    required: required,
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema(["summary", "findings"]),
  )
}

fn validate(
  required: Bool,
  response: Option(String),
  truncated: Bool,
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_final_response(
    spec(required),
    response,
    truncated,
    [],
  )
}

pub fn required_structured_output_accepts_valid_json_test() {
  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    validate(True, Some("{\"summary\":\"ok\",\"findings\":[]}"), False)
  let assert Ok(json_value.JObject(entries)) = json_value.parse(payload)
  assert json_value.object_has_key(entries, "summary")
  assert json_value.object_has_key(entries, "findings")
}

pub fn required_structured_output_rejects_missing_invalid_truncated_and_schema_test() {
  let assert Error(invalid_json) = validate(True, Some("not json"), False)
  assert structured_output.error_code(invalid_json)
    == "structured_output_invalid_json"

  let assert Error(missing_none) = validate(True, None, False)
  assert structured_output.error_code(missing_none)
    == "structured_output_missing"

  let assert Error(missing_blank) = validate(True, Some("   "), False)
  assert structured_output.error_code(missing_blank)
    == "structured_output_missing"

  let assert Error(truncated) =
    validate(True, Some("{\"summary\":\"ok\",\"findings\":[]}"), True)
  assert structured_output.error_code(truncated)
    == "structured_output_truncated"

  let assert Error(not_object) = validate(True, Some("[]"), False)
  assert structured_output.error_code(not_object)
    == "structured_output_schema_invalid"

  let assert Error(missing_key) =
    validate(True, Some("{\"summary\":\"ok\"}"), False)
  assert structured_output.error_code(missing_key)
    == "structured_output_schema_invalid"
  assert string.contains(
    structured_output.error_message(missing_key),
    "findings",
  )
}

pub fn optional_structured_output_absence_succeeds_but_invalid_present_fails_test() {
  assert validate(False, None, False)
    == Ok(structured_output.StructuredOutputAbsent)
  assert validate(False, Some("   "), False)
    == Ok(structured_output.StructuredOutputAbsent)

  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    validate(False, Some("{\"summary\":\"ok\",\"findings\":[]}"), False)

  let assert Error(invalid_json) = validate(False, Some("not json"), False)
  assert structured_output.error_code(invalid_json)
    == "structured_output_invalid_json"

  let assert Error(truncated) = validate(False, None, True)
  assert structured_output.error_code(truncated)
    == "structured_output_truncated"
}

pub fn redacts_secret_strings_before_returning_payload_test() {
  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    structured_output.validate_final_response(
      spec(True),
      Some("{\"summary\":\"token-123\",\"findings\":[\"token-123\"]}"),
      False,
      ["token-123"],
    )

  assert !string.contains(payload, "token-123")
  let assert Ok(json_value.JObject(entries)) = json_value.parse(payload)
  assert json_value.object_has_key(entries, "summary")
  assert json_value.object_has_key(entries, "findings")
}
