import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/workflow_dag

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

pub fn validate_final_response(
  spec: workflow_dag.StructuredOutputSpec,
  final_response: Option(String),
  truncated: Bool,
  secrets: List(String),
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case truncated {
    True ->
      Error(StructuredOutputTruncated(
        "returned a truncated final response; cannot validate structured JSON",
      ))
    False -> validate_nontruncated_response(spec, final_response, secrets)
  }
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
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  case final_response {
    None -> missing_or_absent(spec)
    Some(response) -> {
      let trimmed = string.trim(response)
      case trimmed == "" {
        True -> missing_or_absent(spec)
        False -> validate_present_response(spec, trimmed, secrets)
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
) -> Result(StructuredOutputValidation, StructuredOutputError) {
  use value <- result.try(parse_present_json(trimmed))
  use value <- result.try(validate_schema(spec.schema, value))
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
