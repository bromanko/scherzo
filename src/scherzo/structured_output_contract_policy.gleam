import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/structured_output_source

pub type ContractPolicyError {
  ContractPolicyError(code: String, message: String)
}

pub type SourceContract {
  SourceContract(tool_name: String, parameters_schema_path: String)
}

pub fn validate_source(
  source: structured_output_source.StructuredOutputSource,
) -> Result(SourceContract, ContractPolicyError) {
  case source {
    structured_output_source.FinalResponseSource ->
      Error(ContractPolicyError(
        "structured_output_source_not_pi_tool_call",
        "structured_output.source.type must be pi_tool_call",
      ))
    structured_output_source.PiToolCallSource(
      tool_name,
      require_single,
      reject_sibling_tool_calls,
      parameters_schema_path,
    ) -> {
      use path <- result.try(required_schema_path(parameters_schema_path))
      use Nil <- result.try(validate_supported_policy(
        require_single: require_single,
        reject_sibling_tool_calls: reject_sibling_tool_calls,
      ))
      use Nil <- result.try(validate_schema_path(path))
      Ok(SourceContract(tool_name: tool_name, parameters_schema_path: path))
    }
  }
}

fn required_schema_path(
  parameters_schema_path: Option(String),
) -> Result(String, ContractPolicyError) {
  case parameters_schema_path {
    Some(path) -> Ok(path)
    None ->
      Error(ContractPolicyError(
        "structured_output_missing_parameters_schema_path",
        "structured_output.source.parameters_schema_path is required",
      ))
  }
}

pub fn validate_supported_policy(
  require_single require_single: Bool,
  reject_sibling_tool_calls reject_sibling_tool_calls: Bool,
) -> Result(Nil, ContractPolicyError) {
  case require_single, reject_sibling_tool_calls {
    True, True -> Ok(Nil)
    False, _ ->
      Error(ContractPolicyError(
        "structured_output_unsupported_require_single",
        "structured_output.source.require_single must be true",
      ))
    _, False ->
      Error(ContractPolicyError(
        "structured_output_unsupported_reject_sibling_tool_calls",
        "structured_output.source.reject_sibling_tool_calls must be true",
      ))
  }
}

pub fn validate_schema_path(
  schema_path: String,
) -> Result(Nil, ContractPolicyError) {
  case structured_output_source.valid_parameters_schema_path(schema_path) {
    True -> Ok(Nil)
    False ->
      Error(ContractPolicyError(
        "structured_output_parameters_schema_path_invalid",
        "parameters_schema_path must be repository-relative and confined to the repository: "
          <> schema_path,
      ))
  }
}

pub fn validate_provider_schema(
  schema: json_value.JsonValue,
  schema_path: String,
) -> Result(Nil, ContractPolicyError) {
  case schema {
    json_value.JObject(entries) -> {
      use Nil <- result.try(validate_schema_entries(entries, schema_path, ""))
      case object_field(entries, "type") {
        Some(json_value.JString("object")) -> Ok(Nil)
        _ ->
          Error(ContractPolicyError(
            "structured_output_provider_schema_not_object",
            "provider schema "
              <> schema_path
              <> " must have top-level type \"object\"",
          ))
      }
    }
    _ ->
      Error(ContractPolicyError(
        "structured_output_provider_schema_not_object",
        "provider schema must be a JSON object: " <> schema_path,
      ))
  }
}

fn validate_schema_entries(
  entries: List(#(String, json_value.JsonValue)),
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, value), ..rest] -> {
      use Nil <- result.try(validate_schema_keyword(
        key,
        value,
        schema_path,
        location,
      ))
      use Nil <- result.try(validate_schema_keyword_value(
        key,
        value,
        schema_path,
        append_pointer(location, key),
      ))
      validate_schema_entries(rest, schema_path, location)
    }
  }
}

fn validate_schema_keyword(
  key: String,
  value: json_value.JsonValue,
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case list.contains(allowed_provider_schema_keywords(), key) {
    True ->
      case key == "type" {
        True -> validate_schema_type_keyword(value, schema_path, location)
        False -> Ok(Nil)
      }
    False -> provider_schema_keyword_error(schema_path, location, key)
  }
}

fn validate_schema_type_keyword(
  value: json_value.JsonValue,
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case value {
    json_value.JArray(_) ->
      provider_schema_keyword_error(schema_path, location, "type")
    _ -> Ok(Nil)
  }
}

fn validate_schema_keyword_value(
  key: String,
  value: json_value.JsonValue,
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case key {
    "properties" -> validate_schema_properties(value, schema_path, location)
    "items" -> validate_schema_value(value, schema_path, location)
    "additionalProperties" ->
      validate_schema_value(value, schema_path, location)
    _ -> Ok(Nil)
  }
}

fn validate_schema_properties(
  value: json_value.JsonValue,
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case value {
    json_value.JObject(entries) ->
      validate_schema_property_entries(entries, schema_path, location)
    _ -> Ok(Nil)
  }
}

fn validate_schema_property_entries(
  entries: List(#(String, json_value.JsonValue)),
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case entries {
    [] -> Ok(Nil)
    [#(property_name, value), ..rest] -> {
      use Nil <- result.try(validate_schema_value(
        value,
        schema_path,
        append_pointer(location, property_name),
      ))
      validate_schema_property_entries(rest, schema_path, location)
    }
  }
}

fn validate_schema_array(
  values: List(json_value.JsonValue),
  schema_path: String,
  location: String,
  index: Int,
) -> Result(Nil, ContractPolicyError) {
  case values {
    [] -> Ok(Nil)
    [value, ..rest] -> {
      use Nil <- result.try(validate_schema_value(
        value,
        schema_path,
        location <> "/" <> int.to_string(index),
      ))
      validate_schema_array(rest, schema_path, location, index + 1)
    }
  }
}

fn validate_schema_value(
  value: json_value.JsonValue,
  schema_path: String,
  location: String,
) -> Result(Nil, ContractPolicyError) {
  case value {
    json_value.JObject(entries) ->
      validate_schema_entries(entries, schema_path, location)
    json_value.JArray(values) ->
      validate_schema_array(values, schema_path, location, 0)
    _ -> Ok(Nil)
  }
}

fn allowed_provider_schema_keywords() -> List(String) {
  [
    "type",
    "description",
    "properties",
    "required",
    "additionalProperties",
    "items",
    "minLength",
    "maxLength",
    "minimum",
    "maximum",
    "minItems",
    "maxItems",
    "pattern",
  ]
}

fn provider_schema_keyword_error(
  schema_path: String,
  location: String,
  keyword: String,
) -> Result(Nil, ContractPolicyError) {
  Error(ContractPolicyError(
    "structured_output_provider_incompatible_schema",
    "provider schema "
      <> schema_path
      <> " contains disallowed keyword "
      <> keyword
      <> " at "
      <> append_pointer(location, keyword),
  ))
}

fn append_pointer(location: String, key: String) -> String {
  case location == "" {
    True -> "/" <> escape_json_pointer(key)
    False -> location <> "/" <> escape_json_pointer(key)
  }
}

fn escape_json_pointer(value: String) -> String {
  value
  |> string.replace(each: "~", with: "~0")
  |> string.replace(each: "/", with: "~1")
}

fn object_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> object_field(rest, key)
      }
  }
}
