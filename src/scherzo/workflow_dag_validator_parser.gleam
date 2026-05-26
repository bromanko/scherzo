import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/duration
import yay

pub type ParsedValidator {
  ParsedJsonSchemaValidator(name: String, path: String, draft: Option(String))
  ParsedCommandValidator(
    name: String,
    argv: List(String),
    timeout_ms: Int,
    working_directory: ParsedValidatorWorkingDirectory,
    env: List(#(String, String)),
  )
}

pub type ParsedValidatorWorkingDirectory {
  ParsedValidatorInWorkspace
  ParsedValidatorInRepository
  ParsedValidatorInRunRoot
}

pub type ValidatorParseError {
  ValidatorParseError(code: String, message: String)
}

pub fn parse(
  node: yay.Node,
) -> Result(List(ParsedValidator), ValidatorParseError) {
  case get_node(node, "validator"), get_node(node, "validators") {
    Some(_), Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_ambiguous",
        "structured_output cannot contain both validator and validators",
      ))
    Some(legacy), None -> read_legacy_structured_validator(legacy)
    None, Some(yay.NodeSeq(values)) -> {
      use validators <- result.try(
        read_structured_validator_list(values, 1, []),
      )
      validate_unique_validator_names(validators)
    }
    None, Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validators_not_list",
        "structured_output.validators must be a list",
      ))
    None, None -> Ok([])
  }
}

pub fn parsed_validator_name(validator: ParsedValidator) -> String {
  case validator {
    ParsedJsonSchemaValidator(name: name, ..) -> name
    ParsedCommandValidator(name: name, ..) -> name
  }
}

pub fn parse_command_step_timeout(
  node: yay.Node,
) -> Result(Option(Int), ValidatorParseError) {
  case get_node(node, "timeout") {
    Some(yay.NodeStr(value)) ->
      duration.parse_positive_ms(value, "timeout")
      |> result.map(Some)
      |> result.map_error(fn(duration_error) {
        ValidatorParseError(
          "invalid_command_timeout",
          duration.error_message(duration_error),
        )
      })
    Some(_) ->
      Error(ValidatorParseError(
        "command_timeout_not_duration",
        "timeout must be a duration string with unit ms, s, m, or h",
      ))
    None -> parse_legacy_command_step_timeout(node)
  }
}

fn parse_legacy_command_step_timeout(
  node: yay.Node,
) -> Result(Option(Int), ValidatorParseError) {
  case get_node(node, "timeout_ms") {
    None -> Ok(None)
    Some(yay.NodeInt(value)) ->
      case value > 0 {
        True -> Ok(Some(value))
        False ->
          Error(ValidatorParseError(
            "invalid_command_timeout_ms",
            "timeout_ms must be positive",
          ))
      }
    Some(_) ->
      Error(ValidatorParseError(
        "command_timeout_ms_not_int",
        "timeout_ms must be an integer",
      ))
  }
}

fn read_legacy_structured_validator(
  node: yay.Node,
) -> Result(List(ParsedValidator), ValidatorParseError) {
  case node {
    yay.NodeStr(value) ->
      case string.trim(value) |> string.lowercase {
        "review_lane_draft" -> Ok([legacy_review_lane_draft_validator()])
        _ ->
          Error(ValidatorParseError(
            "unknown_structured_output_validator",
            "unknown structured_output.validator: " <> value,
          ))
      }
    _ ->
      Error(ValidatorParseError(
        "structured_output_validator_not_string",
        "structured_output.validator must be a string",
      ))
  }
}

fn legacy_review_lane_draft_validator() -> ParsedValidator {
  ParsedCommandValidator(
    name: "review_lane_draft_compat",
    argv: [
      "python3",
      ".scherzo/workflows/scripts/scherzo-review",
      "validate-structured-output",
      "--validator",
      "review_lane_draft",
    ],
    timeout_ms: 30_000,
    working_directory: ParsedValidatorInRepository,
    env: [],
  )
}

fn read_structured_validator_list(
  values: List(yay.Node),
  index: Int,
  acc: List(ParsedValidator),
) -> Result(List(ParsedValidator), ValidatorParseError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] -> {
      use validator <- result.try(read_structured_validator_entry(value, index))
      read_structured_validator_list(rest, index + 1, [validator, ..acc])
    }
  }
}

fn read_structured_validator_entry(
  node: yay.Node,
  index: Int,
) -> Result(ParsedValidator, ValidatorParseError) {
  case node {
    yay.NodeMap(_) -> {
      use validator_type <- result.try(read_validator_type(node))
      let default_name = "validator_" <> int.to_string(index)
      use name <- result.try(read_validator_name(node, default_name))
      case validator_type {
        "json_schema" -> read_json_schema_validator(node, name)
        "command" -> read_command_validator(node, name)
        other ->
          Error(ValidatorParseError(
            "unknown_structured_output_validator_type",
            "unknown structured_output.validators.type: " <> other,
          ))
      }
    }
    _ ->
      Error(ValidatorParseError(
        "structured_output_validator_not_map",
        "structured_output.validators entries must be maps",
      ))
  }
}

fn read_validator_type(node: yay.Node) -> Result(String, ValidatorParseError) {
  case get_node(node, "type") {
    None ->
      Error(ValidatorParseError(
        "missing_structured_output_validator_type",
        "structured_output.validators.type is required",
      ))
    Some(yay.NodeStr(value)) -> Ok(string.trim(value) |> string.lowercase)
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_type_not_string",
        "structured_output.validators.type must be a string",
      ))
  }
}

fn read_validator_name(
  node: yay.Node,
  default_name: String,
) -> Result(String, ValidatorParseError) {
  let name = case get_node(node, "name") {
    None -> Ok(default_name)
    Some(yay.NodeStr(value)) -> Ok(value)
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_name_not_string",
        "structured_output.validators.name must be a string",
      ))
  }
  use name <- result.try(name)
  let trimmed = string.trim(name)
  case valid_validator_name(trimmed) {
    True -> Ok(trimmed)
    False ->
      Error(ValidatorParseError(
        "invalid_structured_output_validator_name",
        "invalid structured_output.validators.name: " <> name,
      ))
  }
}

fn read_json_schema_validator(
  node: yay.Node,
  name: String,
) -> Result(ParsedValidator, ValidatorParseError) {
  use path <- result.try(read_repository_relative_path(
    node,
    "path",
    "structured_output.validators.path",
  ))
  use draft <- result.try(read_optional_string_field(
    node,
    "draft",
    "structured_output.validators.draft",
  ))
  Ok(ParsedJsonSchemaValidator(name: name, path: path, draft: draft))
}

fn read_command_validator(
  node: yay.Node,
  name: String,
) -> Result(ParsedValidator, ValidatorParseError) {
  use argv <- result.try(read_command_argv(node))
  use timeout_ms <- result.try(read_command_timeout_ms(node))
  use working_directory <- result.try(read_command_working_directory(node))
  use env <- result.try(read_command_env(node))
  Ok(ParsedCommandValidator(
    name: name,
    argv: argv,
    timeout_ms: timeout_ms,
    working_directory: working_directory,
    env: env,
  ))
}

fn read_command_argv(
  node: yay.Node,
) -> Result(List(String), ValidatorParseError) {
  case get_node(node, "argv") {
    None ->
      Error(ValidatorParseError(
        "missing_structured_output_validator_argv",
        "structured_output.validators.argv is required for command validators",
      ))
    Some(yay.NodeSeq(values)) -> {
      use argv <- result.try(read_command_argv_entries(values, []))
      case argv {
        [] ->
          Error(ValidatorParseError(
            "structured_output_validator_argv_empty",
            "structured_output.validators.argv must not be empty",
          ))
        [executable, ..] ->
          case string.trim(executable) == "" {
            True ->
              Error(ValidatorParseError(
                "structured_output_validator_executable_empty",
                "structured_output.validators.argv[0] must not be empty",
              ))
            False ->
              case validate_executable_path(executable) {
                Ok(Nil) -> Ok(argv)
                Error(error) -> Error(error)
              }
          }
      }
    }
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_argv_not_list",
        "structured_output.validators.argv must be a list",
      ))
  }
}

fn read_command_argv_entries(
  values: List(yay.Node),
  acc: List(String),
) -> Result(List(String), ValidatorParseError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] ->
      read_command_argv_entries(rest, [value, ..acc])
    [_, ..] ->
      Error(ValidatorParseError(
        "structured_output_validator_argv_entry_not_string",
        "structured_output.validators.argv entries must be strings",
      ))
  }
}

fn validate_executable_path(
  executable: String,
) -> Result(Nil, ValidatorParseError) {
  case string.contains(executable, "/") {
    False -> Ok(Nil)
    True ->
      case valid_repository_relative_path(executable) {
        True -> Ok(Nil)
        False ->
          Error(ValidatorParseError(
            "invalid_structured_output_validator_argv_path",
            "structured_output.validators.argv[0] must be repository-relative when it contains a path separator",
          ))
      }
  }
}

fn read_command_timeout_ms(node: yay.Node) -> Result(Int, ValidatorParseError) {
  case get_node(node, "timeout") {
    Some(yay.NodeStr(value)) ->
      duration.parse_positive_ms(value, "structured_output.validators.timeout")
      |> result.map_error(fn(duration_error) {
        ValidatorParseError(
          "invalid_structured_output_validator_timeout",
          duration.error_message(duration_error),
        )
      })
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_timeout_not_duration",
        "structured_output.validators.timeout must be a duration string with unit ms, s, m, or h",
      ))
    None -> read_legacy_command_timeout_ms(node)
  }
}

fn read_legacy_command_timeout_ms(
  node: yay.Node,
) -> Result(Int, ValidatorParseError) {
  case get_node(node, "timeout_ms") {
    None -> Ok(30_000)
    Some(yay.NodeInt(timeout_ms)) ->
      case timeout_ms > 0 {
        True -> Ok(timeout_ms)
        False ->
          Error(ValidatorParseError(
            "invalid_structured_output_validator_timeout_ms",
            "structured_output.validators.timeout_ms must be positive",
          ))
      }
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_timeout_ms_not_int",
        "structured_output.validators.timeout_ms must be an integer",
      ))
  }
}

fn read_command_working_directory(
  node: yay.Node,
) -> Result(ParsedValidatorWorkingDirectory, ValidatorParseError) {
  case get_node(node, "working_directory") {
    None -> Ok(ParsedValidatorInWorkspace)
    Some(yay.NodeStr(value)) ->
      case string.trim(value) |> string.lowercase {
        "workspace" -> Ok(ParsedValidatorInWorkspace)
        "repository" -> Ok(ParsedValidatorInRepository)
        "run_root" -> Ok(ParsedValidatorInRunRoot)
        other ->
          Error(ValidatorParseError(
            "invalid_structured_output_validator_working_directory",
            "unknown structured_output.validators.working_directory: " <> other,
          ))
      }
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_working_directory_not_string",
        "structured_output.validators.working_directory must be a string",
      ))
  }
}

fn read_command_env(
  node: yay.Node,
) -> Result(List(#(String, String)), ValidatorParseError) {
  case get_node(node, "env") {
    None -> Ok([])
    Some(yay.NodeMap(pairs)) -> read_command_env_entries(pairs, [])
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_env_not_map",
        "structured_output.validators.env must be a map",
      ))
  }
}

fn read_command_env_entries(
  pairs: List(#(yay.Node, yay.Node)),
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), ValidatorParseError) {
  case pairs {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(key), yay.NodeStr(value)), ..rest] -> {
      use key <- result.try(validate_command_env_key(key))
      read_command_env_entries(rest, [#(key, value), ..acc])
    }
    [#(_, yay.NodeStr(_)), ..] ->
      Error(ValidatorParseError(
        "structured_output_validator_env_key_not_string",
        "structured_output.validators.env keys must be strings",
      ))
    [#(yay.NodeStr(_), _), ..] ->
      Error(ValidatorParseError(
        "structured_output_validator_env_value_not_string",
        "structured_output.validators.env values must be strings",
      ))
    [_, ..] ->
      Error(ValidatorParseError(
        "structured_output_validator_env_entry_invalid",
        "structured_output.validators.env entries must be string pairs",
      ))
  }
}

fn validate_command_env_key(
  key: String,
) -> Result(String, ValidatorParseError) {
  case valid_env_key(key) && !reserved_env_key(key) {
    True -> Ok(key)
    False ->
      Error(ValidatorParseError(
        "invalid_structured_output_validator_env_key",
        "invalid structured_output.validators.env key: " <> key,
      ))
  }
}

fn read_repository_relative_path(
  node: yay.Node,
  key: String,
  field: String,
) -> Result(String, ValidatorParseError) {
  case get_node(node, key) {
    None ->
      Error(ValidatorParseError(
        "missing_structured_output_validator_path",
        field <> " is required",
      ))
    Some(yay.NodeStr(value)) -> {
      let trimmed = string.trim(value)
      case valid_repository_relative_path(trimmed) {
        True -> Ok(trimmed)
        False ->
          Error(ValidatorParseError(
            "invalid_structured_output_validator_path",
            "invalid " <> field <> ": " <> value,
          ))
      }
    }
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_path_not_string",
        field <> " must be a string",
      ))
  }
}

fn read_optional_string_field(
  node: yay.Node,
  key: String,
  field: String,
) -> Result(Option(String), ValidatorParseError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) ->
      Error(ValidatorParseError(
        "structured_output_validator_" <> key <> "_not_string",
        field <> " must be a string",
      ))
  }
}

fn validate_unique_validator_names(
  validators: List(ParsedValidator),
) -> Result(List(ParsedValidator), ValidatorParseError) {
  use Nil <- result.try(validate_unique_validator_names_loop(validators, []))
  Ok(validators)
}

fn validate_unique_validator_names_loop(
  validators: List(ParsedValidator),
  seen: List(String),
) -> Result(Nil, ValidatorParseError) {
  case validators {
    [] -> Ok(Nil)
    [validator, ..rest] -> {
      let name = parsed_validator_name(validator)
      case list.contains(seen, name) {
        True ->
          Error(ValidatorParseError(
            "duplicate_structured_output_validator_name",
            "duplicate structured_output validator name: " <> name,
          ))
        False -> validate_unique_validator_names_loop(rest, [name, ..seen])
      }
    }
  }
}

fn valid_validator_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_lower(first) && all(rest, is_validator_char)
  }
}

fn valid_env_key(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_env_first_char(first) && all(rest, is_env_rest_char)
  }
}

fn reserved_env_key(value: String) -> Bool {
  value == "PATH"
  || value == "HOME"
  || value == "PWD"
  || string.starts_with(value, "SCHERZO_")
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

fn is_validator_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
}

fn is_env_first_char(ch: String) -> Bool {
  is_lower(ch) || is_upper(ch) || ch == "_"
}

fn is_env_rest_char(ch: String) -> Bool {
  is_env_first_char(ch) || is_digit(ch)
}

fn is_lower_or_digit(ch: String) -> Bool {
  is_lower(ch) || is_digit(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "z") != Gt
}

fn is_upper(ch: String) -> Bool {
  string.compare(ch, "A") != Lt && string.compare(ch, "Z") != Gt
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != Lt && string.compare(ch, "9") != Gt
}

fn all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && all(rest, predicate)
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}
