import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import yay

pub type StructuredOutputSource {
  FinalResponseSource
  PiToolCallSource(
    tool_name: String,
    require_single: Bool,
    reject_sibling_tool_calls: Bool,
    parameters_schema_path: Option(String),
  )
}

pub type SourceError {
  SourceError(code: String, message: String)
}

pub fn default() -> StructuredOutputSource {
  FinalResponseSource
}

pub fn type_to_string(source: StructuredOutputSource) -> String {
  case source {
    FinalResponseSource -> "final_response"
    PiToolCallSource(_, _, _, _) -> "pi_tool_call"
  }
}

pub fn tool_name(source: StructuredOutputSource) -> Option(String) {
  case source {
    FinalResponseSource -> None
    PiToolCallSource(name, _, _, _) -> Some(name)
  }
}

pub fn parameters_schema_path(
  source: StructuredOutputSource,
) -> Option(String) {
  case source {
    FinalResponseSource -> None
    PiToolCallSource(_, _, _, path) -> path
  }
}

pub fn parse(node: yay.Node) -> Result(StructuredOutputSource, SourceError) {
  case get_node(node, "source") {
    None -> Ok(default())
    Some(source_node) ->
      case source_node {
        yay.NodeMap(_) -> parse_source_map(source_node)
        _ ->
          error(
            "structured_output_source_not_map",
            "structured_output.source must be a map",
          )
      }
  }
}

fn parse_source_map(
  node: yay.Node,
) -> Result(StructuredOutputSource, SourceError) {
  use source_type <- result.try(read_source_type(node))
  case source_type {
    "final_response" -> read_final_response_source(node)
    "pi_tool_call" -> read_pi_tool_call_source(node)
    other ->
      error(
        "unsupported_structured_output_source_type",
        "unsupported structured_output.source.type: " <> other,
      )
  }
}

fn read_source_type(node: yay.Node) -> Result(String, SourceError) {
  case get_node(node, "type") {
    None ->
      error(
        "missing_structured_output_source_type",
        "structured_output.source.type is required",
      )
    Some(yay.NodeStr(value)) -> Ok(string.trim(value) |> string.lowercase)
    Some(_) ->
      error(
        "structured_output_source_type_not_string",
        "structured_output.source.type must be a string",
      )
  }
}

fn read_final_response_source(
  node: yay.Node,
) -> Result(StructuredOutputSource, SourceError) {
  case first_present_pi_field(node) {
    None -> Ok(FinalResponseSource)
    Some(field) ->
      error(
        "structured_output_source_conflicting_field",
        "structured_output.source."
          <> field
          <> " is only valid for type pi_tool_call",
      )
  }
}

fn first_present_pi_field(node: yay.Node) -> Option(String) {
  case get_node(node, "tool_name") {
    Some(_) -> Some("tool_name")
    None ->
      case get_node(node, "require_single") {
        Some(_) -> Some("require_single")
        None ->
          case get_node(node, "reject_sibling_tool_calls") {
            Some(_) -> Some("reject_sibling_tool_calls")
            None ->
              case get_node(node, "parameters_schema_path") {
                Some(_) -> Some("parameters_schema_path")
                None -> None
              }
          }
      }
  }
}

fn read_pi_tool_call_source(
  node: yay.Node,
) -> Result(StructuredOutputSource, SourceError) {
  use name <- result.try(read_tool_name(node))
  use require_single <- result.try(read_bool(node, "require_single", True))
  use reject_sibling_tool_calls <- result.try(read_bool(
    node,
    "reject_sibling_tool_calls",
    True,
  ))
  use parameters_schema_path <- result.try(read_optional_schema_path(node))
  case require_single, reject_sibling_tool_calls {
    False, _ ->
      error(
        "unsupported_structured_output_source_require_single",
        "structured_output.source.require_single must be true",
      )
    _, False ->
      error(
        "unsupported_structured_output_source_reject_sibling_tool_calls",
        "structured_output.source.reject_sibling_tool_calls must be true",
      )
    True, True ->
      Ok(PiToolCallSource(
        tool_name: name,
        require_single: require_single,
        reject_sibling_tool_calls: reject_sibling_tool_calls,
        parameters_schema_path: parameters_schema_path,
      ))
  }
}

fn read_optional_schema_path(
  node: yay.Node,
) -> Result(Option(String), SourceError) {
  case get_node(node, "parameters_schema_path") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> {
      let path = string.trim(value)
      case valid_parameters_schema_path(path) {
        True -> Ok(Some(path))
        False ->
          error(
            "structured_output_parameters_schema_path_invalid",
            "structured_output.source.parameters_schema_path must be repository-relative and confined to the repository: "
              <> value,
          )
      }
    }
    Some(_) ->
      error(
        "structured_output_source_parameters_schema_path_not_string",
        "structured_output.source.parameters_schema_path must be a string",
      )
  }
}

pub fn valid_parameters_schema_path(value: String) -> Bool {
  value != ""
  && !string.starts_with(value, "/")
  && !has_parent_segment(value)
  && !starts_with_env_prefix(value)
  && !starts_with_drive_path(value)
  && !string.starts_with(value, "<absolute-local-path>")
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn starts_with_env_prefix(value: String) -> Bool {
  string.starts_with(value, "$")
}

fn starts_with_drive_path(value: String) -> Bool {
  case string.to_graphemes(value) {
    [letter, ":", separator, ..] ->
      is_letter(letter) && { separator == "/" || separator == "\\" }
    _ -> False
  }
}

fn is_letter(ch: String) -> Bool {
  is_lower(ch) || is_upper(ch)
}

fn read_tool_name(node: yay.Node) -> Result(String, SourceError) {
  case get_node(node, "tool_name") {
    None ->
      error(
        "missing_structured_output_source_tool_name",
        "structured_output.source.tool_name is required for pi_tool_call",
      )
    Some(yay.NodeStr(value)) -> {
      let name = string.trim(value)
      case valid_tool_name(name) {
        True -> Ok(name)
        False ->
          error(
            "invalid_structured_output_source_tool_name",
            "invalid structured_output.source.tool_name: " <> value,
          )
      }
    }
    Some(_) ->
      error(
        "structured_output_source_tool_name_not_string",
        "structured_output.source.tool_name must be a string",
      )
  }
}

fn read_bool(
  node: yay.Node,
  field: String,
  default: Bool,
) -> Result(Bool, SourceError) {
  case get_node(node, field) {
    None -> Ok(default)
    Some(yay.NodeBool(value)) -> Ok(value)
    Some(_) ->
      error(
        "structured_output_source_" <> field <> "_not_bool",
        "structured_output.source." <> field <> " must be a boolean",
      )
  }
}

fn valid_tool_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_lower(first) && all(rest, is_tool_name_char)
  }
}

fn is_tool_name_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
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

fn error(code: String, message: String) -> Result(a, SourceError) {
  Error(SourceError(code, message))
}
