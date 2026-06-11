import gleam/option.{None}
import gleam/string
import scherzo/control/query/types as query_types

pub type ParseError {
  ParseError(message: String)
}

pub fn task_query_ref(
  value: String,
) -> Result(query_types.TaskQueryRef, ParseError) {
  let value = string.trim(value)
  case value == "" {
    True -> Error(parse_error("task show requires a non-empty task reference"))
    False ->
      case string.starts_with(value, "id:") {
        True -> {
          let id = string.drop_start(value, 3) |> string.trim
          case id == "" {
            True -> Error(parse_error("task show id must include a remote id"))
            False -> Ok(query_types.TaskRemoteId(provider: None, id: id))
          }
        }
        False -> Ok(query_types.TaskDisplayId(value))
      }
  }
}

pub fn error_message(error: ParseError) -> String {
  let ParseError(message) = error
  message
}

fn parse_error(message: String) -> ParseError {
  ParseError(message)
}
