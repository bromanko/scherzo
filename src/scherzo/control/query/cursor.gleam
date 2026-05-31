import gleam/int
import gleam/string
import scherzo/control/query/types

const cursor_prefix = "cursor:"

pub fn encode_offset(after: Int) -> String {
  cursor_prefix <> int.to_string(after)
}

pub fn decode_offset(token: String) -> Result(Int, types.QueryError) {
  case string.starts_with(token, cursor_prefix) {
    False -> invalid_cursor()
    True -> {
      let value = string.drop_start(token, string.length(cursor_prefix))
      case int.parse(value) {
        Ok(parsed) ->
          case parsed < 0 {
            True -> invalid_cursor()
            False -> Ok(parsed)
          }
        Error(_) -> invalid_cursor()
      }
    }
  }
}

fn invalid_cursor() -> Result(Int, types.QueryError) {
  Error(types.QueryError(types.InvalidCursor, "invalid query cursor"))
}
