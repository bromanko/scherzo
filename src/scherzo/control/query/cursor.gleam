import gleam/int
import gleam/string
import scherzo/control/query/types

const cursor_prefix = "cursor:"

const work_item_cursor_prefix = "work-item:"

pub fn encode_offset(after: Int) -> String {
  cursor_prefix <> int.to_string(after)
}

pub fn decode_offset(token: String) -> Result(Int, types.QueryError) {
  decode_prefixed_offset(token, cursor_prefix)
}

pub fn encode_work_item_offset(after: Int, fingerprint: String) -> String {
  work_item_cursor_prefix <> int.to_string(after) <> ":" <> fingerprint
}

pub fn decode_work_item_offset(
  token: String,
  fingerprint: String,
) -> Result(Int, types.QueryError) {
  case string.starts_with(token, work_item_cursor_prefix) {
    False -> invalid_cursor()
    True -> {
      let value =
        string.drop_start(token, string.length(work_item_cursor_prefix))
      case string.split_once(value, on: ":") {
        Ok(#(offset, supplied_fingerprint)) ->
          case supplied_fingerprint == fingerprint {
            True -> parse_offset(offset)
            False -> invalid_cursor()
          }
        Error(Nil) -> invalid_cursor()
      }
    }
  }
}

fn decode_prefixed_offset(
  token: String,
  prefix: String,
) -> Result(Int, types.QueryError) {
  case string.starts_with(token, prefix) {
    False -> invalid_cursor()
    True -> {
      let value = string.drop_start(token, string.length(prefix))
      parse_offset(value)
    }
  }
}

fn parse_offset(value: String) -> Result(Int, types.QueryError) {
  case int.parse(value) {
    Ok(parsed) ->
      case parsed < 0 {
        True -> invalid_cursor()
        False -> Ok(parsed)
      }
    Error(Nil) -> invalid_cursor()
  }
}

fn invalid_cursor() -> Result(Int, types.QueryError) {
  Error(types.QueryError(types.InvalidCursor, "invalid query cursor"))
}
