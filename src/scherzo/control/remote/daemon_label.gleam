import gleam/int
import gleam/list
import gleam/string

const max_length = 80

pub type ValidationError {
  Empty
  TooLong(Int)
  ContainsControlCharacter
}

pub fn normalize(value: String) -> Result(String, ValidationError) {
  case contains_control_character(value) {
    True -> Error(ContainsControlCharacter)
    False -> {
      let value = string.trim(value)
      case value == "" {
        True -> Error(Empty)
        False -> {
          let length = value |> string.to_utf_codepoints |> list.length
          case length > max_length {
            True -> Error(TooLong(max_length))
            False -> Ok(value)
          }
        }
      }
    }
  }
}

pub fn error_message(error: ValidationError) -> String {
  case error {
    Empty -> "must be non-empty after trimming whitespace"
    TooLong(limit) ->
      "must be at most " <> int.to_string(limit) <> " characters"
    ContainsControlCharacter ->
      "must not contain newlines or control characters"
  }
}

pub fn shape_description() -> String {
  "trimmed 1-80 character display text; spaces and punctuation are allowed, but whitespace-only names, newlines, and control characters are not"
}

fn contains_control_character(value: String) -> Bool {
  value
  |> string.to_utf_codepoints
  |> list.any(fn(codepoint) {
    let code = string.utf_codepoint_to_int(codepoint)
    code <= 31 || code == 127 || { code >= 128 && code <= 159 }
  })
}
