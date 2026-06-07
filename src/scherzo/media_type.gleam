import gleam/list
import gleam/string

pub type MediaTypeError {
  MediaTypeError(code: String, message: String)
}

pub fn validate(value: String) -> Result(Nil, MediaTypeError) {
  let trimmed = string.trim(value) |> string.lowercase
  case trimmed == "" || has_control_character(trimmed) {
    True ->
      Error(MediaTypeError(
        "invalid_media_type",
        "media_type must be a non-empty MIME type string",
      ))
    False -> validate_parts(trimmed)
  }
}

fn validate_parts(value: String) -> Result(Nil, MediaTypeError) {
  case string.split(value, ";") {
    [] -> invalid()
    [essence, ..params] ->
      case validate_essence(string.trim(essence)) {
        True -> validate_parameters(params)
        False -> invalid()
      }
  }
}

fn validate_essence(value: String) -> Bool {
  case string.split(value, "/") {
    [type_, subtype] -> valid_token(type_) && valid_token(subtype)
    _ -> False
  }
}

fn validate_parameters(params: List(String)) -> Result(Nil, MediaTypeError) {
  case params {
    [] -> Ok(Nil)
    [param, ..rest] ->
      case valid_parameter(string.trim(param)) {
        True -> validate_parameters(rest)
        False -> invalid()
      }
  }
}

fn valid_parameter(param: String) -> Bool {
  case string.split_once(param, "=") {
    Error(Nil) -> False
    Ok(#(name, value)) ->
      valid_token(string.trim(name))
      && valid_parameter_value(string.trim(value))
  }
}

fn valid_parameter_value(value: String) -> Bool {
  value != "" && !has_control_character(value) && !string.contains(value, ";")
}

fn valid_token(value: String) -> Bool {
  value != "" && list.all(string.to_graphemes(value), is_token_char)
}

fn is_token_char(char: String) -> Bool {
  case char {
    "!"
    | "#"
    | "$"
    | "%"
    | "&"
    | "'"
    | "*"
    | "+"
    | "-"
    | "."
    | "^"
    | "_"
    | "`"
    | "|"
    | "~" -> True
    _ -> is_alpha_numeric(char)
  }
}

fn is_alpha_numeric(char: String) -> Bool {
  case char {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9" -> True
    _ -> False
  }
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
}

fn invalid() -> Result(Nil, MediaTypeError) {
  Error(MediaTypeError(
    "invalid_media_type",
    "media_type must be a syntactically valid MIME type",
  ))
}
