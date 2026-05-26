import gleam/int
import gleam/result
import gleam/string

pub type ParseError {
  Empty(field: String)
  MissingUnit(field: String)
  InvalidInteger(field: String)
  NotPositive(field: String)
  Negative(field: String)
}

pub fn error_message(error: ParseError) -> String {
  case error {
    Empty(field) -> field <> " must be non-empty"
    MissingUnit(field) -> field <> " must use unit ms, s, m, or h"
    InvalidInteger(field) -> field <> " must start with an integer"
    NotPositive(field) -> field <> " must be positive"
    Negative(field) -> field <> " must be zero or positive"
  }
}

pub fn parse_non_negative_ms(
  value: String,
  field: String,
) -> Result(Int, ParseError) {
  use milliseconds <- result.try(parse_ms(value, field))
  case milliseconds < 0 {
    True -> Error(Negative(field))
    False -> Ok(milliseconds)
  }
}

pub fn parse_positive_ms(
  value: String,
  field: String,
) -> Result(Int, ParseError) {
  use milliseconds <- result.try(parse_ms(value, field))
  case milliseconds <= 0 {
    True -> Error(NotPositive(field))
    False -> Ok(milliseconds)
  }
}

fn parse_ms(value: String, field: String) -> Result(Int, ParseError) {
  let value = string.trim(value)
  case value == "" {
    True -> Error(Empty(field))
    False -> parse_non_empty_ms(value, field)
  }
}

fn parse_non_empty_ms(value: String, field: String) -> Result(Int, ParseError) {
  let #(number_text, multiplier, unit_ok) = case string.ends_with(value, "ms") {
    True -> #(string.drop_end(value, 2), 1, True)
    False ->
      case string.ends_with(value, "s") {
        True -> #(string.drop_end(value, 1), 1000, True)
        False ->
          case string.ends_with(value, "m") {
            True -> #(string.drop_end(value, 1), 60_000, True)
            False ->
              case string.ends_with(value, "h") {
                True -> #(string.drop_end(value, 1), 3_600_000, True)
                False -> #(value, 1, False)
              }
          }
      }
  }
  case unit_ok {
    False -> Error(MissingUnit(field))
    True ->
      number_text
      |> string.trim
      |> int.parse
      |> result.map(fn(number) { number * multiplier })
      |> result.replace_error(InvalidInteger(field))
  }
}
