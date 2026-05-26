import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/string

pub fn to_string(error: json.DecodeError) -> String {
  case error {
    json.UnexpectedEndOfInput -> "unexpected_end_of_input"
    json.UnexpectedByte(byte) -> "unexpected_byte:" <> byte
    json.UnexpectedSequence(sequence) -> "unexpected_sequence:" <> sequence
    json.UnableToDecode(errors) ->
      "unable_to_decode:" <> dynamic_decode_errors_to_string(errors)
  }
}

fn dynamic_decode_errors_to_string(errors: List(decode.DecodeError)) -> String {
  case errors {
    [] -> "unknown_decode_error"
    errors ->
      errors
      |> list.map(dynamic_decode_error_to_string)
      |> string.join(with: ";")
  }
}

fn dynamic_decode_error_to_string(error: decode.DecodeError) -> String {
  let decode.DecodeError(expected, found, path) = error
  "path="
  <> decode_path_to_string(path)
  <> " expected="
  <> expected
  <> " found="
  <> found
}

fn decode_path_to_string(path: List(String)) -> String {
  case path {
    [] -> "<root>"
    path -> string.join(path, with: ".")
  }
}
