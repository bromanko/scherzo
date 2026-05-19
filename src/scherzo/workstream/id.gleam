import gleam/string

pub type IdError {
  EmptyIdentifier
  IdentifierContainsWhitespace
}

pub fn linear_workstream_id(
  issue_identifier: String,
) -> Result(String, IdError) {
  let trimmed = string.trim(issue_identifier)
  case trimmed == "" {
    True -> Error(EmptyIdentifier)
    False ->
      case contains_whitespace(trimmed) {
        True -> Error(IdentifierContainsWhitespace)
        False -> Ok("linear:" <> trimmed)
      }
  }
}

pub fn error_code(error: IdError) -> String {
  case error {
    EmptyIdentifier -> "empty_issue_identifier"
    IdentifierContainsWhitespace -> "issue_identifier_contains_whitespace"
  }
}

pub fn error_message(error: IdError) -> String {
  case error {
    EmptyIdentifier -> "issue identifier must not be empty"
    IdentifierContainsWhitespace ->
      "issue identifier must not contain whitespace"
  }
}

fn contains_whitespace(value: String) -> Bool {
  string.contains(value, " ")
  || string.contains(value, "\n")
  || string.contains(value, "\r")
  || string.contains(value, "\t")
}
