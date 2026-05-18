import gleam/list
import gleam/string
import simplifile

pub fn read_file(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> {
      let message = path <> " could not be read"
      panic as message
    }
  }
}

pub fn assert_contains(
  path: String,
  contents: String,
  expected: String,
) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = path <> " is missing expected text: " <> expected
      panic as message
    }
  }
}

pub fn assert_contains_all(
  path: String,
  contents: String,
  expected: List(String),
) -> Nil {
  list.each(expected, fn(text) { assert_contains(path, contents, text) })
}

pub fn assert_not_contains(
  path: String,
  contents: String,
  unexpected: String,
) -> Nil {
  case string.contains(contents, unexpected) {
    False -> Nil
    True -> {
      let message = path <> " still contains unexpected text: " <> unexpected
      panic as message
    }
  }
}
