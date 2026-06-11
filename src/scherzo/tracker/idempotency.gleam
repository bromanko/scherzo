import gleam/string

pub const marker_prefix = "scherzo:outbox:"

pub fn marker(key: String) -> String {
  "<!-- " <> marker_prefix <> key <> " -->"
}

pub fn append_marker(body: String, key: String) -> String {
  let marker = marker(key)
  case string.contains(body, marker) {
    True -> body
    False -> body <> "\n\n" <> marker
  }
}

pub fn contains_marker(body: String, key: String) -> Bool {
  string.contains(body, marker(key))
}

pub fn extract_key(body: String) -> Result(String, Nil) {
  body
  |> string.split(marker_prefix)
  |> extract_after_prefix
}

fn extract_after_prefix(parts: List(String)) -> Result(String, Nil) {
  case parts {
    [_, after, ..] ->
      after
      |> string.split(" -->")
      |> first_non_empty
    _ -> Error(Nil)
  }
}

fn first_non_empty(parts: List(String)) -> Result(String, Nil) {
  case parts {
    [value, ..] -> {
      let value = string.trim(value)
      case value == "" {
        True -> Error(Nil)
        False -> Ok(value)
      }
    }
    [] -> Error(Nil)
  }
}
