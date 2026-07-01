import gleam/string
import scherzo/hash
import scherzo/path

pub type Cursor {
  Cursor(provider_id: String, last_item_key: String)
}

pub type CursorError {
  CursorError(code: String, message: String)
}

pub fn encode(workspace_root: String, cursor: Cursor) -> String {
  let Cursor(provider_id, last_item_key) = cursor
  "v1|"
  <> root_hash(workspace_root)
  <> "|"
  <> provider_id
  <> "|"
  <> escape(last_item_key)
}

pub fn decode(
  workspace_root: String,
  value: String,
) -> Result(Cursor, CursorError) {
  let trimmed = string.trim(value)
  case trimmed {
    "" -> Error(CursorError("empty_cursor", "cleanup cursor must not be empty"))
    _ ->
      case string.split(trimmed, on: "|") {
        [version, encoded_root, provider_id, encoded_key] ->
          case version {
            "v1" ->
              case encoded_root == root_hash(workspace_root) {
                False ->
                  Error(CursorError(
                    "wrong_root",
                    "cleanup cursor does not match the requested workspace root",
                  ))
                True ->
                  case provider_id == "" {
                    True ->
                      Error(CursorError(
                        "malformed_cursor",
                        "cleanup cursor provider is empty",
                      ))
                    False -> Ok(Cursor(provider_id, unescape(encoded_key)))
                  }
              }
            _ ->
              Error(CursorError(
                "unknown_version",
                "cleanup cursor version is not supported",
              ))
          }
        _ ->
          Error(CursorError("malformed_cursor", "cleanup cursor is malformed"))
      }
  }
}

fn root_hash(workspace_root: String) -> String {
  workspace_root
  |> string.trim
  |> path.absolute_or_original
  |> hash.sha256_hex
}

fn escape(value: String) -> String {
  value
  |> string.replace(each: "%", with: "%25")
  |> string.replace(each: "|", with: "%7C")
  |> string.replace(each: "\n", with: "%0A")
}

fn unescape(value: String) -> String {
  value
  |> string.replace(each: "%0A", with: "\n")
  |> string.replace(each: "%7C", with: "|")
  |> string.replace(each: "%25", with: "%")
}
