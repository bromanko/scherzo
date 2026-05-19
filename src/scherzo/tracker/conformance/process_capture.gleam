import gleam/int
import gleam/string
import scherzo/tracker/conformance/types

pub fn truncate_diagnostics(text: String) -> String {
  case string.length(text) > types.max_external_diagnostics_chars {
    True ->
      string.slice(text, 0, types.max_external_diagnostics_chars)
      <> "... [truncated "
      <> int.to_string(
        string.length(text) - types.max_external_diagnostics_chars,
      )
      <> " chars]"
    False -> text
  }
}
