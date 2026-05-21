import gleam/int
import gleam/string
import scherzo/tracker/conformance/types

pub fn capture_text(text: String) -> types.TranscriptEvidence {
  let original_chars = string.length(text)
  case original_chars > types.max_external_diagnostics_chars {
    True ->
      types.TranscriptEvidence(
        body: truncated_body(text, original_chars),
        truncated: True,
        original_chars: original_chars,
      )
    False ->
      types.TranscriptEvidence(
        body: text,
        truncated: False,
        original_chars: original_chars,
      )
  }
}

pub fn truncate_diagnostics(text: String) -> String {
  let types.TranscriptEvidence(body: body, ..) = capture_text(text)
  body
}

fn truncated_body(text: String, original_chars: Int) -> String {
  string.slice(text, 0, types.max_external_diagnostics_chars)
  <> "... [truncated "
  <> int.to_string(original_chars - types.max_external_diagnostics_chars)
  <> " chars]"
}
