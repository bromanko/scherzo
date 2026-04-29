import gleam/int
import gleam/list
import gleam/string

/// Escape terminal control characters in untrusted text before rendering.
///
/// The renderer applies this before adding any project-owned ANSI style codes so
/// payload text cannot smuggle escape sequences such as CSI/OSC controls into a
/// terminal. C0 controls and DEL are rendered with Unicode control pictures;
/// C1 controls are rendered as visible Unicode escapes.
pub fn text(value: String) -> String {
  value
  |> string.to_utf_codepoints
  |> list.map(escape_codepoint)
  |> string.concat
}

fn escape_codepoint(codepoint) -> String {
  let code = string.utf_codepoint_to_int(codepoint)
  case code {
    code if code >= 0 && code <= 31 -> control_picture(9216 + code)
    127 -> control_picture(9249)
    code if code >= 128 && code <= 159 -> unicode_escape(code)
    _ -> string.from_utf_codepoints([codepoint])
  }
}

fn control_picture(code: Int) -> String {
  case string.utf_codepoint(code) {
    Ok(codepoint) -> string.from_utf_codepoints([codepoint])
    Error(_) -> unicode_escape(code)
  }
}

fn unicode_escape(code: Int) -> String {
  "\\u{" <> int.to_base16(code) <> "}"
}
