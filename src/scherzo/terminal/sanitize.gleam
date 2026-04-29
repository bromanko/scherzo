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

/// Split untrusted block text into display lines.
///
/// Ordinary newlines become layout boundaries, while every other terminal
/// control character remains escaped inside the returned line text. CRLF is
/// normalized to LF first. A lone carriage return is not treated as layout; it
/// is escaped as a visible control picture.
pub fn block_lines(value: String) -> List(String) {
  block_lines_loop(<<value:utf8>>, [], []) |> list.reverse
}

fn block_lines_loop(
  codepoints: BitArray,
  current_parts: List(String),
  acc: List(String),
) -> List(String) {
  case codepoints {
    <<13, 10, rest:bytes>> -> {
      let line = current_parts |> list.reverse |> string.concat
      block_lines_loop(rest, [], [line, ..acc])
    }
    <<10, rest:bytes>> -> {
      let line = current_parts |> list.reverse |> string.concat
      block_lines_loop(rest, [], [line, ..acc])
    }
    <<codepoint:utf8_codepoint, rest:bytes>> ->
      block_lines_loop(
        rest,
        [escape_codepoint(codepoint), ..current_parts],
        acc,
      )
    _ -> [current_parts |> list.reverse |> string.concat, ..acc]
  }
}

/// Split, sanitize, and truncate block text for bounded display.
///
/// This is equivalent to `block_lines` followed by dropping a final empty line
/// produced solely by a trailing newline, truncating each retained line to
/// `max_chars`, and keeping at most `max_lines` lines. The returned boolean is
/// `True` when more body lines existed after the returned prefix.
pub fn bounded_body_lines(
  value: String,
  max_lines: Int,
  max_chars: Int,
  truncation_note: String,
) -> #(List(String), Bool) {
  case max_lines <= 0 {
    True -> #([], True)
    False ->
      bounded_body_lines_loop(
        <<value:utf8>>,
        max_lines,
        max_chars,
        truncation_note,
        "",
        False,
        [],
        0,
      )
  }
}

fn bounded_body_lines_loop(
  codepoints: BitArray,
  max_lines: Int,
  max_chars: Int,
  truncation_note: String,
  current: String,
  current_truncated: Bool,
  acc: List(String),
  emitted: Int,
) -> #(List(String), Bool) {
  case codepoints {
    <<13, 10, rest:bytes>> ->
      finish_bounded_line(
        rest,
        max_lines,
        max_chars,
        truncation_note,
        current,
        current_truncated,
        acc,
        emitted,
      )
    <<10, rest:bytes>> ->
      finish_bounded_line(
        rest,
        max_lines,
        max_chars,
        truncation_note,
        current,
        current_truncated,
        acc,
        emitted,
      )
    <<codepoint:utf8_codepoint, rest:bytes>> -> {
      let #(current, current_truncated) =
        append_bounded_codepoint(
          current,
          current_truncated,
          max_chars,
          codepoint,
        )
      bounded_body_lines_loop(
        rest,
        max_lines,
        max_chars,
        truncation_note,
        current,
        current_truncated,
        acc,
        emitted,
      )
    }
    _ -> {
      let line = bounded_line(current, current_truncated, truncation_note)
      #([line, ..acc] |> list.reverse, False)
    }
  }
}

fn finish_bounded_line(
  rest: BitArray,
  max_lines: Int,
  max_chars: Int,
  truncation_note: String,
  current: String,
  current_truncated: Bool,
  acc: List(String),
  emitted: Int,
) -> #(List(String), Bool) {
  let line = bounded_line(current, current_truncated, truncation_note)
  let acc = [line, ..acc]
  let emitted = emitted + 1
  case emitted >= max_lines, rest {
    True, <<>> -> #(list.reverse(acc), False)
    True, _ -> #(list.reverse(acc), True)
    False, <<>> -> #(list.reverse(acc), False)
    False, _ ->
      bounded_body_lines_loop(
        rest,
        max_lines,
        max_chars,
        truncation_note,
        "",
        False,
        acc,
        emitted,
      )
  }
}

fn append_bounded_codepoint(
  current: String,
  current_truncated: Bool,
  max_chars: Int,
  codepoint,
) -> #(String, Bool) {
  case current_truncated {
    True -> #(current, True)
    False -> {
      let candidate = current <> escape_codepoint(codepoint)
      case string.length(candidate) <= max_chars {
        True -> #(candidate, False)
        False -> #(
          string.slice(candidate, at_index: 0, length: max_chars),
          True,
        )
      }
    }
  }
}

fn bounded_line(
  current: String,
  current_truncated: Bool,
  truncation_note: String,
) -> String {
  case current_truncated {
    True -> current <> " " <> truncation_note
    False -> current
  }
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
