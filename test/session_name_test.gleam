import gleam/list
import gleam/order.{Gt, Lt}
import gleam/string
import scherzo/session/name

pub fn generated_names_are_deterministic_and_safe_test() {
  let first = name.generate("LIV-43", "LIV-43--576460751551-1")
  let again = name.generate("LIV-43", "LIV-43--576460751551-1")
  let different = name.generate("LIV-43", "LIV-43--576460751551-2")

  assert first == again
  assert first != different
  assert string.starts_with(first, "liv-43-")
  assert list.all(string.to_graphemes(first), is_safe_grapheme)
  assert !string.contains(first, "--")
}

pub fn generated_names_sanitize_unsafe_issue_identifier_test() {
  let display_name = name.generate("  Weird/Issue_#42  ", "seed")

  assert string.starts_with(display_name, "weird-issue-42-")
  assert list.all(string.to_graphemes(display_name), is_safe_grapheme)
  assert !string.contains(display_name, "--")
}

pub fn generated_names_fallback_for_empty_issue_identifier_test() {
  let display_name = name.generate(" !@# ", "seed")

  assert string.starts_with(display_name, "session-")
  assert list.all(string.to_graphemes(display_name), is_safe_grapheme)
}

fn is_safe_grapheme(grapheme: String) -> Bool {
  is_between(grapheme, "a", "z")
  || is_between(grapheme, "0", "9")
  || grapheme == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}
