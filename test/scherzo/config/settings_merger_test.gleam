import gleam/string
import gleeunit/should
import scherzo/config/settings_merger

// -----------------------------------------------------------------------------
// Basic merge tests
// -----------------------------------------------------------------------------

pub fn merge_empty_objects_test() {
  let result = settings_merger.merge("{}", "{}")
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> should.equal("{}")
}

pub fn merge_custom_overrides_non_hook_fields_test() {
  let base = "{\"model\": \"sonnet\", \"timeout\": 30}"
  let custom = "{\"model\": \"opus\"}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Custom overrides base for "model"
  merged |> string.contains("\"model\":\"opus\"") |> should.be_true
  // Base "timeout" is preserved
  merged |> string.contains("\"timeout\":30") |> should.be_true
}

pub fn merge_preserves_base_fields_not_in_custom_test() {
  let base = "{\"a\": 1, \"b\": 2, \"c\": 3}"
  let custom = "{\"b\": 99}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("\"a\":1") |> should.be_true
  merged |> string.contains("\"b\":99") |> should.be_true
  merged |> string.contains("\"c\":3") |> should.be_true
}

// -----------------------------------------------------------------------------
// Hook merging tests
// -----------------------------------------------------------------------------

pub fn merge_hooks_appends_custom_to_same_event_test() {
  let base =
    "{\"hooks\": {\"SessionStart\": [{\"matcher\": \"\", \"hooks\": [{\"type\": \"command\", \"command\": \"base\"}]}]}}"
  let custom =
    "{\"hooks\": {\"SessionStart\": [{\"matcher\": \"\", \"hooks\": [{\"type\": \"command\", \"command\": \"custom\"}]}]}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Both base and custom hooks should be present
  merged |> string.contains("\"command\":\"base\"") |> should.be_true
  merged |> string.contains("\"command\":\"custom\"") |> should.be_true
}

pub fn merge_hooks_preserves_base_events_not_in_custom_test() {
  let base =
    "{\"hooks\": {\"SessionStart\": [{\"id\": 1}], \"Stop\": [{\"id\": 2}]}}"
  let custom = "{\"hooks\": {\"Stop\": [{\"id\": 3}]}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // SessionStart from base preserved
  merged |> string.contains("SessionStart") |> should.be_true
  merged |> string.contains("\"id\":1") |> should.be_true
  // Stop has both base and custom
  merged |> string.contains("\"id\":2") |> should.be_true
  merged |> string.contains("\"id\":3") |> should.be_true
}

pub fn merge_hooks_adds_new_events_from_custom_test() {
  let base = "{\"hooks\": {\"SessionStart\": [{\"id\": 1}]}}"
  let custom = "{\"hooks\": {\"PostToolUse\": [{\"id\": 2}]}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Both events present
  merged |> string.contains("SessionStart") |> should.be_true
  merged |> string.contains("PostToolUse") |> should.be_true
  merged |> string.contains("\"id\":1") |> should.be_true
  merged |> string.contains("\"id\":2") |> should.be_true
}

pub fn merge_hooks_base_comes_first_test() {
  // This tests that base hooks appear before custom hooks in the merged array
  let base = "{\"hooks\": {\"SessionStart\": [{\"order\": \"first\"}]}}"
  let custom = "{\"hooks\": {\"SessionStart\": [{\"order\": \"second\"}]}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Verify both are present and base comes before custom in the array
  let assert Ok(first_idx) = find_substring_index(merged, "\"first\"")
  let assert Ok(second_idx) = find_substring_index(merged, "\"second\"")
  { first_idx < second_idx } |> should.be_true
}

fn find_substring_index(haystack: String, needle: String) -> Result(Int, Nil) {
  case string.split_once(haystack, needle) {
    Ok(#(before, _)) -> Ok(string.length(before))
    Error(_) -> Error(Nil)
  }
}

// -----------------------------------------------------------------------------
// Edge case tests
// -----------------------------------------------------------------------------

pub fn merge_base_no_hooks_custom_has_hooks_test() {
  let base = "{\"model\": \"sonnet\"}"
  let custom = "{\"hooks\": {\"SessionStart\": [{\"id\": 1}]}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("\"model\":\"sonnet\"") |> should.be_true
  merged |> string.contains("SessionStart") |> should.be_true
}

pub fn merge_base_has_hooks_custom_no_hooks_test() {
  let base = "{\"hooks\": {\"SessionStart\": [{\"id\": 1}]}}"
  let custom = "{\"model\": \"opus\"}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("SessionStart") |> should.be_true
  merged |> string.contains("\"model\":\"opus\"") |> should.be_true
}

pub fn merge_handles_nested_objects_test() {
  let base = "{\"nested\": {\"a\": 1}}"
  let custom = "{\"nested\": {\"b\": 2}}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Custom nested object overrides base (not deep merged)
  merged |> string.contains("\"b\":2") |> should.be_true
}

pub fn merge_handles_arrays_test() {
  let base = "{\"items\": [1, 2, 3]}"
  let custom = "{\"items\": [4, 5]}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  // Custom array overrides base (not concatenated - only hooks are special)
  merged |> string.contains("[4,5]") |> should.be_true
}

pub fn merge_handles_null_values_test() {
  let base = "{\"a\": null}"
  let custom = "{\"b\": null}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("null") |> should.be_true
}

pub fn merge_handles_boolean_values_test() {
  let base = "{\"enabled\": true}"
  let custom = "{\"enabled\": false}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("\"enabled\":false") |> should.be_true
}

pub fn merge_handles_string_values_test() {
  let base = "{\"name\": \"base\"}"
  let custom = "{\"name\": \"custom\"}"

  let result = settings_merger.merge(base, custom)
  result |> should.be_ok

  let assert Ok(merged) = result
  merged |> string.contains("\"name\":\"custom\"") |> should.be_true
}

// -----------------------------------------------------------------------------
// Error handling tests
// -----------------------------------------------------------------------------

pub fn merge_returns_error_for_invalid_base_json_test() {
  let result = settings_merger.merge("{bad json", "{}")
  result |> should.be_error

  let assert Error(err) = result
  err |> string.contains("auto-generated") |> should.be_true
}

pub fn merge_returns_error_for_invalid_custom_json_test() {
  let result = settings_merger.merge("{}", "{bad json")
  result |> should.be_error

  let assert Error(err) = result
  err |> string.contains("custom") |> should.be_true
}

pub fn merge_returns_error_for_non_object_base_test() {
  let result = settings_merger.merge("[1, 2, 3]", "{}")
  result |> should.be_error
}

pub fn merge_returns_error_for_non_object_custom_test() {
  let result = settings_merger.merge("{}", "\"string\"")
  result |> should.be_error
}
