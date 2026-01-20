import gleam/string
import gleeunit/should
import scherzo/agent/claude_settings

pub fn generate_autonomous_settings_produces_valid_json_test() {
  let result = claude_settings.generate_autonomous_settings("task-123")

  // Should start with { and end with }
  result |> string.starts_with("{") |> should.be_true
  result |> string.ends_with("}") |> should.be_true
}

pub fn generate_autonomous_settings_contains_hooks_key_test() {
  let result = claude_settings.generate_autonomous_settings("task-123")

  result |> string.contains("\"hooks\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_session_start_test() {
  let result = claude_settings.generate_autonomous_settings("task-123")

  result |> string.contains("\"SessionStart\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_stop_test() {
  let result = claude_settings.generate_autonomous_settings("task-123")

  result |> string.contains("\"Stop\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_pre_compact_test() {
  let result = claude_settings.generate_autonomous_settings("task-123")

  result |> string.contains("\"PreCompact\"") |> should.be_true
}

pub fn generate_autonomous_settings_includes_task_id_in_commands_test() {
  let result = claude_settings.generate_autonomous_settings("my-task-456")

  // Task ID should appear in scherzo prime command
  result |> string.contains("scherzo prime my-task-456") |> should.be_true
  // Task ID should appear in scherzo checkpoint command
  result
  |> string.contains("scherzo checkpoint --type=final my-task-456")
  |> should.be_true
}

pub fn generate_autonomous_settings_sanitizes_unsafe_task_id_test() {
  // Task ID with shell injection attempt
  let result = claude_settings.generate_autonomous_settings("task; rm -rf /")

  // Should NOT contain the dangerous characters
  result |> string.contains("rm -rf") |> should.be_false
  // Should contain the sanitized placeholder
  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_allows_alphanumeric_test() {
  let result = claude_settings.generate_autonomous_settings("Task123ABC")

  result |> string.contains("scherzo prime Task123ABC") |> should.be_true
}

pub fn generate_autonomous_settings_allows_dashes_test() {
  let result = claude_settings.generate_autonomous_settings("task-with-dashes")

  result |> string.contains("scherzo prime task-with-dashes") |> should.be_true
}

pub fn generate_autonomous_settings_allows_underscores_test() {
  let result =
    claude_settings.generate_autonomous_settings("task_with_underscores")

  result
  |> string.contains("scherzo prime task_with_underscores")
  |> should.be_true
}

pub fn generate_autonomous_settings_rejects_spaces_test() {
  let result = claude_settings.generate_autonomous_settings("task with spaces")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_backticks_test() {
  let result = claude_settings.generate_autonomous_settings("task`whoami`")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_dollar_sign_test() {
  let result = claude_settings.generate_autonomous_settings("task$HOME")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_contains_command_type_test() {
  let result = claude_settings.generate_autonomous_settings("task-1")

  result |> string.contains("\"type\":\"command\"") |> should.be_true
}

// Additional security tests for shell metacharacters

pub fn generate_autonomous_settings_rejects_path_traversal_test() {
  let result =
    claude_settings.generate_autonomous_settings("../../../etc/passwd")

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("../") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_pipe_test() {
  let result =
    claude_settings.generate_autonomous_settings("task|cat /etc/passwd")

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("|") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_ampersand_test() {
  let result = claude_settings.generate_autonomous_settings("task&&rm -rf")

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("&&") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_redirect_test() {
  let result = claude_settings.generate_autonomous_settings("task>output.txt")

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains(">") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_parentheses_test() {
  let result = claude_settings.generate_autonomous_settings("$(whoami)")

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("$(") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_newline_test() {
  let result = claude_settings.generate_autonomous_settings("task\nrm -rf /")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_null_byte_test() {
  // Null byte could truncate strings in some contexts
  let result =
    claude_settings.generate_autonomous_settings("task\u{0000}malicious")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_quotes_test() {
  let result = claude_settings.generate_autonomous_settings("task'injection")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_double_quotes_test() {
  let result = claude_settings.generate_autonomous_settings("task\"injection")

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_handles_empty_string_test() {
  let result = claude_settings.generate_autonomous_settings("")

  // Empty string should still produce valid JSON
  result |> string.starts_with("{") |> should.be_true
}
