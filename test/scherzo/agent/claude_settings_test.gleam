import gleam/list
import gleam/string
import gleeunit/should
import scherzo/agent/claude_settings

pub fn generate_autonomous_settings_produces_valid_json_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "scherzo",
    )

  // Should start with { and end with }
  result |> string.starts_with("{") |> should.be_true
  result |> string.ends_with("}") |> should.be_true
}

pub fn generate_autonomous_settings_contains_hooks_key_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "scherzo",
    )

  result |> string.contains("\"hooks\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_session_start_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "scherzo",
    )

  result |> string.contains("\"SessionStart\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_stop_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "scherzo",
    )

  result |> string.contains("\"Stop\"") |> should.be_true
}

pub fn generate_autonomous_settings_contains_pre_compact_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "scherzo",
    )

  result |> string.contains("\"PreCompact\"") |> should.be_true
}

pub fn generate_autonomous_settings_includes_task_id_in_commands_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "my-task-456",
      "agent-456",
      "scherzo",
    )

  // Task ID should appear in scherzo prime command
  result |> string.contains("scherzo prime my-task-456") |> should.be_true
  // Task ID should appear in scherzo checkpoint command
  result
  |> string.contains("scherzo checkpoint --type=final my-task-456")
  |> should.be_true
}

pub fn generate_autonomous_settings_sanitizes_unsafe_task_id_test() {
  // Task ID with shell injection attempt
  let result =
    claude_settings.generate_autonomous_settings(
      "task; rm -rf /",
      "agent-bad",
      "scherzo",
    )

  // Should NOT contain the dangerous characters
  result |> string.contains("rm -rf") |> should.be_false
  // Should contain the sanitized placeholder
  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_allows_alphanumeric_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "Task123ABC",
      "agent-abc",
      "scherzo",
    )

  result |> string.contains("scherzo prime Task123ABC") |> should.be_true
}

pub fn generate_autonomous_settings_allows_dashes_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-with-dashes",
      "agent-dash",
      "scherzo",
    )

  result |> string.contains("scherzo prime task-with-dashes") |> should.be_true
}

pub fn generate_autonomous_settings_allows_underscores_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task_with_underscores",
      "agent-underscore",
      "scherzo",
    )

  result
  |> string.contains("scherzo prime task_with_underscores")
  |> should.be_true
}

pub fn generate_autonomous_settings_rejects_spaces_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task with spaces",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_backticks_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task`whoami`",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_dollar_sign_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task$HOME",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_contains_command_type_test() {
  let result =
    claude_settings.generate_autonomous_settings("task-1", "agent-1", "scherzo")

  result |> string.contains("\"type\":\"command\"") |> should.be_true
}

// Additional security tests for shell metacharacters

pub fn generate_autonomous_settings_rejects_path_traversal_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "../../../etc/passwd",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("../") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_pipe_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task|cat /etc/passwd",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("|") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_ampersand_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task&&rm -rf",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("&&") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_redirect_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task>output.txt",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains(">") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_parentheses_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "$(whoami)",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
  result |> string.contains("$(") |> should.be_false
}

pub fn generate_autonomous_settings_rejects_newline_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task\nrm -rf /",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_null_byte_test() {
  // Null byte could truncate strings in some contexts
  let result =
    claude_settings.generate_autonomous_settings(
      "task\u{0000}malicious",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_quotes_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task'injection",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_rejects_double_quotes_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task\"injection",
      "agent-test",
      "scherzo",
    )

  result |> string.contains("invalid-task-id") |> should.be_true
}

pub fn generate_autonomous_settings_handles_empty_string_test() {
  let result =
    claude_settings.generate_autonomous_settings("", "agent-test", "scherzo")

  // Empty string should still produce valid JSON
  result |> string.starts_with("{") |> should.be_true
}

// ---------------------------------------------------------------------------
// Custom scherzo_bin path tests
// ---------------------------------------------------------------------------

pub fn generate_autonomous_settings_uses_custom_scherzo_bin_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-123",
      "agent-123",
      "/custom/path/to/scherzo",
    )

  // Should use the custom path in prime command
  result
  |> string.contains("/custom/path/to/scherzo prime task-123")
  |> should.be_true
  // Should use the custom path in checkpoint command
  result
  |> string.contains(
    "/custom/path/to/scherzo checkpoint --type=final --agent-id=agent-123 task-123",
  )
  |> should.be_true
}

pub fn generate_autonomous_settings_uses_dev_mode_scherzo_bin_test() {
  // Development mode uses "cd /path && gleam run --" format
  let scherzo_bin = "cd /home/user/scherzo && gleam run --"
  let result =
    claude_settings.generate_autonomous_settings(
      "task-456",
      "agent-456",
      scherzo_bin,
    )

  // Should include the full development command
  result
  |> string.contains("cd /home/user/scherzo && gleam run -- prime task-456")
  |> should.be_true
  result
  |> string.contains(
    "cd /home/user/scherzo && gleam run -- checkpoint --type=final --agent-id=agent-456 task-456",
  )
  |> should.be_true
}

pub fn generate_autonomous_settings_scherzo_bin_in_all_hooks_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-789",
      "agent-789",
      "my-scherzo",
    )

  // Count occurrences of "my-scherzo" - should appear 3 times
  // (SessionStart prime, Stop checkpoint, PreCompact prime)
  let count =
    result
    |> string.split("my-scherzo")
    |> list.length
  // split gives n+1 parts for n occurrences
  count |> should.equal(4)
}

pub fn generate_autonomous_settings_empty_scherzo_bin_still_works_test() {
  // Edge case: empty scherzo_bin (would result in " prime task-id")
  let result =
    claude_settings.generate_autonomous_settings("task-123", "agent-123", "")

  // Should still produce valid JSON structure
  result |> string.starts_with("{") |> should.be_true
  result |> string.contains("hooks") |> should.be_true
}

// ---------------------------------------------------------------------------
// Agent ID in Stop hook tests
// ---------------------------------------------------------------------------

pub fn generate_autonomous_settings_includes_agent_id_in_stop_hook_test() {
  let result =
    claude_settings.generate_autonomous_settings(
      "task-abc",
      "agent-task-abc-12345",
      "scherzo",
    )

  // Should include the agent ID in the checkpoint command
  result
  |> string.contains("--agent-id=agent-task-abc-12345")
  |> should.be_true
}
