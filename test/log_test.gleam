import gleam/string
import scherzo/log

pub fn log_escapes_spaces_newlines_equals_and_quotes_test() {
  let line =
    log.info("workspace prepared", [
      #("issue_id", "issue id"),
      #("path", "a=b"),
      #("message", "hello\n\"there\""),
    ])

  assert string.contains(line, "level=info")
  assert string.contains(line, "event=\"workspace prepared\"")
  assert string.contains(line, "issue_id=\"issue id\"")
  assert string.contains(line, "path=\"a=b\"")
  assert string.contains(line, "message=\"hello\\n\\\"there\\\"\"")
}

pub fn log_redacts_sensitive_field_names_test() {
  let line =
    log.info("startup_failed", [
      #("LINEAR_API_KEY", "lin-secret"),
      #("authorization", "Bearer token"),
      #("normal", "visible"),
    ])

  assert string.contains(line, "LINEAR_API_KEY=[REDACTED]")
  assert string.contains(line, "authorization=[REDACTED]")
  assert string.contains(line, "normal=visible")
  assert !string.contains(line, "lin-secret")
  assert !string.contains(line, "Bearer token")
}

pub fn log_redacts_known_secret_inside_error_string_test() {
  let line =
    log.info_with_secrets(
      "tracker_error",
      [#("error", "request failed with abc123 inside")],
      ["abc123"],
    )

  assert string.contains(line, "[REDACTED]")
  assert !string.contains(line, "abc123")
}

pub fn log_redacts_registration_tokens_without_redacting_repair_words_test() {
  let repair_line =
    log.info("plan_completion", [
      #("stdout", "PLAN_COMPLETION_RECOVERY_STATUS=repair_needed"),
    ])
  assert string.contains(repair_line, "repair_needed")

  let secret_line =
    log.info("remote_registration", [
      #("message", "server revoked dcred_secret_1"),
      #("invite", "pair_secret_1"),
    ])
  assert string.contains(secret_line, "[REDACTED]")
  assert !string.contains(secret_line, "dcred_secret_1")
  assert !string.contains(secret_line, "pair_secret_1")
}
