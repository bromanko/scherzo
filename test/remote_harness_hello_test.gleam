import gleam/string
import scherzo/control/remote_harness_hello

pub fn remote_harness_hello_accepts_valid_message_and_redacts_auth_test() {
  let token = "test-token"
  let line =
    remote_harness_hello.encode(
      "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      token,
      ["control_commands"],
    )

  let assert Ok(hello) = remote_harness_hello.decode(line, token)
  assert hello.daemon_id == "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  assert hello.boot_id == "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  assert hello.capabilities == ["control_commands"]

  let redacted = remote_harness_hello.redact_auth(line)
  assert string.contains(redacted, "[REDACTED]")
  assert !string.contains(redacted, token)
}

pub fn remote_harness_hello_redacts_valid_json_with_auth_whitespace_test() {
  let token = "test-token"
  let line =
    "{ \"version\" : 1, \"type\" : \"hello\", \"daemon_id\" : \"daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\", \"boot_id\" : \"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\", \"auth\" : \""
    <> token
    <> "\", \"capabilities\" : [\"control_commands\"] }"

  let assert Ok(hello) = remote_harness_hello.decode(line, token)
  assert hello.daemon_id == "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

  let redacted = remote_harness_hello.redact_auth(line)
  assert string.contains(redacted, "[REDACTED]")
  assert !string.contains(redacted, token)
}

pub fn remote_harness_hello_rejects_bad_json_versions_types_and_shapes_test() {
  assert_invalid("{", "bad_json", "token")
  assert_invalid(
    "{\"version\":2,\"type\":\"hello\",\"daemon_id\":\"daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"boot_id\":\"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"auth\":\"token\",\"capabilities\":[]}",
    "unsupported_version",
    "token",
  )
  assert_invalid(
    "{\"version\":1,\"type\":\"heartbeat\",\"daemon_id\":\"daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"boot_id\":\"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"auth\":\"token\",\"capabilities\":[]}",
    "invalid_hello_type",
    "token",
  )
  assert_invalid(
    "{\"version\":1,\"type\":\"hello\",\"daemon_id\":\"daemon_bad\",\"boot_id\":\"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"auth\":\"token\",\"capabilities\":[]}",
    "invalid_hello",
    "token",
  )
  assert_invalid(
    "{\"version\":1,\"type\":\"hello\",\"daemon_id\":\"daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"boot_id\":\"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"auth\":\"token\"}",
    "invalid_hello",
    "token",
  )
}

pub fn remote_harness_hello_rejects_wrong_auth_test() {
  assert_invalid(
    "{\"version\":1,\"type\":\"hello\",\"daemon_id\":\"daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"boot_id\":\"boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"auth\":\"wrong\",\"capabilities\":[]}",
    "wrong_auth",
    "token",
  )
}

fn assert_invalid(line: String, expected_code: String, token: String) -> Nil {
  let assert Error(remote_harness_hello.HelloError(code: code, message: message)) =
    remote_harness_hello.decode(line, token)
  assert code == expected_code
  assert string.length(message) > 0
}
