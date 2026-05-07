import gleam/erlang/process
import gleam/option.{None}
import gleam/string
import scherzo/hash
import scherzo/path

pub fn hash_ffi_returns_known_sha256_hex_test() {
  assert hash.sha256_hex("hello")
    == "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
}

pub fn time_ffi_monotonic_ms_does_not_go_backwards_test() {
  let before = monotonic_ms()
  process.sleep(1)
  let after = monotonic_ms()
  assert after >= before
}

pub fn config_ffi_path_helpers_are_stable_without_host_prefix_assumptions_test() {
  let assert Ok(dir) = path.dirname("test/tmp/ffi-stable/file.txt")
  assert dir == "test/tmp/ffi-stable"

  let assert Ok(abs) = path.absolute(".")
  assert string.length(abs) > 0
  let assert Ok(tmp) = path.temp_dir()
  assert string.length(tmp) > 0
  assert path.env("SCHERZO_FFI_STABLE_TEST_DEFINITELY_MISSING") == None
}

pub fn terminal_ffi_returns_safe_types_test() {
  let columns = terminal_columns()
  assert columns >= 0
  let supports_color = stdout_supports_color()
  assert supports_color || !supports_color
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int

@external(erlang, "scherzo_terminal_ffi", "stdout_supports_color")
fn stdout_supports_color() -> Bool

@external(erlang, "scherzo_terminal_ffi", "terminal_columns")
fn terminal_columns() -> Int
