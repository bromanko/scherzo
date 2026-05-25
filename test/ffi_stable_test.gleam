import gleam/option.{None}
import gleam/string
import scherzo/hash
import scherzo/path

pub fn hash_ffi_returns_known_sha256_hex_test() {
  assert hash.sha256_hex("hello")
    == "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
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
