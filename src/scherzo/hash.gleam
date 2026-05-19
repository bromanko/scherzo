import gleam/string

pub fn sha256_hex(contents: String) -> String {
  do_sha256_hex(contents)
}

pub fn sha256_hex_bytes(contents: BitArray) -> String {
  do_sha256_hex_bytes(contents)
}

pub fn short_sha256_hex(contents: String, chars: Int) -> String {
  let chars = case chars < 0 {
    True -> 0
    False -> chars
  }
  string.slice(sha256_hex(contents), 0, chars)
}

@external(erlang, "scherzo_hash_ffi", "sha256_hex")
fn do_sha256_hex(contents: String) -> String

@external(erlang, "scherzo_hash_ffi", "sha256_hex")
fn do_sha256_hex_bytes(contents: BitArray) -> String
