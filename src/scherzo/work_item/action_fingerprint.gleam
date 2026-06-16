import gleam/string
import scherzo/hash

pub fn instance_id(action_id: String, target_key: String) -> String {
  "wia_" <> hash.short_sha256_hex(action_id <> ":" <> target_key, 12)
}

pub fn fingerprint(parts: List(String)) -> String {
  parts |> string.join(with: "|") |> hash.sha256_hex
}
