import gleam/dict
import gleam/list
import scherzo/hash
import scherzo/workspace_driver_env

fn value_for(env: List(#(String, String)), key: String) -> String {
  let assert Ok(value) = dict.get(dict.from_list(env), key)
  value
}

pub fn validates_driver_env_keys_and_reserved_names_test() {
  assert workspace_driver_env.valid_key("PATH")
  assert workspace_driver_env.valid_key("_PRIVATE")
  assert workspace_driver_env.valid_key("SCHERZO_JJ_WORKSPACE_BASE")
  assert !workspace_driver_env.valid_key("")
  assert !workspace_driver_env.valid_key("1BAD")
  assert !workspace_driver_env.valid_key("BAD-NAME")

  assert workspace_driver_env.reserved_generated_key("SCHERZO_WORKSPACE_DRIVER")
  assert workspace_driver_env.reserved_generated_key("SCHERZO_RUN_ID")
  assert !workspace_driver_env.reserved_generated_key(
    "SCHERZO_JJ_WORKSPACE_BASE",
  )
  assert !workspace_driver_env.reserved_generated_key("PATH")
}

pub fn canonicalize_sorts_and_deduplicates_entries_test() {
  assert workspace_driver_env.canonicalize([
      #("ZZZ", "last"),
      #("AAA", "first"),
      #("AAA", "duplicate"),
    ])
    == [#("AAA", "first"), #("ZZZ", "last")]
}

pub fn merge_applies_profile_then_generated_precedence_test() {
  let merged =
    workspace_driver_env.merge(
      [
        #("PATH", "/profile/bin"),
        #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
        #("SCHERZO_WORKSPACE_PATH", "profile-should-lose"),
        #("EMPTY_VALUE", ""),
      ],
      [
        #("SCHERZO_WORKSPACE_PATH", "/generated/workspace"),
        #("SCHERZO_WORKSPACE_DRIVER", "driver-command"),
      ],
    )

  assert value_for(merged, "PATH") == "/profile/bin"
  assert value_for(merged, "SCHERZO_JJ_WORKSPACE_BASE") == "profile-base"
  assert value_for(merged, "SCHERZO_WORKSPACE_PATH") == "/generated/workspace"
  assert value_for(merged, "SCHERZO_WORKSPACE_DRIVER") == "driver-command"
  assert value_for(merged, "EMPTY_VALUE") == ""
  assert list.length(merged) == 5
}

pub fn redaction_values_use_sensitive_keys_only_test() {
  let values =
    workspace_driver_env.values_for_redaction([
      #("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"),
      #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
      #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
      #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
      #("PATH", "/profile/bin"),
      #("EMPTY_VALUE", ""),
      #("SHORT_TOKEN", "short"),
      #("DRIVER_SECRET_TOKEN_COPY", "driver-env-redaction-token"),
    ])

  assert values == ["driver-env-redaction-token"]
}

pub fn fingerprint_entries_hash_values_without_raw_secrets_test() {
  let entries =
    workspace_driver_env.fingerprint_entries([
      #("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"),
      #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
    ])

  assert entries
    == [
      #("DRIVER_SECRET_TOKEN", hash.sha256_hex("driver-env-redaction-token")),
      #("SCHERZO_JJ_WORKSPACE_BASE", hash.sha256_hex("profile-base")),
    ]
}
