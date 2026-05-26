import gleam/list
import gleam/string
import scherzo/daemon_identity
import simplifile

fn reset_root(root: String) {
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  Nil
}

fn persisted_path(root: String) -> String {
  daemon_identity.path_for_workspace_root(root)
}

fn read_persisted(root: String) -> String {
  let assert Ok(contents) = simplifile.read(persisted_path(root))
  contents
}

fn daemon_prefix() -> String {
  "{\"version\":1,\"daemon_id\":\""
}

fn is_id_shape(value: String, prefix: String) -> Bool {
  string.starts_with(value, prefix)
  && string.length(value) == string.length(prefix) + 32
  && value
  |> string.drop_start(string.length(prefix))
  |> is_lower_hex_string
}

fn is_lower_hex_string(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    chars -> chars |> list.all(is_lower_hex_char)
  }
}

fn is_lower_hex_char(char: String) -> Bool {
  case char {
    "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f" -> True
    _ -> False
  }
}

pub fn first_boot_writes_versioned_daemon_identity_test() {
  let root = "test/tmp/daemon-identity-first-boot"
  reset_root(root)

  let assert Ok(identity) = daemon_identity.load_or_create(root)
  let contents = read_persisted(root)

  assert identity.path == persisted_path(root)
  assert is_id_shape(identity.daemon_id, "daemon_")
  assert is_id_shape(identity.boot_id, "boot_")
  assert contents == daemon_prefix() <> identity.daemon_id <> "\"}"
}

pub fn repeated_boot_reuses_daemon_id_and_generates_fresh_boot_id_test() {
  let root = "test/tmp/daemon-identity-repeated-boot"
  reset_root(root)

  let assert Ok(first) = daemon_identity.load_or_create(root)
  let contents = read_persisted(root)
  let assert Ok(second) = daemon_identity.load_or_create(root)

  assert second.daemon_id == first.daemon_id
  assert second.boot_id != first.boot_id
  assert read_persisted(root) == contents
}

pub fn missing_identity_file_recreates_identity_test() {
  let root = "test/tmp/daemon-identity-missing-file"
  reset_root(root)

  let assert Ok(first) = daemon_identity.load_or_create(root)
  let path = persisted_path(root)
  let assert Ok(Nil) = simplifile.delete_file(path)
  let assert Ok(second) = daemon_identity.load_or_create(root)

  assert second.daemon_id != first.daemon_id
  assert second.path == path
  assert read_persisted(root) == daemon_prefix() <> second.daemon_id <> "\"}"
}

pub fn malformed_identity_file_fails_without_overwrite_test() {
  let root = "test/tmp/daemon-identity-malformed"
  reset_root(root)

  let contents = "not json"
  let path = persisted_path(root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) = simplifile.write(path, contents)

  let assert Error(daemon_identity.IdentityInvalid(error_path, _)) =
    daemon_identity.load_or_create(root)
  assert error_path == path
  assert read_persisted(root) == contents
}

pub fn unsupported_version_fails_without_overwrite_test() {
  let root = "test/tmp/daemon-identity-unsupported-version"
  reset_root(root)

  let contents =
    "{\"version\":2,\"daemon_id\":\"daemon_0123456789abcdef0123456789abcdef\"}"
  let path = persisted_path(root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) = simplifile.write(path, contents)

  let assert Error(daemon_identity.IdentityInvalid(error_path, message)) =
    daemon_identity.load_or_create(root)
  assert error_path == path
  assert string.contains(message, "unsupported")
  assert read_persisted(root) == contents
}

pub fn invalid_daemon_id_shape_fails_without_overwrite_test() {
  let root = "test/tmp/daemon-identity-invalid-shape"
  reset_root(root)

  let contents = "{\"version\":1,\"daemon_id\":\"daemon_NOTHEX\"}"
  let path = persisted_path(root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) = simplifile.write(path, contents)

  let assert Error(daemon_identity.IdentityInvalid(error_path, message)) =
    daemon_identity.load_or_create(root)
  assert error_path == path
  assert string.contains(message, "invalid shape")
  assert read_persisted(root) == contents
}

pub fn empty_workspace_root_returns_error_test() {
  let assert Error(daemon_identity.InvalidWorkspaceRoot(root)) =
    daemon_identity.load_or_create("   ")
  assert root == ""
}
