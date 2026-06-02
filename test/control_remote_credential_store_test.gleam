import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/result
import scherzo/control/remote/credential_store
import simplifile

pub type TestMessage {
  ChmodCalled(String)
}

fn reset_root(root: String) {
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  Nil
}

fn test_ref() -> credential_store.CredentialRef {
  let assert Ok(ref) = credential_store.normalize_credential_ref("work-laptop")
  ref
}

fn test_dependencies(
  root: String,
  subject: process.Subject(TestMessage),
) -> credential_store.Dependencies {
  credential_store.Dependencies(
    home_dir: fn() { Ok(root) },
    temp_dir: fn() { Ok(root <> "/tmp") },
    is_file: fn(path) {
      case simplifile.is_file(path) {
        Ok(True) -> True
        _ -> False
      }
    },
    read: fn(path) {
      simplifile.read(path) |> result.map_error(simplifile.describe_error)
    },
    write: fn(path, contents) {
      simplifile.write(path, contents)
      |> result.map_error(simplifile.describe_error)
    },
    create_directory_all: fn(path) {
      simplifile.create_directory_all(path)
      |> result.map_error(simplifile.describe_error)
    },
    chmod_private: fn(path) {
      process.send(subject, ChmodCalled(path))
      Ok(Nil)
    },
  )
}

pub fn normalize_credential_ref_validation_test() {
  let assert Ok(credential_store.CredentialRef(profile: profile)) =
    credential_store.normalize_credential_ref(" work-laptop.1 ")
  assert profile == "work-laptop.1"

  let assert Error(message) =
    credential_store.normalize_credential_ref("bad/name")
  assert message
    == "ui_server.credential_ref must use letters, digits, dot, underscore, or hyphen"
}

pub fn write_credential_persists_owner_only_file_test() {
  let root = "test/tmp/credential-store-write"
  reset_root(root)
  let subject = process.new_subject()
  let deps = test_dependencies(root, subject)
  let ref = test_ref()

  let assert Ok(credential_store.CredentialWritten(path)) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-1"),
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )

  assert path == root <> "/.config/scherzo/daemon-credentials/work-laptop.json"
  let assert Ok(ChmodCalled(chmod_path)) = process.receive(subject, within: 0)
  assert chmod_path == path
  let assert Ok(Some(stored)) =
    credential_store.read_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      deps,
    )
  assert stored.credential_id == Some("cred-1")
  assert stored.secret == "dcred_secret_alpha"
}

pub fn write_credential_is_idempotent_for_same_secret_test() {
  let root = "test/tmp/credential-store-idempotent"
  reset_root(root)
  let subject = process.new_subject()
  let deps = test_dependencies(root, subject)
  let ref = test_ref()

  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-1"),
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)

  let assert Ok(credential_store.CredentialAlreadyStored(_)) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-1"),
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )
}

pub fn write_credential_requires_replace_for_different_secret_test() {
  let root = "test/tmp/credential-store-replace"
  reset_root(root)
  let subject = process.new_subject()
  let deps = test_dependencies(root, subject)
  let ref = test_ref()

  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-1"),
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)

  let assert Error(credential_store.ReplaceRequired(path)) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-2"),
        secret: "dcred_secret_beta",
      ),
      False,
      deps,
    )
  assert path == root <> "/.config/scherzo/daemon-credentials/work-laptop.json"
}

pub fn write_credential_replace_updates_existing_entry_test() {
  let root = "test/tmp/credential-store-explicit-replace"
  reset_root(root)
  let subject = process.new_subject()
  let deps = test_dependencies(root, subject)
  let ref = test_ref()

  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-1"),
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)

  let assert Ok(credential_store.CredentialWritten(_)) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: Some("cred-2"),
        secret: "dcred_secret_beta",
      ),
      True,
      deps,
    )
  let _ = process.receive(subject, within: 0)
  let assert Ok(Some(stored)) =
    credential_store.read_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      deps,
    )
  assert stored.credential_id == Some("cred-2")
  assert stored.secret == "dcred_secret_beta"
}

pub fn write_credential_scopes_by_server_and_daemon_test() {
  let root = "test/tmp/credential-store-scoped"
  reset_root(root)
  let subject = process.new_subject()
  let deps = test_dependencies(root, subject)
  let ref = test_ref()

  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: None,
        secret: "dcred_secret_alpha",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)
  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_beta",
      credential_store.DaemonCredential(
        credential_id: None,
        secret: "dcred_secret_beta",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)
  let assert Ok(_) =
    credential_store.write_credential_with(
      ref,
      "https://ui-other.example.test",
      "daemon_alpha",
      credential_store.DaemonCredential(
        credential_id: None,
        secret: "dcred_secret_gamma",
      ),
      False,
      deps,
    )
  let _ = process.receive(subject, within: 0)

  let assert Ok(Some(alpha)) =
    credential_store.read_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_alpha",
      deps,
    )
  let assert Ok(Some(beta)) =
    credential_store.read_credential_with(
      ref,
      "https://ui.example.test",
      "daemon_beta",
      deps,
    )
  let assert Ok(Some(other_server)) =
    credential_store.read_credential_with(
      ref,
      "https://ui-other.example.test",
      "daemon_alpha",
      deps,
    )

  assert alpha.secret == "dcred_secret_alpha"
  assert beta.secret == "dcred_secret_beta"
  assert other_server.secret == "dcred_secret_gamma"
}
