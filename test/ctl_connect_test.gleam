import gleam/erlang/process
import gleam/option.{Some}
import gleam/string
import scherzo/connect
import scherzo/control/remote/credential_store
import scherzo/control/remote/pairing_client
import scherzo/daemon_identity
import scherzo/runtime_bundle
import simplifile
import support/test_helpers
import test_async

fn write_config(root: String) -> String {
  test_helpers.reset_dir(root)
  let config_path = root <> "/scherzo.yaml"
  let workflow_dir = root <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let workspace_root = root <> "/workspaces"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
",
    )
  let assert Ok(Nil) = simplifile.write(config_path, "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
workspace:
  root: " <> workspace_root <> "
agents:
  concurrency: 1
  sessions_per_task: 1
  retries:
    attempts: 1
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
")
  config_path
}

fn output(subject: process.Subject(String)) -> connect.Output {
  connect.Output(line: fn(line) { process.send(subject, line) })
}

fn deps(
  write_result: Result(
    credential_store.WriteResult,
    credential_store.StoreError,
  ),
) -> connect.Dependencies {
  connect.Dependencies(
    load_bundle: runtime_bundle.load,
    load_or_create_identity: fn(root) {
      Ok(daemon_identity.DaemonIdentity("daemon_abc", "boot_abc", root <> "/id"))
    },
    exchange_pairing_token: fn(
      server_url,
      _pairing_token,
      daemon_id,
      _allow_loopback_url,
    ) {
      Ok(pairing_client.PairingSuccess(
        server_url: server_url,
        daemon_id: daemon_id,
        credential: credential_store.DaemonCredential(
          Some("cred-1"),
          "dcred_secret_1",
        ),
      ))
    },
    write_credential: fn(_ref, _server_url, _daemon_id, _credential, _replace) {
      write_result
    },
  )
}

pub fn connect_pretty_output_is_redacted_test() {
  let root = "test/tmp/connect-pretty"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "credential_ref work-laptop")
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_json_output_is_redacted_test() {
  let root = "test/tmp/connect-json"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        replace_credential: False,
        json: True,
        allow_loopback_url: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialAlreadyStored("/tmp/creds.json"))),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "\"credential_ref\":\"work-laptop\"")
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_replace_required_error_test() {
  let root = "test/tmp/connect-replace"
  let config_path = write_config(root)
  let assert Error(connect.Failed(code, message)) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        config_path: Some(config_path),
      ),
      deps(Error(credential_store.ReplaceRequired("/tmp/creds.json"))),
      output(process.new_subject()),
    )
  assert code == "replace_required"
  assert string.contains(message, "/tmp/creds.json")
}
