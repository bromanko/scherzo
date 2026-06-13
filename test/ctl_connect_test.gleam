import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/connect
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/types as query_types
import scherzo/control/remote/credential_store
import scherzo/control/remote/pairing_client
import scherzo/control/server
import scherzo/daemon_identity
import scherzo/runtime_bundle
import scherzo/session/event
import simplifile
import support/test_helpers
import test_async

fn write_config(root: String) -> String {
  write_config_with_tail(root, "")
}

fn write_config_with_tail(root: String, tail: String) -> String {
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
" <> tail)
  config_path
}

fn read_config_contents(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
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
  deps_with_activation(write_result, connect.ReloadNotified)
}

fn deps_with_activation(
  write_result: Result(
    credential_store.WriteResult,
    credential_store.StoreError,
  ),
  activation: connect.ActivationStatus,
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
      _daemon_label,
      _allow_loopback_url,
    ) {
      Ok(pairing_success(server_url, daemon_id))
    },
    write_credential: fn(_ref, _server_url, _daemon_id, _credential, _replace) {
      write_result
    },
    notify_reload: fn(_) { activation },
  )
}

fn pairing_success(
  server_url: String,
  daemon_id: String,
) -> pairing_client.PairingSuccess {
  pairing_client.PairingSuccess(
    server_url: server_url,
    daemon_id: daemon_id,
    credential: credential_store.DaemonCredential(
      Some("cred-1"),
      "dcred_secret_1",
    ),
  )
}

pub fn connect_parse_accepts_friendly_name_test() {
  let assert Ok(command) =
    connect.parse([
      "--pairing-token",
      "pair_secret_1",
      "--server-url",
      "https://ui.example.test",
      "--name",
      " Project Foo / MacBook #1 ",
    ])

  assert command.daemon_label == Some("Project Foo / MacBook #1")
  assert !command.activate
}

pub fn connect_parse_accepts_activate_test() {
  let assert Ok(command) =
    connect.parse([
      "--pairing-token",
      "pair_secret_1",
      "--server-url",
      "https://ui.example.test",
      "--activate",
    ])

  assert command.activate
}

pub fn connect_parse_rejects_invalid_friendly_name_test() {
  let assert Error(connect.UsageError(empty_message)) =
    connect.parse([
      "--pairing-token",
      "pair_secret_1",
      "--server-url",
      "https://ui.example.test",
      "--name",
      "   ",
    ])
  assert string.contains(empty_message, "--name")
  assert string.contains(empty_message, "non-empty")

  let assert Error(connect.UsageError(long_message)) =
    connect.parse([
      "--pairing-token",
      "pair_secret_1",
      "--server-url",
      "https://ui.example.test",
      "--name",
      string.repeat("x", times: 81),
    ])
  assert string.contains(long_message, "--name")
  assert string.contains(long_message, "at most 80")

  let assert Error(connect.UsageError(control_message)) =
    connect.parse([
      "--pairing-token",
      "pair_secret_1",
      "--server-url",
      "https://ui.example.test",
      "--name",
      "Project\nFoo",
    ])
  assert string.contains(control_message, "--name")
  assert string.contains(control_message, "control characters")
}

pub fn connect_usage_documents_name_precedence_and_shape_test() {
  let usage = connect.usage()

  assert string.contains(usage, "--activate")
  assert string.contains(usage, "--name <friendly-name>")
  assert string.contains(usage, "Overrides ui_server.daemon_label")
  assert string.contains(usage, "spaces and punctuation")
}

pub fn connect_cli_label_overrides_config_label_test() {
  let root = "test/tmp/connect-label-precedence"
  let config_path =
    write_config_with_tail(
      root,
      "ui_server:\n  enabled: false\n  daemon_label: Config Project\n",
    )
  let observed = process.new_subject()

  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: Some("CLI Project / MacBook"),
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      connect.Dependencies(
        load_bundle: runtime_bundle.load,
        load_or_create_identity: fn(root) {
          Ok(daemon_identity.DaemonIdentity(
            "daemon_abc",
            "boot_abc",
            root <> "/id",
          ))
        },
        exchange_pairing_token: fn(
          server_url,
          _pairing_token,
          daemon_id,
          daemon_label,
          _allow_loopback_url,
        ) {
          process.send(observed, daemon_label)
          Ok(pairing_success(server_url, daemon_id))
        },
        write_credential: fn(
          _ref,
          _server_url,
          _daemon_id,
          _credential,
          _replace,
        ) {
          Ok(credential_store.CredentialWritten("/tmp/creds.json"))
        },
        notify_reload: fn(_) { connect.ReloadNotified },
      ),
      output(process.new_subject()),
    )

  assert test_async.expect_message(observed) == Some("CLI Project / MacBook")
}

pub fn connect_uses_config_label_when_cli_name_absent_test() {
  let root = "test/tmp/connect-config-label"
  let config_path =
    write_config_with_tail(
      root,
      "ui_server:\n  enabled: false\n  daemon_label: Config Project\n",
    )
  let observed = process.new_subject()

  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      connect.Dependencies(
        load_bundle: runtime_bundle.load,
        load_or_create_identity: fn(root) {
          Ok(daemon_identity.DaemonIdentity(
            "daemon_abc",
            "boot_abc",
            root <> "/id",
          ))
        },
        exchange_pairing_token: fn(
          server_url,
          _pairing_token,
          daemon_id,
          daemon_label,
          _allow_loopback_url,
        ) {
          process.send(observed, daemon_label)
          Ok(pairing_success(server_url, daemon_id))
        },
        write_credential: fn(
          _ref,
          _server_url,
          _daemon_id,
          _credential,
          _replace,
        ) {
          Ok(credential_store.CredentialWritten("/tmp/creds.json"))
        },
        notify_reload: fn(_) { connect.ReloadNotified },
      ),
      output(process.new_subject()),
    )

  assert test_async.expect_message(observed) == Some("Config Project")
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
        daemon_label: Some("Project Foo / MacBook"),
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "credential_ref work-laptop")
  assert string.contains(line, "Project Foo / MacBook")
  assert string.contains(line, "Project config was not changed")
  assert string.contains(
    line,
    "Notified the running daemon to reload stored UI pairing",
  )
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
        daemon_label: None,
        replace_credential: False,
        json: True,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialAlreadyStored("/tmp/creds.json"))),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "\"credential_ref\":\"work-laptop\"")
  assert string.contains(line, "\"activation_status\":\"reload_notified\"")
  assert !string.contains(line, "daemon_label")
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_json_output_includes_non_secret_daemon_label_test() {
  let root = "test/tmp/connect-json-label"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: Some("Project Foo"),
        replace_credential: False,
        json: True,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialAlreadyStored("/tmp/creds.json"))),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "\"daemon_label\":\"Project Foo\"")
  assert string.contains(line, "\"config_activation_status\":\"not_requested\"")
  assert string.contains(
    line,
    "\"activation_message\":\"Notified the running daemon to reload stored UI pairing.\"",
  )
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_pretty_output_reports_manual_reload_fallback_test() {
  let root = "test/tmp/connect-manual-reload"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps_with_activation(
        Ok(credential_store.CredentialWritten("/tmp/creds.json")),
        connect.ManualReloadRequired,
      ),
      output(subject),
    )
  let line = test_async.expect_message(subject)
  assert string.contains(line, "Run scherzoctl reload or restart the daemon")
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_without_activate_does_not_mutate_project_config_test() {
  let root = "test/tmp/connect-no-activate-config"
  let config_path = write_config(root)
  let before = read_config_contents(config_path)
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: Some("Project Foo"),
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(process.new_subject()),
    )

  assert read_config_contents(config_path) == before
}

pub fn connect_activate_writes_non_secret_ui_server_config_test() {
  let root = "test/tmp/connect-activate-config"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test/",
        credential_ref: "work-laptop",
        daemon_label: Some("Project Foo / MacBook"),
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(subject),
    )

  let config = read_config_contents(config_path)
  let line = test_async.expect_message(subject)
  assert string.contains(config, "ui_server:\n")
  assert string.contains(config, "  enabled: true")
  assert string.contains(config, "  endpoint: \"https://ui.example.test\"")
  assert string.contains(config, "  credential_ref: \"work-laptop\"")
  assert string.contains(config, "  daemon_label: \"Project Foo / MacBook\"")
  assert !string.contains(config, "pair_secret_1")
  assert !string.contains(config, "dcred_secret_1")
  assert string.contains(line, "Activated ui_server")
  assert string.contains(line, "daemon should now connect")
  assert string.contains(line, "Hot-reloaded")
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_activate_json_reports_manual_reload_fallback_test() {
  let root = "test/tmp/connect-activate-manual-reload"
  let config_path = write_config(root)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: True,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps_with_activation(
        Ok(credential_store.CredentialWritten("/tmp/creds.json")),
        connect.ManualReloadRequired,
      ),
      output(subject),
    )

  let config = read_config_contents(config_path)
  let line = test_async.expect_message(subject)
  assert string.contains(config, "  enabled: true")
  assert string.contains(
    line,
    "\"config_activation_status\":\"config_updated\"",
  )
  assert string.contains(line, "\"reload_status\":\"manual_reload_required\"")
  assert string.contains(
    line,
    "\"reload_message\":\"Run scherzoctl reload or restart the daemon to start the UI connection.\"",
  )
  assert !string.contains(line, "pair_secret_1")
  assert !string.contains(line, "dcred_secret_1")
}

pub fn connect_activate_accepts_commented_ui_server_header_test() {
  let root = "test/tmp/connect-activate-commented-header"
  let config_path =
    write_config_with_tail(root, "ui_server:   # remote UI\n  enabled: false\n")
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(process.new_subject()),
    )

  let config = read_config_contents(config_path)
  assert string.contains(config, "ui_server:   # remote UI\n")
  assert string.contains(config, "  enabled: true")
  assert string.contains(config, "  endpoint: \"https://ui.example.test\"")
  assert string.contains(config, "  credential_ref: \"work-laptop\"")
}

pub fn connect_activate_preserves_existing_ui_server_fields_test() {
  let root = "test/tmp/connect-activate-preserve-fields"
  let config_path =
    write_config_with_tail(
      root,
      "ui_server:\n  # keep command bridge setting\n  command_bridge_enabled: true\n  enabled: false\n",
    )
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(process.new_subject()),
    )

  let config = read_config_contents(config_path)
  assert string.contains(config, "  # keep command bridge setting")
  assert string.contains(config, "  command_bridge_enabled: true")
  assert string.contains(config, "  enabled: true")
  assert string.contains(config, "  endpoint: \"https://ui.example.test\"")
  assert string.contains(config, "  credential_ref: \"work-laptop\"")
}

pub fn connect_activate_is_idempotent_for_matching_ui_server_config_test() {
  let root = "test/tmp/connect-activate-idempotent"
  let config_path =
    write_config_with_tail(
      root,
      "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  credential_ref: work-laptop\n  daemon_label: Project Foo\n",
    )
  let before = read_config_contents(config_path)
  let subject = process.new_subject()
  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: Some("Project Foo"),
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialAlreadyStored("/tmp/creds.json"))),
      output(subject),
    )

  assert read_config_contents(config_path) == before
  assert string.contains(test_async.expect_message(subject), "already active")
}

pub fn connect_activate_rejects_conflicting_ui_server_config_test() {
  let root = "test/tmp/connect-activate-conflict"
  let config_path =
    write_config_with_tail(
      root,
      "ui_server:\n  enabled: false\n  endpoint: https://other.example.test\n  credential_ref: work-laptop\n",
    )
  let before = read_config_contents(config_path)
  let assert Error(connect.Failed(code, message)) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      deps(Ok(credential_store.CredentialWritten("/tmp/creds.json"))),
      output(process.new_subject()),
    )

  assert code == "ui_server_activation_conflict"
  assert string.contains(message, "ui_server.endpoint")
  assert string.contains(message, "refusing to replace")
  assert read_config_contents(config_path) == before
}

pub fn connect_activate_rejects_inline_ui_server_before_side_effects_test() {
  let root = "test/tmp/connect-activate-inline-unsupported"
  let config_path =
    write_config_with_tail(root, "ui_server: { enabled: false }\n")
  let before = read_config_contents(config_path)
  let observed = process.new_subject()
  let assert Error(connect.Failed(code, message)) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      connect.Dependencies(
        load_bundle: runtime_bundle.load,
        load_or_create_identity: fn(root) {
          process.send(observed, "identity")
          Ok(daemon_identity.DaemonIdentity(
            "daemon_abc",
            "boot_abc",
            root <> "/id",
          ))
        },
        exchange_pairing_token: fn(
          server_url,
          _pairing_token,
          daemon_id,
          _daemon_label,
          _allow_loopback_url,
        ) {
          process.send(observed, "exchange")
          Ok(pairing_success(server_url, daemon_id))
        },
        write_credential: fn(
          _ref,
          _server_url,
          _daemon_id,
          _credential,
          _replace,
        ) {
          process.send(observed, "write")
          Ok(credential_store.CredentialWritten("/tmp/creds.json"))
        },
        notify_reload: fn(_) {
          process.send(observed, "reload")
          connect.ReloadNotified
        },
      ),
      output(process.new_subject()),
    )

  assert code == "ui_server_activation_unsupported"
  assert string.contains(message, "block-style ui_server")
  assert read_config_contents(config_path) == before
  test_async.assert_no_extra_message(observed)
}

pub fn connect_activate_write_failure_reports_partial_success_test() {
  let root = "test/tmp/connect-activate-write-failure"
  let config_path = write_config(root)
  let observed = process.new_subject()
  let assert Error(connect.Failed(code, message)) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: "https://ui.example.test",
        credential_ref: "work-laptop",
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: True,
        config_path: Some(config_path),
      ),
      connect.Dependencies(
        load_bundle: fn(path) {
          case runtime_bundle.load(path) {
            Ok(bundle) ->
              Ok(runtime_bundle.RuntimeBundle(..bundle, config_path: root))
            Error(error) -> Error(error)
          }
        },
        load_or_create_identity: fn(workspace_root) {
          process.send(observed, "identity")
          Ok(daemon_identity.DaemonIdentity(
            "daemon_abc",
            "boot_abc",
            workspace_root <> "/id",
          ))
        },
        exchange_pairing_token: fn(
          server_url,
          _pairing_token,
          daemon_id,
          _daemon_label,
          _allow_loopback_url,
        ) {
          process.send(observed, "exchange")
          Ok(pairing_success(server_url, daemon_id))
        },
        write_credential: fn(
          _ref,
          _server_url,
          _daemon_id,
          _credential,
          _replace,
        ) {
          process.send(observed, "write")
          Ok(credential_store.CredentialWritten("/tmp/creds.json"))
        },
        notify_reload: fn(_) {
          process.send(observed, "reload")
          connect.ReloadNotified
        },
      ),
      output(process.new_subject()),
    )

  assert code == "ui_server_activation_failed"
  assert string.contains(message, "Stored daemon credential at /tmp/creds.json")
  assert string.contains(message, "failed to activate ui_server")
  assert string.contains(message, "fresh pairing token")
  assert string.contains(message, "--replace-credential")
  assert !string.contains(message, "pair_secret_1")
  assert !string.contains(message, "dcred_secret_1")
  assert test_async.expect_message(observed) == "identity"
  assert test_async.expect_message(observed) == "exchange"
  assert test_async.expect_message(observed) == "write"
  test_async.assert_no_extra_message(observed)

  let config = read_config_contents(config_path)
  assert !string.contains(config, "ui_server:\n")
  assert !string.contains(config, "pair_secret_1")
  assert !string.contains(config, "dcred_secret_1")
}

pub fn notify_local_reload_for_workspace_applies_reload_command_test() {
  let workspace_root = "test/tmp/connect-notify-reload/workspaces/main"
  test_helpers.reset_dir("test/tmp/connect-notify-reload")
  let command_subject = process.new_subject()
  let backend =
    server.Backend(
      list_sessions: fn(_) { Ok(event.SessionList(sessions: [], now_ms: 1)) },
      get_session: fn(_, _) { Ok(None) },
      events_after: fn(_, cursor, _, _) {
        Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
      },
      query: fn(_) {
        Error(query_types.QueryError(
          query_types.UnsupportedQuery,
          "query backend unavailable",
        ))
      },
      apply_command: fn(operator_command, _) {
        process.send(command_subject, operator_command)
        Ok(command.applied(operator_command, Some("done")))
      },
    )
  let assert Ok(server_handle) =
    server.start(
      server.Settings(
        host: "127.0.0.1",
        port: 0,
        token: "control-token",
        event_timeout_ms: 500,
        stream_poll_ms: 20,
        command_timeout_ms: 500,
      ),
      backend,
    )
  let control_path = control_file.path_for_workspace(workspace_root)
  let assert Ok(Nil) =
    control_file.write(
      control_path,
      control_file.ControlFile(
        host: "127.0.0.1",
        port: server.bound_port(server_handle),
        token: "control-token",
        workspace_root: workspace_root,
        started_at_ms: 1,
        command_timeout_ms: 500,
      ),
    )

  assert connect.notify_local_reload_for_workspace(workspace_root)
    == connect.ReloadNotified
  let assert Ok(applied_command) =
    process.receive(command_subject, within: 1000)
  assert applied_command == command.ReloadWorkflow

  server.stop(server_handle)
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
        daemon_label: None,
        replace_credential: False,
        json: False,
        allow_loopback_url: False,
        activate: False,
        config_path: Some(config_path),
      ),
      deps(Error(credential_store.ReplaceRequired("/tmp/creds.json"))),
      output(process.new_subject()),
    )
  assert code == "replace_required"
  assert string.contains(message, "/tmp/creds.json")
}
