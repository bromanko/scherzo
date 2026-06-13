import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/config
import scherzo/config/types as config_types
import scherzo/connect
import scherzo/control/client as control_client
import scherzo/control/file as control_file
import scherzo/control/remote/credential_store
import scherzo/control/remote/pairing_client
import scherzo/control/server as control_server
import scherzo/daemon_identity
import scherzo/log
import scherzo/model_config
import scherzo/orchestrator/daemon_remote_client
import scherzo/runtime_bundle
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import simplifile
import support/remote_ui_test_server
import support/test_helpers
import test_async

pub fn remote_daemon_registration_fake_integration_transcript_test() {
  let root = "test/tmp/remote-daemon-registration-integration"
  let transcript_path = root <> "/transcript.log"
  test_helpers.reset_dir(root)
  let server = remote_ui_test_server.start("dcred_secret_1", transcript_path)
  let server_url = remote_ui_test_server.server_url(server)
  let workspace_root = root <> "/workspaces"
  let connect_output = process.new_subject()

  let assert Ok(Nil) =
    connect.run_with_deps(
      connect.Command(
        pairing_token: "pair_secret_1",
        server_url: server_url,
        credential_ref: "work-laptop",
        daemon_label: Some("project-foo"),
        replace_credential: False,
        json: False,
        allow_loopback_url: True,
        activate: False,
        config_path: None,
      ),
      connect.Dependencies(
        load_bundle: fn(_) { connect_bundle(workspace_root) },
        load_or_create_identity: daemon_identity.load_or_create,
        exchange_pairing_token: fn(
          server_url,
          pairing_token,
          daemon_id,
          daemon_label,
          allow_loopback_url,
        ) {
          pairing_client.exchange_pairing_token_with_label(
            server_url,
            pairing_token,
            daemon_id,
            daemon_label,
            allow_loopback_url,
            pairing_client.default_dependencies(),
          )
        },
        write_credential: credential_store.write_credential,
        notify_reload: fn(_) { connect.ManualReloadRequired },
      ),
      connect.Output(line: fn(line) { process.send(connect_output, line) }),
    )
  let connect_line = test_async.expect_message(connect_output)
  let assert Ok(identity) = daemon_identity.load_or_create(workspace_root)
  let assert Ok(ref) = credential_store.normalize_credential_ref("work-laptop")
  let assert Ok(Some(_)) =
    credential_store.read_credential(ref, server_url, identity.daemon_id)

  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  hub.register_session(event_hub, session_summary())
  let #(control_handle, discovered_control_file) =
    start_control_server(event_hub, workspace_root)
  let logs = process.new_subject()
  let effective = effective_config(workspace_root, server_url)

  let assert Ok(remote_handle) =
    daemon_remote_client.start(
      effective,
      event_hub,
      [],
      fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
    )

  let _ =
    remote_ui_test_server.wait_for_contains(
      transcript_path,
      "outage_attempt=closed_before_handshake",
      150,
    )
  let assert Ok(sessions) =
    control_client.list_sessions(discovered_control_file)
  assert list.length(sessions) == 1
  append_line(transcript_path, "local_scherzoctl_fallback=ok")

  let transcript =
    remote_ui_test_server.wait_for_contains(
      transcript_path,
      "credential_revoked",
      150,
    )
  let log_entries = test_async.drain_subject(logs)
  let sanitized =
    transcript
    |> string.replace(each: "dcred_secret_1", with: "[REDACTED]")
    |> string.replace(each: identity.daemon_id, with: "[DAEMON_ID]")
  let assert Ok(Nil) = simplifile.write(transcript_path, sanitized)

  assert string.contains(connect_line, "Stored credential for daemon")
  assert string.contains(sanitized, "pairing_exchange_body=")
  assert string.contains(sanitized, "\"daemonLabel\":\"project-foo\"")
  assert string.contains(sanitized, "authorization=Bearer [REDACTED]")
  assert string.contains(sanitized, "daemon_hello")
  assert string.contains(sanitized, "heartbeat")
  assert string.contains(sanitized, "daemon_state")
  assert string.contains(sanitized, "outage_attempt=closed_before_handshake")
  assert string.contains(sanitized, "credential_revoked")
  assert string.contains(sanitized, "local_scherzoctl_fallback=ok")
  assert list.any(log_entries, fn(entry) {
    string.contains(entry, "ui_websocket_credential_revoked")
  })

  assert daemon_remote_client.stop(remote_handle, 1000) == Ok(Nil)
  control_server.stop(control_handle)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

fn start_control_server(
  event_hub: process.Subject(hub.Message),
  workspace_root: String,
) -> #(control_server.Server, control_file.ControlFile) {
  let token = "control-token"
  let assert Ok(server) =
    control_server.start(
      control_server.Settings(
        host: "127.0.0.1",
        port: 0,
        token: token,
        event_timeout_ms: 500,
        stream_poll_ms: 20,
        command_timeout_ms: 500,
      ),
      control_server.event_hub_store(event_hub),
    )
  let control =
    control_file.ControlFile(
      host: "127.0.0.1",
      port: control_server.bound_port(server),
      token: token,
      workspace_root: workspace_root,
      started_at_ms: 1,
      command_timeout_ms: 500,
    )
  #(server, control)
}

fn effective_config(
  workspace_root: String,
  server_url: String,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: workspace_root),
    control: config.default_control_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config_types.UiServerConfig(
      enabled: True,
      endpoint: Some(server_url),
      credential_ref: Some("work-laptop"),
      daemon_label: Some("project-foo"),
      command_bridge_enabled: False,
      heartbeat_interval_ms: 25,
      state_interval_ms: 25,
      retry_initial_ms: 25,
      retry_max_ms: 25,
    ),
  )
}

fn connect_bundle(
  workspace_root: String,
) -> Result(runtime_bundle.RuntimeBundle, runtime_bundle.BundleError) {
  let effective =
    config_types.EffectiveConfig(
      tracker: config.default_tracker_config(),
      polling: config.default_polling_config(),
      workspace: config_types.WorkspaceConfig(root: workspace_root),
      control: config.default_control_config(),
      hooks: config.default_hooks_config(),
      agent: config.default_agent_config(),
      pi: config.default_pi_config(),
      handoff: config.default_handoff_config(),
      linear_contract: config.default_linear_contract_config(),
      linear_commands: config.default_linear_command_config(),
      ui_server: config.default_ui_server_config(),
    )
  Ok(
    runtime_bundle.RuntimeBundle(
      config_path: "test/support/connect-bundle.yaml",
      config_contents: "",
      dependencies: [],
      effective: effective,
      orchestrator: config_types.OrchestratorConfig(
        effective: effective,
        config_dir: ".",
        routing: config_types.RoutingConfig(
          workflow_label_prefix: "workflow:",
          require_exactly_one_workflow_label: False,
          default_workflow: None,
          workflows: dict.new(),
        ),
        dag_hooks: config_types.empty_dag_hooks(),
        workspace_profiles: config_types.WorkspaceHookProfiles(
          default_profile: "default",
          profiles: dict.new(),
        ),
        artifact_limits: config_types.ArtifactLimits(
          command_stream_max_chars: 4000,
          template_field_max_chars: 4000,
          workflow_summary_max_chars: 4000,
        ),
        artifact_repositories: artifact_publication_config.empty_repositories(),
        model_settings: model_config.default_settings(),
        scheduled_jobs: [],
      ),
      workflows: dict.new(),
      secrets: [],
    ),
  )
}

fn session_summary() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    display_name: "Demo session",
    issue_id: "issue-1",
    issue_identifier: "LIV-1",
    issue_title: "Remote state",
    workspace_path: "test/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 3,
    current_turn_status: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    current_turn_started_at_ms: None,
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 123,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn append_line(path: String, line: String) -> Nil {
  let existing = case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
  let assert Ok(Nil) = simplifile.write(path, existing <> line <> "\n")
  Nil
}
