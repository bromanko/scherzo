import gleam/option.{type Option, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/remote/credential_store
import scherzo/daemon_identity
import scherzo/orchestrator/daemon_remote_client
import scherzo/session/hub
import support/remote_ui_test_server
import support/test_helpers

pub fn daemon_remote_client_requires_stored_credential_test() {
  let root = "test/tmp/daemon-remote-client-missing-credential"
  test_helpers.reset_dir(root)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(root, Some("https://ui.example.test"), Some("work-laptop"))

  let assert Error(daemon_remote_client.StartError(code, message)) =
    daemon_remote_client.start(effective, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  assert code == "missing_daemon_credential"
  assert message != ""
  hub.stop(event_hub)
}

pub fn daemon_remote_client_rejects_invalid_loopback_endpoint_test() {
  let root = "test/tmp/daemon-remote-client-invalid-endpoint"
  test_helpers.reset_dir(root)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(root, Some("http://0.0.0.0:3000"), Some("work-laptop"))

  let assert Error(daemon_remote_client.StartError(code, _)) =
    daemon_remote_client.start(effective, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  assert code == "invalid_loopback_url"
  hub.stop(event_hub)
}

pub fn daemon_remote_client_uses_websocket_authorization_handshake_test() {
  let root = "test/tmp/daemon-remote-client-websocket"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("dcred_secret_1", transcript_path)
  let assert Ok(identity) = daemon_identity.load_or_create(root)
  let assert Ok(ref) = credential_store.normalize_credential_ref("work-laptop")
  let assert Ok(_) =
    credential_store.write_credential(
      ref,
      remote_ui_test_server.server_url(server),
      identity.daemon_id,
      credential_store.DaemonCredential(Some("cred-1"), "dcred_secret_1"),
      False,
    )
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(
      root,
      Some(remote_ui_test_server.server_url(server)),
      Some("work-laptop"),
    )

  let assert Ok(handle) =
    daemon_remote_client.start(effective, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  let transcript =
    remote_ui_test_server.wait_for_contains(
      transcript_path,
      "client_frame=",
      100,
    )
  assert string.contains(transcript, "authorization=Bearer dcred_secret_1")
  assert string.contains(transcript, "daemon_hello")
  assert string.contains(transcript, "heartbeat")
  assert string.contains(transcript, "daemon_state")
  assert daemon_remote_client.stop(handle, 1000) == Ok(Nil)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

fn effective_config(
  root: String,
  endpoint: Option(String),
  credential_ref: Option(String),
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: root),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config_types.UiServerConfig(
      enabled: True,
      endpoint: endpoint,
      credential_ref: credential_ref,
      command_bridge_enabled: False,
      heartbeat_interval_ms: 1000,
      state_interval_ms: 1000,
      retry_initial_ms: 50,
      retry_max_ms: 100,
    ),
  )
}
