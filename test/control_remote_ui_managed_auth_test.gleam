import gleam/option.{None, Some}
import gleam/string
import scherzo/control/remote/ui_managed_auth

pub fn managed_auth_uses_runtime_credential_after_launch_grant_ttl_test() {
  let auth =
    Some(ui_managed_auth.ManagedLaunchAuth(
      launch_credential: Some("launch_secret_1"),
      launch_expires_at_ms: 100,
      runtime_credential: None,
    ))

  let auth =
    ui_managed_auth.capture_runtime_credential(
      auth,
      Some("runtime_secret_1"),
      50,
    )
  let #(next_auth, credential) =
    ui_managed_auth.credential_for_connect(auth, "durable_secret", 101)

  assert credential == Ok("runtime_secret_1")
  let assert Some(ui_managed_auth.ManagedLaunchAuth(
    launch_credential: None,
    runtime_credential: Some("runtime_secret_1"),
    ..,
  )) = next_auth
  let secrets =
    ui_managed_auth.redaction_secrets(next_auth, "durable_secret", [
      "base_secret",
    ])
  assert !list_contains(secrets, "launch_secret_1")
  assert list_contains(secrets, "runtime_secret_1")
  assert !list_contains(secrets, "durable_secret")
}

pub fn managed_auth_falls_back_to_launch_grant_before_ack_within_ttl_test() {
  let auth =
    Some(ui_managed_auth.ManagedLaunchAuth(
      launch_credential: Some("launch_secret_1"),
      launch_expires_at_ms: 100,
      runtime_credential: None,
    ))

  let #(next_auth, credential) =
    ui_managed_auth.credential_for_connect(auth, "durable_secret", 99)

  assert credential == Ok("launch_secret_1")
  let assert Some(ui_managed_auth.ManagedLaunchAuth(
    launch_credential: Some("launch_secret_1"),
    runtime_credential: None,
    ..,
  )) = next_auth
}

pub fn managed_auth_rejects_launch_grant_after_ttl_without_runtime_test() {
  let auth =
    Some(ui_managed_auth.ManagedLaunchAuth(
      launch_credential: Some("launch_secret_1"),
      launch_expires_at_ms: 100,
      runtime_credential: None,
    ))

  let #(next_auth, credential) =
    ui_managed_auth.credential_for_connect(auth, "durable_secret", 100)

  let assert Error(message) = credential
  assert string.contains(message, "runtime credential exchange")
  let assert Some(ui_managed_auth.ManagedLaunchAuth(
    launch_credential: None,
    runtime_credential: None,
    ..,
  )) = next_auth
}

pub fn managed_auth_classifies_auth_rejections_without_treating_5xx_as_permanent_test() {
  assert ui_managed_auth.is_permanent_auth_rejection(
    "websocket_close:1008:managed launch hello mismatch",
  )
  assert ui_managed_auth.is_permanent_auth_rejection(
    "websocket_http_status:401:credential-invalid",
  )
  assert ui_managed_auth.is_permanent_auth_rejection(
    "websocket_http_status:403:api rejected daemon credential",
  )
  assert !ui_managed_auth.is_permanent_auth_rejection("econnrefused")
  assert !ui_managed_auth.is_permanent_auth_rejection(
    "websocket_http_status:503:api restarting",
  )
  assert !ui_managed_auth.is_permanent_auth_rejection(
    "websocket_http_status:503:credential-invalid during api restart",
  )
}

fn list_contains(values: List(String), needle: String) -> Bool {
  case values {
    [] -> False
    [value, ..rest] -> value == needle || list_contains(rest, needle)
  }
}
