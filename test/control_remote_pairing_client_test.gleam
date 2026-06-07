import gleam/erlang/process
import gleam/option.{Some}
import gleam/string
import scherzo/control/remote/pairing_client
import support/remote_ui_test_server
import support/test_helpers
import test_async

fn deps(
  response: Result(pairing_client.HttpResponse, Nil),
) -> pairing_client.Dependencies {
  pairing_client.Dependencies(post_json: fn(_request) { response })
}

pub fn pairing_exchange_success_decodes_durable_credential_test() {
  let assert Ok(success) =
    pairing_client.exchange_pairing_token(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(
        Ok(pairing_client.HttpResponse(
          status: 201,
          body: "{\"credentialId\":\"cred-1\",\"credential\":\"dcred_secret_1\"}",
        )),
      ),
    )
  assert success.server_url == "https://ui.example.test"
  assert success.credential.credential_id == Some("cred-1")
  assert success.credential.secret == "dcred_secret_1"
}

pub fn pairing_exchange_sends_daemon_label_when_provided_test() {
  let requests = process.new_subject()
  let assert Ok(success) =
    pairing_client.exchange_pairing_token_with_label(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      Some("Project Foo / MacBook"),
      False,
      pairing_client.Dependencies(post_json: fn(request) {
        process.send(requests, request)
        Ok(pairing_client.HttpResponse(
          status: 201,
          body: "{\"credentialId\":\"cred-1\",\"credential\":\"dcred_secret_1\"}",
        ))
      }),
    )
  assert success.daemon_id == "daemon_abc"
  let request = test_async.expect_message(requests)
  assert string.contains(request.json_body, "\"pairingToken\":\"pair_token_1\"")
  assert string.contains(request.json_body, "\"daemonId\":\"daemon_abc\"")
  assert string.contains(
    request.json_body,
    "\"daemonLabel\":\"Project Foo / MacBook\"",
  )
}

pub fn pairing_exchange_maps_known_failure_statuses_test() {
  let assert Error(pairing_client.InvalidPairingToken) =
    pairing_client.exchange_pairing_token(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(Ok(pairing_client.HttpResponse(status: 400, body: "{}"))),
    )
  let assert Error(pairing_client.ExpiredPairingToken) =
    pairing_client.exchange_pairing_token(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(Ok(pairing_client.HttpResponse(status: 401, body: "{}"))),
    )
  let assert Error(pairing_client.PairingTokenAlreadyConsumed) =
    pairing_client.exchange_pairing_token(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(Ok(pairing_client.HttpResponse(status: 409, body: "{}"))),
    )
}

pub fn pairing_exchange_reports_unreachable_and_url_errors_test() {
  let assert Error(pairing_client.UnreachableServer) =
    pairing_client.exchange_pairing_token(
      "https://ui.example.test",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(Error(Nil)),
    )
  let assert Error(pairing_client.InvalidUrl(_)) =
    pairing_client.exchange_pairing_token(
      "http://127.0.0.1:3000",
      "pair_token_1",
      "daemon_abc",
      False,
      deps(Error(Nil)),
    )
}

pub fn pairing_exchange_default_dependencies_posts_to_fake_ui_server_test() {
  let root = "test/tmp/pairing-client-default-dependencies"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("dcred_secret_1", transcript_path)
  let assert Ok(success) =
    pairing_client.exchange_pairing_token(
      remote_ui_test_server.server_url(server),
      "pair_token_1",
      "daemon_abc",
      True,
      pairing_client.default_dependencies(),
    )
  assert success.server_url == remote_ui_test_server.server_url(server)
  assert success.credential.credential_id == Some("cred-1")
  assert success.credential.secret == "dcred_secret_1"
  let transcript =
    remote_ui_test_server.wait_for_contains(
      transcript_path,
      "pairing_exchange_body=",
      50,
    )
  assert transcript |> string.contains("\"pairingToken\":\"pair_token_1\"")
  assert transcript |> string.contains("\"daemonId\":\"daemon_abc\"")
  remote_ui_test_server.stop(server)
}
