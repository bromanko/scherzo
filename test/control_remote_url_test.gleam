import scherzo/control/remote/url

pub fn validates_https_url_and_derives_websocket_url_test() {
  let assert Ok(validated) =
    url.validate_server_url("https://ui.example.test", allow_loopback: False)
  assert validated.base_url == "https://ui.example.test"
  assert validated.websocket_url == "wss://ui.example.test/api/daemons/ws"
  assert validated.is_loopback == False
}

pub fn rejects_zero_host_and_non_loopback_http_test() {
  let assert Error(url.InvalidLoopbackHost) =
    url.validate_server_url("http://0.0.0.0:3000", allow_loopback: True)
  let assert Error(url.HttpRequiresLoopback) =
    url.validate_server_url("http://ui.example.test:3000", allow_loopback: True)
}

pub fn loopback_requires_explicit_opt_in_test() {
  let assert Error(url.LoopbackUrlWrongHost) =
    url.validate_server_url("http://127.0.0.1:4000", allow_loopback: False)
  let assert Ok(validated) =
    url.validate_server_url("http://127.0.0.1:4000", allow_loopback: True)
  assert validated.websocket_url == "ws://127.0.0.1:4000/api/daemons/ws"
}

pub fn path_prefixed_urls_keep_pairing_and_websocket_under_prefix_test() {
  let assert Ok(validated) =
    url.validate_server_url(
      "https://ui.example.test/scherzo/",
      allow_loopback: False,
    )
  assert validated.base_url == "https://ui.example.test/scherzo"
  assert validated.websocket_url
    == "wss://ui.example.test/scherzo/api/daemons/ws"

  let assert Ok(loopback) =
    url.validate_server_url(
      "http://127.0.0.1:4000/dev-ui/",
      allow_loopback: True,
    )
  assert loopback.base_url == "http://127.0.0.1:4000/dev-ui"
  assert loopback.websocket_url == "ws://127.0.0.1:4000/dev-ui/api/daemons/ws"
}
