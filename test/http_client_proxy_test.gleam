import gleam/option.{type Option, None, Some}
import scherzo/http_client_proxy

pub fn lowercase_proxy_environment_is_parsed_test() {
  let env =
    environment([
      #("http_proxy", "http://127.0.0.1:17328"),
      #("https_proxy", "http://127.0.0.1:17329/"),
      #("no_proxy", " localhost, 127.0.0.1 ,, .internal "),
    ])

  let assert Ok([
    http_client_proxy.ProxySettings(
      http_client_proxy.HttpProxy,
      "127.0.0.1",
      17_328,
      ["localhost", "127.0.0.1", ".internal"],
    ),
    http_client_proxy.ProxySettings(
      http_client_proxy.HttpsProxy,
      "127.0.0.1",
      17_329,
      ["localhost", "127.0.0.1", ".internal"],
    ),
  ]) = http_client_proxy.settings_from_environment(env)
}

pub fn uppercase_proxy_environment_is_used_as_fallback_test() {
  let env = environment([#("HTTPS_PROXY", "http://proxy.example:8080")])

  let assert Ok([
    http_client_proxy.ProxySettings(
      http_client_proxy.HttpsProxy,
      "proxy.example",
      8080,
      [],
    ),
  ]) = http_client_proxy.settings_from_environment(env)
}

pub fn lowercase_proxy_environment_takes_precedence_test() {
  let env =
    environment([
      #("https_proxy", "http://lower.example:8000"),
      #("HTTPS_PROXY", "http://upper.example:9000"),
    ])

  let assert Ok([
    http_client_proxy.ProxySettings(
      http_client_proxy.HttpsProxy,
      "lower.example",
      8000,
      [],
    ),
  ]) = http_client_proxy.settings_from_environment(env)
}

pub fn proxy_default_port_is_eighty_test() {
  let env = environment([#("https_proxy", "http://proxy.example")])

  let assert Ok([
    http_client_proxy.ProxySettings(
      http_client_proxy.HttpsProxy,
      "proxy.example",
      80,
      [],
    ),
  ]) = http_client_proxy.settings_from_environment(env)
}

pub fn unsupported_proxy_scheme_fails_without_echoing_value_test() {
  let env = environment([#("https_proxy", "https://sensitive.example:443")])

  let assert Error(http_client_proxy.UnsupportedProxyScheme(
    http_client_proxy.HttpsProxy,
  )) = http_client_proxy.settings_from_environment(env)
  let error =
    http_client_proxy.UnsupportedProxyScheme(http_client_proxy.HttpsProxy)
  assert http_client_proxy.error_message(error)
    == "https_proxy must use an http:// proxy URL"
}

pub fn proxy_credentials_are_rejected_without_echoing_value_test() {
  let env =
    environment([#("https_proxy", "http://user:secret@proxy.example:8080")])

  let assert Error(http_client_proxy.ProxyCredentialsUnsupported(
    http_client_proxy.HttpsProxy,
  )) = http_client_proxy.settings_from_environment(env)
  let error =
    http_client_proxy.ProxyCredentialsUnsupported(http_client_proxy.HttpsProxy)
  assert http_client_proxy.error_message(error)
    == "https_proxy must not contain credentials"
}

pub fn malformed_proxy_url_is_rejected_test() {
  let env = environment([#("https_proxy", "not a proxy URL")])

  let assert Error(http_client_proxy.InvalidProxyUrl(
    http_client_proxy.HttpsProxy,
  )) = http_client_proxy.settings_from_environment(env)
}

fn environment(
  entries: List(#(String, String)),
) -> fn(String) -> Option(String) {
  fn(name) { lookup(entries, name) }
}

fn lookup(entries: List(#(String, String)), name: String) -> Option(String) {
  case entries {
    [] -> None
    [#(candidate, value), ..] if candidate == name -> Some(value)
    [_, ..rest] -> lookup(rest, name)
  }
}
