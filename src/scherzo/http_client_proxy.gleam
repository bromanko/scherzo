import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import scherzo/path

pub type ProxyKind {
  HttpProxy
  HttpsProxy
}

pub type ProxySettings {
  ProxySettings(
    kind: ProxyKind,
    host: String,
    port: Int,
    no_proxy: List(String),
  )
}

pub type ProxyError {
  InvalidProxyUrl(ProxyKind)
  UnsupportedProxyScheme(ProxyKind)
  ProxyCredentialsUnsupported(ProxyKind)
  ProxyConfigurationFailed(ProxyKind)
}

pub fn configure_from_environment() -> Result(Nil, ProxyError) {
  ensure_http_client_started()
  use settings <- result.try(settings_from_environment(path.env))
  configure_all(settings)
}

pub fn settings_from_environment(
  env_reader: fn(String) -> Option(String),
) -> Result(List(ProxySettings), ProxyError) {
  let no_proxy =
    first_environment_value(["no_proxy", "NO_PROXY"], env_reader)
    |> parse_no_proxy
  use http_proxy <- result.try(proxy_from_environment(
    HttpProxy,
    ["http_proxy", "HTTP_PROXY"],
    no_proxy,
    env_reader,
  ))
  use https_proxy <- result.try(proxy_from_environment(
    HttpsProxy,
    ["https_proxy", "HTTPS_PROXY"],
    no_proxy,
    env_reader,
  ))
  Ok(list.append(optional_settings(http_proxy), optional_settings(https_proxy)))
}

pub fn error_message(error: ProxyError) -> String {
  case error {
    InvalidProxyUrl(kind) -> proxy_name(kind) <> " is not a valid proxy URL"
    UnsupportedProxyScheme(kind) ->
      proxy_name(kind) <> " must use an http:// proxy URL"
    ProxyCredentialsUnsupported(kind) ->
      proxy_name(kind) <> " must not contain credentials"
    ProxyConfigurationFailed(kind) ->
      "failed to configure Erlang httpc from " <> proxy_name(kind)
  }
}

fn proxy_from_environment(
  kind: ProxyKind,
  names: List(String),
  no_proxy: List(String),
  env_reader: fn(String) -> Option(String),
) -> Result(Option(ProxySettings), ProxyError) {
  case first_environment_value(names, env_reader) {
    None -> Ok(None)
    Some(value) -> parse_proxy_url(kind, value, no_proxy) |> result.map(Some)
  }
}

fn parse_proxy_url(
  kind: ProxyKind,
  value: String,
  no_proxy: List(String),
) -> Result(ProxySettings, ProxyError) {
  case uri.parse(value) {
    Error(Nil) -> Error(InvalidProxyUrl(kind))
    Ok(Uri(scheme:, userinfo:, host:, port:, path:, query:, fragment:)) -> {
      use _ <- result.try(validate_scheme(kind, scheme))
      use _ <- result.try(validate_credentials(kind, userinfo))
      case host, port, path, query, fragment {
        Some(host), Some(port), "", None, None
          if host != "" && port > 0 && port <= 65_535
        -> Ok(ProxySettings(kind:, host:, port:, no_proxy:))
        Some(host), Some(port), "/", None, None
          if host != "" && port > 0 && port <= 65_535
        -> Ok(ProxySettings(kind:, host:, port:, no_proxy:))
        Some(host), None, "", None, None if host != "" ->
          Ok(ProxySettings(kind:, host:, port: 80, no_proxy:))
        Some(host), None, "/", None, None if host != "" ->
          Ok(ProxySettings(kind:, host:, port: 80, no_proxy:))
        _, _, _, _, _ -> Error(InvalidProxyUrl(kind))
      }
    }
  }
}

fn validate_scheme(
  kind: ProxyKind,
  scheme: Option(String),
) -> Result(Nil, ProxyError) {
  case scheme {
    Some("http") -> Ok(Nil)
    _ -> Error(UnsupportedProxyScheme(kind))
  }
}

fn validate_credentials(
  kind: ProxyKind,
  userinfo: Option(String),
) -> Result(Nil, ProxyError) {
  case userinfo {
    None -> Ok(Nil)
    Some(_) -> Error(ProxyCredentialsUnsupported(kind))
  }
}

fn parse_no_proxy(value: Option(String)) -> List(String) {
  case value {
    None -> []
    Some(value) ->
      value
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(value) { value != "" })
  }
}

fn first_environment_value(
  names: List(String),
  env_reader: fn(String) -> Option(String),
) -> Option(String) {
  case names {
    [] -> None
    [name, ..rest] ->
      case env_reader(name) {
        Some(value) -> {
          let value = string.trim(value)
          case value {
            "" -> first_environment_value(rest, env_reader)
            value -> Some(value)
          }
        }
        None -> first_environment_value(rest, env_reader)
      }
  }
}

fn optional_settings(settings: Option(ProxySettings)) -> List(ProxySettings) {
  case settings {
    Some(settings) -> [settings]
    None -> []
  }
}

fn configure_all(settings: List(ProxySettings)) -> Result(Nil, ProxyError) {
  list.try_each(settings, fn(settings) {
    set_proxy(
      proxy_kind_name(settings.kind),
      settings.host,
      settings.port,
      settings.no_proxy,
    )
    |> result.replace_error(ProxyConfigurationFailed(settings.kind))
  })
}

fn proxy_kind_name(kind: ProxyKind) -> String {
  case kind {
    HttpProxy -> "http"
    HttpsProxy -> "https"
  }
}

fn proxy_name(kind: ProxyKind) -> String {
  case kind {
    HttpProxy -> "http_proxy"
    HttpsProxy -> "https_proxy"
  }
}

@external(erlang, "scherzo_http_client_ffi", "ensure_started")
fn ensure_http_client_started() -> Nil

@external(erlang, "scherzo_http_client_ffi", "set_proxy")
fn set_proxy(
  kind: String,
  host: String,
  port: Int,
  no_proxy: List(String),
) -> Result(Nil, Nil)
