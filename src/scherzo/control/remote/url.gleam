import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/uri

pub type ValidationError {
  InvalidUrl
  MissingHost
  UnsupportedScheme
  HttpRequiresLoopback
  InvalidLoopbackHost
  LoopbackUrlWrongHost
}

pub type ValidatedUrl {
  ValidatedUrl(
    base_url: String,
    websocket_url: String,
    host: String,
    scheme: String,
    is_loopback: Bool,
  )
}

pub fn validate_server_url(
  value: String,
  allow_loopback allow_loopback: Bool,
) -> Result(ValidatedUrl, ValidationError) {
  let base_url = trim_trailing_slashes(string.trim(value))
  case uri.parse(base_url) {
    Error(_) -> Error(InvalidUrl)
    Ok(parsed) -> validate_parsed(base_url, parsed, allow_loopback)
  }
}

pub fn error_code(error: ValidationError) -> String {
  case error {
    InvalidUrl -> "invalid_server_url"
    MissingHost -> "server_url_missing_host"
    UnsupportedScheme -> "unsupported_server_url_scheme"
    HttpRequiresLoopback -> "http_requires_loopback"
    InvalidLoopbackHost -> "invalid_loopback_url"
    LoopbackUrlWrongHost -> "loopback_url_wrong_host"
  }
}

pub fn error_message(error: ValidationError) -> String {
  case error {
    InvalidUrl ->
      "server URL must be a valid absolute URL with no query, fragment, or userinfo"
    MissingHost -> "server URL must include a host"
    UnsupportedScheme ->
      "server URL must use https, or http only for loopback development URLs"
    HttpRequiresLoopback -> "http is allowed only for loopback development URLs"
    InvalidLoopbackHost ->
      "0.0.0.0 and unspecified hosts are not valid advertised server URLs"
    LoopbackUrlWrongHost ->
      "loopback URLs only work on the same host as the UI server; use a reachable HTTPS advertised URL instead"
  }
}

fn validate_parsed(
  base_url: String,
  parsed: uri.Uri,
  allow_loopback: Bool,
) -> Result(ValidatedUrl, ValidationError) {
  let uri.Uri(
    scheme: scheme,
    host: host,
    userinfo: userinfo,
    path: path,
    query: query,
    fragment: fragment,
    ..,
  ) = parsed
  case host, scheme, userinfo, query, fragment {
    Some(host), Some(scheme), None, None, None if host != "" -> {
      let is_loopback = loopback_host(host)
      case host == "0.0.0.0" || host == "::" {
        True -> Error(InvalidLoopbackHost)
        False ->
          case scheme {
            "https" ->
              Ok(ValidatedUrl(
                base_url: with_base_path(base_url, path),
                websocket_url: websocket_url("wss", host, parsed.port, path),
                host: host,
                scheme: scheme,
                is_loopback: is_loopback,
              ))
            "http" ->
              case is_loopback {
                False -> Error(HttpRequiresLoopback)
                True ->
                  case allow_loopback {
                    False -> Error(LoopbackUrlWrongHost)
                    True ->
                      Ok(ValidatedUrl(
                        base_url: with_base_path(base_url, path),
                        websocket_url: websocket_url(
                          "ws",
                          host,
                          parsed.port,
                          path,
                        ),
                        host: host,
                        scheme: scheme,
                        is_loopback: True,
                      ))
                  }
              }
            _ -> Error(UnsupportedScheme)
          }
      }
    }
    Some(_), None, _, _, _ -> Error(UnsupportedScheme)
    None, _, _, _, _ -> Error(MissingHost)
    _, _, _, _, _ -> Error(InvalidUrl)
  }
}

fn websocket_url(
  scheme: String,
  host: String,
  port: Option(Int),
  path: String,
) -> String {
  let default_port = case scheme {
    "wss" -> 443
    _ -> 80
  }
  let port_suffix = case port {
    Some(port) if port != default_port -> ":" <> int_to_string(port)
    _ -> ""
  }
  scheme
  <> "://"
  <> host
  <> port_suffix
  <> websocket_path_prefix(path)
  <> "/api/daemons/ws"
}

fn websocket_path_prefix(path: String) -> String {
  case path == "" || path == "/" {
    True -> ""
    False -> trim_trailing_slashes(path)
  }
}

fn with_base_path(base_url: String, path: String) -> String {
  case path == "" || path == "/" {
    True -> base_url
    False -> trim_trailing_slashes(base_url)
  }
}

fn loopback_host(host: String) -> Bool {
  host == "localhost" || host == "127.0.0.1" || host == "::1"
}

fn trim_trailing_slashes(value: String) -> String {
  case
    value != "https://" && value != "http://" && string.ends_with(value, "/")
  {
    True -> trim_trailing_slashes(string.drop_end(value, 1))
    False -> value
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
