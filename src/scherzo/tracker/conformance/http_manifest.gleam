import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri
import scherzo/tracker/conformance/types

pub fn validate_endpoint(
  endpoint: types.HttpEndpointConfig,
) -> Result(Nil, types.ManifestError) {
  let types.HttpEndpointConfig(url: url, headers: headers, retry: retry) =
    endpoint

  use Nil <- result.try(validate_url(url))
  use Nil <- result.try(validate_headers(headers))
  validate_retry(retry)
}

pub fn endpoint_to_json(endpoint: types.HttpEndpointConfig) -> json.Json {
  let types.HttpEndpointConfig(url: url, headers: headers, retry: retry) =
    endpoint
  json.object([
    #("url", json.string(url)),
    #("headers", json.array(headers, of: header_to_json)),
    #("retry", retry_to_json(retry)),
  ])
}

pub fn endpoint_decoder() -> decode.Decoder(types.HttpEndpointConfig) {
  use url <- decode.field("url", decode.string)
  use headers <- decode.optional_field(
    "headers",
    [],
    decode.list(header_decoder()),
  )
  use retry <- decode.optional_field(
    "retry",
    types.HttpRetryConfig(max_attempts: 1, backoff_ms: 0),
    retry_decoder(),
  )
  decode.success(types.HttpEndpointConfig(
    url: url,
    headers: headers,
    retry: retry,
  ))
}

fn validate_url(url: String) -> Result(Nil, types.ManifestError) {
  case uri.parse(url) {
    Error(_) -> Error(invalid_http_url_error())
    Ok(parsed) -> {
      let uri.Uri(
        scheme: scheme,
        userinfo: userinfo,
        host: host,
        fragment: fragment,
        ..,
      ) = parsed
      case scheme, userinfo, host, fragment {
        Some("http"), None, Some(host), None if host != "" -> Ok(Nil)
        Some("https"), None, Some(host), None if host != "" -> Ok(Nil)
        _, _, _, _ -> Error(invalid_http_url_error())
      }
    }
  }
}

fn validate_headers(
  headers: List(types.HttpHeaderConfig),
) -> Result(Nil, types.ManifestError) {
  case headers {
    [] -> Ok(Nil)
    [header, ..rest] -> {
      use Nil <- result.try(validate_header(header))
      validate_headers(rest)
    }
  }
}

fn validate_header(
  header: types.HttpHeaderConfig,
) -> Result(Nil, types.ManifestError) {
  let types.HttpHeaderConfig(name: name, value_from_env: value_from_env, ..) =
    header

  use Nil <- result.try(case valid_header_name(name) {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "invalid_http_header_name",
        "driver.endpoint.headers[].name must be non-empty and must not contain colon, carriage return, or newline",
      ))
  })

  case value_from_env != "" {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "invalid_http_header_env",
        "driver.endpoint.headers[].value_from_env must be non-empty",
      ))
  }
}

fn validate_retry(
  retry: types.HttpRetryConfig,
) -> Result(Nil, types.ManifestError) {
  let types.HttpRetryConfig(max_attempts: max_attempts, backoff_ms: backoff_ms) =
    retry
  case
    max_attempts >= 1
    && max_attempts <= types.max_http_retry_attempts
    && backoff_ms >= 0
    && backoff_ms <= types.max_http_retry_backoff_ms
  {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "invalid_http_retry",
        "driver.endpoint.retry.max_attempts must be between 1 and "
          <> int.to_string(types.max_http_retry_attempts)
          <> ", and driver.endpoint.retry.backoff_ms must be between 0 and "
          <> int.to_string(types.max_http_retry_backoff_ms),
      ))
  }
}

fn header_to_json(header: types.HttpHeaderConfig) -> json.Json {
  let types.HttpHeaderConfig(
    name: name,
    value_from_env: value_from_env,
    value_prefix: value_prefix,
  ) = header
  json.object([
    #("name", json.string(name)),
    #("value_from_env", json.string(value_from_env)),
    #("value_prefix", json.string(value_prefix)),
  ])
}

fn header_decoder() -> decode.Decoder(types.HttpHeaderConfig) {
  use name <- decode.field("name", decode.string)
  use value_from_env <- decode.field("value_from_env", decode.string)
  use value_prefix <- decode.optional_field("value_prefix", "", decode.string)
  decode.success(types.HttpHeaderConfig(
    name: name,
    value_from_env: value_from_env,
    value_prefix: value_prefix,
  ))
}

fn retry_to_json(retry: types.HttpRetryConfig) -> json.Json {
  let types.HttpRetryConfig(max_attempts: max_attempts, backoff_ms: backoff_ms) =
    retry
  json.object([
    #("max_attempts", json.int(max_attempts)),
    #("backoff_ms", json.int(backoff_ms)),
  ])
}

fn retry_decoder() -> decode.Decoder(types.HttpRetryConfig) {
  use max_attempts <- decode.optional_field("max_attempts", 1, decode.int)
  use backoff_ms <- decode.optional_field("backoff_ms", 0, decode.int)
  decode.success(types.HttpRetryConfig(
    max_attempts: max_attempts,
    backoff_ms: backoff_ms,
  ))
}

fn valid_header_name(value: String) -> Bool {
  value != ""
  && !string.contains(value, ":")
  && !string.contains(value, "\r")
  && !string.contains(value, "\n")
}

fn invalid_http_url_error() -> types.ManifestError {
  types.ManifestError(
    "invalid_http_url",
    "driver.endpoint.url must be an absolute http or https URL with a host and no userinfo or fragment",
  )
}
