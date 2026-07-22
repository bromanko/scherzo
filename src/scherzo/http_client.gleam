import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/path

pub type ConfigureError {
  ConfigureError(message: String)
}

pub fn error_message(error: ConfigureError) -> String {
  let ConfigureError(message) = error
  message
}

/// Configure the shared OTP HTTP client from the process environment.
///
/// Lowercase proxy variables take precedence over their uppercase variants.
/// Empty values are treated as unset.
pub fn configure() -> Result(Nil, ConfigureError) {
  configure_with_env(path.env)
}

pub fn configure_with_env(
  env: fn(String) -> Option(String),
) -> Result(Nil, ConfigureError) {
  configure_values(
    preferred_env_value(env, "http_proxy", "HTTP_PROXY"),
    preferred_env_value(env, "https_proxy", "HTTPS_PROXY"),
    preferred_env_value(env, "no_proxy", "NO_PROXY"),
  )
}

pub fn configure_values(
  http_proxy: Option(String),
  https_proxy: Option(String),
  no_proxy: Option(String),
) -> Result(Nil, ConfigureError) {
  configure_ffi(
    option_value(http_proxy),
    option_value(https_proxy),
    option_value(no_proxy),
  )
}

fn preferred_env_value(
  env: fn(String) -> Option(String),
  lowercase_name: String,
  uppercase_name: String,
) -> Option(String) {
  case non_empty(env(lowercase_name)) {
    Some(value) -> Some(value)
    None -> non_empty(env(uppercase_name))
  }
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> None
        False -> Some(value)
      }
    }
    None -> None
  }
}

fn option_value(value: Option(String)) -> String {
  case non_empty(value) {
    Some(value) -> value
    None -> ""
  }
}

@external(erlang, "scherzo_http_client_ffi", "configure")
fn configure_ffi(
  http_proxy: String,
  https_proxy: String,
  no_proxy: String,
) -> Result(Nil, ConfigureError)
