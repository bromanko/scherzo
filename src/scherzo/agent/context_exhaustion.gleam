import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/error
import scherzo/json_value.{type JsonValue}
import scherzo/pi/protocol

pub type ContextExhaustion {
  ContextExhaustion(
    provider: Option(String),
    provider_code: Option(String),
    message: String,
  )
}

pub fn from_provider_error(
  provider: Option(String),
  code: Option(String),
  message: String,
) -> Option(ContextExhaustion) {
  let code = normalize_option(code)
  let message = string.trim(message)
  case code_is_context_exhaustion(code), code_is_known_non_context(code) {
    True, _ -> Some(ContextExhaustion(provider:, provider_code: code, message:))
    False, True -> None
    False, False ->
      case message_is_context_exhaustion(message) {
        True ->
          Some(ContextExhaustion(provider:, provider_code: code, message:))
        False -> None
      }
  }
}

pub fn from_rpc_record(
  record: protocol.RpcRecord,
) -> Option(ContextExhaustion) {
  let raw = json_value.parse(record.raw_json) |> result_to_option
  let provider =
    first_non_empty([
      raw |> option_then(json_string_at(["message", "provider"])),
      raw |> option_then(json_string_at(["provider"])),
      raw |> option_then(json_string_at(["data", "provider"])),
    ])
  let raw_code =
    first_non_empty([
      raw |> option_then(json_string_at(["message", "error", "code"])),
      raw |> option_then(json_string_at(["error", "code"])),
      raw |> option_then(json_string_at(["data", "error", "code"])),
      raw |> option_then(json_string_at(["code"])),
    ])
  let raw_message =
    first_non_empty([
      record.error_message,
      raw |> option_then(json_string_at(["message", "errorMessage"])),
      raw |> option_then(json_string_at(["message", "error_message"])),
      raw |> option_then(json_string_at(["errorMessage"])),
      raw |> option_then(json_string_at(["error_message"])),
      raw |> option_then(json_string_at(["message"])),
      raw |> option_then(json_string_at(["error", "message"])),
      raw |> option_then(json_string_at(["data", "error", "message"])),
    ])
  let embedded = raw_message |> option_then(extract_embedded_provider_error)
  let provider =
    first_non_empty([provider, embedded |> option_then(fn(e) { e.provider })])
  let code =
    first_non_empty([raw_code, embedded |> option_then(fn(e) { e.code })])
  let message =
    first_non_empty([
      embedded |> option_map(fn(e) { e.message }),
      raw_message,
    ])
  case message {
    None -> None
    Some(message) ->
      case
        record_is_error(record)
        || code_is_context_exhaustion(normalize_option(code))
      {
        True -> from_provider_error(provider, code, message)
        False -> None
      }
  }
}

pub fn from_pi_rpc_error(err: error.PiRpcError) -> Option(ContextExhaustion) {
  case err {
    error.PiContextWindowExhausted(provider, provider_code, detail) ->
      Some(ContextExhaustion(
        provider: provider,
        provider_code: provider_code,
        message: detail,
      ))
    error.PiProtocolError(message) -> from_provider_error(None, None, message)
    _ -> None
  }
}

pub fn to_pi_rpc_error(context: ContextExhaustion) -> error.PiRpcError {
  error.PiContextWindowExhausted(
    context.provider,
    context.provider_code,
    context.message,
  )
}

fn record_is_error(record: protocol.RpcRecord) -> Bool {
  case record.stop_reason {
    Some(reason) -> string.trim(reason) |> string.lowercase == "error"
    None ->
      case record.success {
        Some(False) -> True
        _ -> False
      }
  }
}

fn code_is_context_exhaustion(code: Option(String)) -> Bool {
  case code {
    None -> False
    Some(code) -> {
      let code = normalize(code)
      list.contains(
        [
          "context_length_exceeded",
          "context_window_exceeded",
          "context_limit_exceeded",
          "max_context_length_exceeded",
          "maximum_context_length_exceeded",
          "prompt_too_long",
          "input_too_long",
          "input_length_exceeded",
          "too_many_input_tokens",
        ],
        code,
      )
      || string.contains(code, "context_length")
      || string.contains(code, "context_window")
    }
  }
}

fn code_is_known_non_context(code: Option(String)) -> Bool {
  case code {
    None -> False
    Some(code) -> {
      let code = normalize(code)
      list.contains(
        [
          "rate_limit_exceeded",
          "authentication_error",
          "invalid_api_key",
          "insufficient_quota",
          "quota_exceeded",
          "timeout",
          "server_error",
        ],
        code,
      )
    }
  }
}

fn message_is_context_exhaustion(message: String) -> Bool {
  let text = normalize(message)
  let context_phrase =
    string.contains(text, "context window")
    || string.contains(text, "context length")
    || string.contains(text, "maximum context")
    || string.contains(text, "max context")
    || string.contains(text, "model context")
  let prompt_phrase =
    string.contains(text, "prompt is too long")
    || string.contains(text, "prompt too long")
    || string.contains(text, "prompt length")
  let token_phrase =
    string.contains(text, "too many input tokens")
    || string.contains(text, "input token")
    || string.contains(text, "input exceeds")
  context_phrase || prompt_phrase || token_phrase
}

fn normalize(value: String) -> String {
  value |> string.trim |> string.lowercase
}

fn normalize_option(value: Option(String)) -> Option(String) {
  case value {
    Some(text) ->
      case string.trim(text) == "" {
        True -> None
        False -> Some(normalize(text))
      }
    None -> None
  }
}

type EmbeddedProviderError {
  EmbeddedProviderError(
    provider: Option(String),
    code: Option(String),
    message: String,
  )
}

fn extract_embedded_provider_error(
  message: String,
) -> Option(EmbeddedProviderError) {
  case string.split_once(message, on: "{") |> result_to_option {
    None -> None
    Some(#(_, json_suffix)) -> {
      let candidate = "{" <> json_suffix
      case json_value.parse(candidate) |> result_to_option {
        None -> None
        Some(value) -> {
          let provider =
            first_non_empty([
              json_string_at(["provider"])(value),
              json_string_at(["api"])(value),
            ])
          let code =
            first_non_empty([
              json_string_at(["error", "code"])(value),
              json_string_at(["code"])(value),
            ])
          let nested_message =
            first_non_empty([
              json_string_at(["error", "message"])(value),
              json_string_at(["message"])(value),
            ])
          case nested_message {
            Some(nested_message) ->
              Some(EmbeddedProviderError(
                provider:,
                code:,
                message: nested_message,
              ))
            None -> None
          }
        }
      }
    }
  }
}

fn json_string_at(path: List(String)) -> fn(JsonValue) -> Option(String) {
  fn(value) { json_string_at_path(value, path) }
}

fn json_string_at_path(value: JsonValue, path: List(String)) -> Option(String) {
  case path, value {
    [], json_value.JString(text) -> Some(text)
    [], _ -> None
    [key, ..rest], json_value.JObject(entries) ->
      case object_get(entries, key) {
        Some(child) -> json_string_at_path(child, rest)
        None -> None
      }
    _, _ -> None
  }
}

fn object_get(
  entries: List(#(String, JsonValue)),
  key: String,
) -> Option(JsonValue) {
  case entries {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Some(value)
        False -> object_get(rest, key)
      }
  }
}

fn first_non_empty(values: List(Option(String))) -> Option(String) {
  case values {
    [] -> None
    [value, ..rest] ->
      case value {
        Some(text) ->
          case string.trim(text) == "" {
            True -> first_non_empty(rest)
            False -> Some(text)
          }
        None -> first_non_empty(rest)
      }
  }
}

fn option_then(value: Option(a), next: fn(a) -> Option(b)) -> Option(b) {
  case value {
    Some(value) -> next(value)
    None -> None
  }
}

fn option_map(value: Option(a), next: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(next(value))
    None -> None
  }
}

fn result_to_option(value: Result(a, b)) -> Option(a) {
  case value |> result.map(Some) |> result.replace_error(None) {
    Ok(value) -> value
    Error(fallback) -> fallback
  }
}
