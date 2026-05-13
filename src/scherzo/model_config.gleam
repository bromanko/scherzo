import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import yay

pub type ThinkingLevel {
  ThinkingOff
  ThinkingMinimal
  ThinkingLow
  ThinkingMedium
  ThinkingHigh
  ThinkingXHigh
}

pub type Settings {
  Settings(model: Option(String), thinking: Option(ThinkingLevel))
}

pub type SettingsPaths {
  SettingsPaths(
    provider_path: String,
    provider_model_path: String,
    model_path: String,
    thinking_path: String,
  )
}

pub type ModelError {
  ModelError(code: String, message: String)
}

pub fn default_settings() -> Settings {
  Settings(model: None, thinking: None)
}

pub fn read_settings(
  node: yay.Node,
  paths: SettingsPaths,
  error: fn(String, String) -> e,
) -> Result(Settings, e) {
  use _ <- result.try(reject_provider_field(node, paths, error))
  use model <- result.try(read_optional_model(node, paths.model_path, error))
  use thinking <- result.try(read_optional_thinking(
    node,
    paths.thinking_path,
    error,
  ))
  Ok(Settings(model: model, thinking: thinking))
}

pub fn resolve(defaults: Settings, overrides: Settings) -> Settings {
  Settings(
    model: option.or(overrides.model, defaults.model),
    thinking: option.or(overrides.thinking, defaults.thinking),
  )
}

pub fn parse_model(value: String, path: String) -> Result(String, ModelError) {
  let value = string.trim(value)
  case value == "" {
    True ->
      Error(ModelError(
        "invalid_model",
        path <> " must be a non-empty model/provider selection",
      ))
    False ->
      case has_whitespace(value) {
        True ->
          Error(ModelError(
            "invalid_model",
            path <> " must not contain whitespace",
          ))
        False ->
          case has_thinking_suffix(value) {
            True ->
              Error(ModelError(
                "invalid_model",
                path
                  <> " must not include a :thinking suffix; set thinking separately",
              ))
            False -> Ok(value)
          }
      }
  }
}

pub fn parse_thinking(
  value: String,
  path: String,
) -> Result(ThinkingLevel, ModelError) {
  let normalized = value |> string.trim |> string.lowercase
  case normalized {
    "off" -> Ok(ThinkingOff)
    "minimal" -> Ok(ThinkingMinimal)
    "low" -> Ok(ThinkingLow)
    "medium" -> Ok(ThinkingMedium)
    "high" -> Ok(ThinkingHigh)
    "xhigh" -> Ok(ThinkingXHigh)
    _ ->
      Error(ModelError(
        "invalid_thinking",
        path <> " must be one of: off, minimal, low, medium, high, xhigh",
      ))
  }
}

pub fn thinking_to_string(level: ThinkingLevel) -> String {
  case level {
    ThinkingOff -> "off"
    ThinkingMinimal -> "minimal"
    ThinkingLow -> "low"
    ThinkingMedium -> "medium"
    ThinkingHigh -> "high"
    ThinkingXHigh -> "xhigh"
  }
}

pub fn validate_resolved(
  settings: Settings,
  path: String,
) -> Result(Nil, ModelError) {
  case settings.model, settings.thinking {
    Some(model), Some(level) -> validate_model_thinking(model, level, path)
    _, _ -> Ok(Nil)
  }
}

pub fn apply_to_command(command: String, settings: Settings) -> String {
  command <> model_flag(settings.model) <> thinking_flag(settings.thinking)
}

pub fn apply_to_argv_args(
  args: List(String),
  settings: Settings,
) -> List(String) {
  let args = case settings.model {
    Some(model) -> list.append(args, ["--model", model])
    None -> args
  }
  case settings.thinking {
    Some(level) -> list.append(args, ["--thinking", thinking_to_string(level)])
    None -> args
  }
}

pub fn error_code(error: ModelError) -> String {
  let ModelError(code: code, ..) = error
  code
}

pub fn error_message(error: ModelError) -> String {
  let ModelError(message: message, ..) = error
  message
}

fn validate_model_thinking(
  model: String,
  level: ThinkingLevel,
  path: String,
) -> Result(Nil, ModelError) {
  case level {
    ThinkingOff -> Ok(Nil)
    _ ->
      case known_without_thinking(model) {
        True ->
          Error(ModelError(
            "invalid_model_thinking",
            path
              <> " selects model "
              <> model
              <> " with thinking="
              <> thinking_to_string(level)
              <> ", but that model is not known to support thinking; use thinking: off or choose a thinking-capable model",
          ))
        False -> Ok(Nil)
      }
  }
}

fn known_without_thinking(model: String) -> Bool {
  let model = model |> string.trim |> string.lowercase
  list.contains(known_models_without_thinking(), model)
}

fn known_models_without_thinking() -> List(String) {
  [
    "gpt-4o",
    "gpt-4o-mini",
    "gpt-4o-2024-05-13",
    "gpt-4o-2024-08-06",
    "gpt-4o-2024-11-20",
    "gpt-4o-mini-2024-07-18",
    "gpt-4.1",
    "gpt-4.1-mini",
    "gpt-4.1-nano",
    "gpt-4.1-2025-04-14",
    "gpt-4.1-mini-2025-04-14",
    "gpt-4.1-nano-2025-04-14",
    "gemini-1.5-pro",
    "gemini-1.5-pro-001",
    "gemini-1.5-pro-002",
    "gemini-1.5-flash",
    "gemini-1.5-flash-001",
    "gemini-1.5-flash-002",
    "gemini-1.5-flash-8b",
    "gemini-1.5-flash-8b-001",
    "gemini-2.0-flash",
    "gemini-2.0-flash-001",
    "gemini-2.0-flash-lite",
    "gemini-2.0-flash-lite-001",
    "gemma-3",
    "gemma-3-1b-it",
    "gemma-3-4b-it",
    "gemma-3-12b-it",
    "gemma-3-27b-it",
    "gemma-3n-e2b-it",
    "gemma-3n-e4b-it",
    "openai/gpt-4o",
    "openai/gpt-4o-mini",
    "openai/gpt-4o-2024-05-13",
    "openai/gpt-4o-2024-08-06",
    "openai/gpt-4o-2024-11-20",
    "openai/gpt-4o-mini-2024-07-18",
    "openai/gpt-4.1",
    "openai/gpt-4.1-mini",
    "openai/gpt-4.1-nano",
    "openai/gpt-4.1-2025-04-14",
    "openai/gpt-4.1-mini-2025-04-14",
    "openai/gpt-4.1-nano-2025-04-14",
    "github-copilot/gpt-4o",
    "github-copilot/gpt-4o-mini",
    "github-copilot/gpt-4.1",
    "github-copilot/gpt-4.1-mini",
    "github-copilot/gpt-4.1-nano",
    "google/gemini-1.5-pro",
    "google/gemini-1.5-pro-001",
    "google/gemini-1.5-pro-002",
    "google/gemini-1.5-flash",
    "google/gemini-1.5-flash-001",
    "google/gemini-1.5-flash-002",
    "google/gemini-1.5-flash-8b",
    "google/gemini-1.5-flash-8b-001",
    "google/gemini-2.0-flash",
    "google/gemini-2.0-flash-001",
    "google/gemini-2.0-flash-lite",
    "google/gemini-2.0-flash-lite-001",
    "google/gemma-3",
    "google/gemma-3-1b-it",
    "google/gemma-3-4b-it",
    "google/gemma-3-12b-it",
    "google/gemma-3-27b-it",
    "google/gemma-3n-e2b-it",
    "google/gemma-3n-e4b-it",
  ]
}

fn reject_provider_field(
  node: yay.Node,
  paths: SettingsPaths,
  error: fn(String, String) -> e,
) -> Result(Nil, e) {
  case get_node(node, "provider") {
    None -> Ok(Nil)
    Some(_) ->
      Error(error(
        "unsupported_provider_field",
        paths.provider_path
          <> " is not supported; include the provider in "
          <> paths.provider_model_path
          <> ", for example openai/gpt-5",
      ))
  }
}

fn read_optional_model(
  node: yay.Node,
  path: String,
  error: fn(String, String) -> e,
) -> Result(Option(String), e) {
  case get_node(node, "model") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) ->
      case parse_model(value, path) {
        Ok(model) -> Ok(Some(model))
        Error(err) -> Error(error(error_code(err), error_message(err)))
      }
    Some(_) -> Error(error("invalid_model", path <> " must be a string"))
  }
}

fn read_optional_thinking(
  node: yay.Node,
  path: String,
  error: fn(String, String) -> e,
) -> Result(Option(ThinkingLevel), e) {
  case get_node(node, "thinking") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) ->
      case parse_thinking(value, path) {
        Ok(thinking) -> Ok(Some(thinking))
        Error(err) -> Error(error(error_code(err), error_message(err)))
      }
    Some(_) -> Error(error("invalid_thinking", path <> " must be a string"))
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(_) -> None
      }
    _ -> None
  }
}

fn model_flag(model: Option(String)) -> String {
  case model {
    Some(model) -> " --model " <> shell_quote(model)
    None -> ""
  }
}

fn thinking_flag(thinking: Option(ThinkingLevel)) -> String {
  case thinking {
    Some(level) -> " --thinking " <> shell_quote(thinking_to_string(level))
    None -> ""
  }
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn has_whitespace(value: String) -> Bool {
  string.contains(value, " ")
  || string.contains(value, "\t")
  || string.contains(value, "\n")
  || string.contains(value, "\r")
}

fn has_thinking_suffix(value: String) -> Bool {
  let value = value |> string.trim |> string.lowercase
  string.ends_with(value, ":off")
  || string.ends_with(value, ":minimal")
  || string.ends_with(value, ":low")
  || string.ends_with(value, ":medium")
  || string.ends_with(value, ":high")
  || string.ends_with(value, ":xhigh")
}
