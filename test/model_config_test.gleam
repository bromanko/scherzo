import gleam/option.{None, Some}
import scherzo/model_config

pub fn validate_resolved_rejects_exact_known_non_thinking_model_test() {
  let settings =
    model_config.Settings(
      model: Some("openai/gpt-4o"),
      thinking: Some(model_config.ThinkingHigh),
    )
  let assert Error(error) = model_config.validate_resolved(settings, "pi")
  assert model_config.error_code(error) == "invalid_model_thinking"
}

pub fn validate_resolved_allows_models_that_only_contain_non_thinking_names_test() {
  let settings =
    model_config.Settings(
      model: Some("acme/my-gpt-4o-reasoner"),
      thinking: Some(model_config.ThinkingHigh),
    )
  assert model_config.validate_resolved(settings, "pi") == Ok(Nil)
}

pub fn apply_to_argv_args_appends_model_settings_as_separate_items_test() {
  let args =
    model_config.apply_to_argv_args(
      ["--mode", "rpc"],
      model_config.Settings(
        model: Some("openai/gpt-5.1"),
        thinking: Some(model_config.ThinkingMedium),
      ),
    )

  assert args
    == ["--mode", "rpc", "--model", "openai/gpt-5.1", "--thinking", "medium"]
}

pub fn apply_to_argv_args_leaves_args_unchanged_without_settings_test() {
  let args =
    model_config.apply_to_argv_args(
      ["--mode", "rpc"],
      model_config.Settings(model: None, thinking: None),
    )

  assert args == ["--mode", "rpc"]
}
