import gleam/option.{Some}
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
