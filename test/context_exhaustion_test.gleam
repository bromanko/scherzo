import gleam/option.{None, Some}
import scherzo/agent/context_exhaustion

pub fn provider_classifier_accepts_context_limit_shapes_test() {
  let assert Some(_) =
    context_exhaustion.from_provider_error(
      Some("openai"),
      Some("context_length_exceeded"),
      "too large",
    )
  let assert Some(_) =
    context_exhaustion.from_provider_error(
      None,
      None,
      "maximum context length is 128000 tokens",
    )
  let assert Some(_) =
    context_exhaustion.from_provider_error(None, None, "prompt is too long")
  let assert Some(_) =
    context_exhaustion.from_provider_error(
      None,
      None,
      "too many input tokens were supplied",
    )
}

pub fn provider_classifier_rejects_non_context_failures_test() {
  let assert None =
    context_exhaustion.from_provider_error(
      None,
      Some("rate_limit_exceeded"),
      "try later",
    )
  let assert None =
    context_exhaustion.from_provider_error(None, None, "authentication failed")
  let assert None =
    context_exhaustion.from_provider_error(None, None, "quota exceeded")
  let assert None =
    context_exhaustion.from_provider_error(
      None,
      None,
      "pi stall timeout elapsed without output",
    )
}
