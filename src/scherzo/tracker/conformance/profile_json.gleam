import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/tracker/conformance/types

pub fn retry_behavior_to_json(
  retry_behavior: types.RetryBehaviorConfig,
) -> json.Json {
  let types.RetryBehaviorConfig(
    remote_command_ack: remote_command_ack,
    handoff_report: handoff_report,
  ) = retry_behavior
  json.object([
    #(
      "remote_command_ack",
      option_json(remote_command_ack, retry_behavior_value_to_json),
    ),
    #(
      "handoff_report",
      option_json(handoff_report, retry_behavior_value_to_json),
    ),
  ])
}

pub fn retry_behavior_decoder() -> decode.Decoder(types.RetryBehaviorConfig) {
  use remote_command_ack <- decode.optional_field(
    "remote_command_ack",
    None,
    decode.optional(retry_behavior_value_decoder()),
  )
  use handoff_report <- decode.optional_field(
    "handoff_report",
    None,
    decode.optional(retry_behavior_value_decoder()),
  )
  decode.success(types.RetryBehaviorConfig(
    remote_command_ack: remote_command_ack,
    handoff_report: handoff_report,
  ))
}

fn retry_behavior_value_to_json(value: types.RetryBehavior) -> json.Json {
  case value {
    types.IdempotentUpdateOrDedupe -> json.string("idempotent_update_or_dedupe")
    types.DuplicateVisible -> json.string("duplicate_visible")
  }
}

fn retry_behavior_value_decoder() -> decode.Decoder(types.RetryBehavior) {
  use value <- decode.then(decode.string)
  case string.trim(value) {
    "idempotent_update_or_dedupe" ->
      decode.success(types.IdempotentUpdateOrDedupe)
    "duplicate_visible" -> decode.success(types.DuplicateVisible)
    _ -> decode.failure(types.DuplicateVisible, expected: "retry behavior")
  }
}

fn option_json(value: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case value {
    Some(inner) -> encoder(inner)
    None -> json.null()
  }
}
