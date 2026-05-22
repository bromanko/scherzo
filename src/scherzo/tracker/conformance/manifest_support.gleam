import gleam/dynamic/decode
import gleam/json
import scherzo/tracker/conformance/fixture_manifest
import scherzo/tracker/conformance/http_manifest
import scherzo/tracker/conformance/operation_json
import scherzo/tracker/conformance/types

pub fn validate_tasks(
  tasks: List(types.FixtureTaskDeclaration),
  adapter_kind: String,
) -> Result(Nil, types.ManifestError) {
  fixture_manifest.validate_tasks(tasks, adapter_kind)
}

pub fn tasks_to_json(tasks: List(types.FixtureTaskDeclaration)) -> json.Json {
  fixture_manifest.tasks_to_json(tasks)
}

pub fn tasks_decoder() -> decode.Decoder(List(types.FixtureTaskDeclaration)) {
  fixture_manifest.tasks_decoder()
}

pub fn validate_endpoint(
  endpoint: types.HttpEndpointConfig,
) -> Result(Nil, types.ManifestError) {
  http_manifest.validate_endpoint(endpoint)
}

pub fn endpoint_to_json(endpoint: types.HttpEndpointConfig) -> json.Json {
  http_manifest.endpoint_to_json(endpoint)
}

pub fn endpoint_decoder() -> decode.Decoder(types.HttpEndpointConfig) {
  http_manifest.endpoint_decoder()
}

pub fn comment_request_to_json(
  comment: types.CommentRequestPayload,
) -> json.Json {
  operation_json.comment_request_to_json(comment)
}

pub fn comment_request_payload_decoder() -> decode.Decoder(types.RequestPayload) {
  operation_json.comment_request_payload_decoder()
}

pub fn state_transition_request_to_json(
  transition: types.StateTransitionRequestPayload,
) -> json.Json {
  operation_json.state_transition_request_to_json(transition)
}

pub fn state_transition_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  operation_json.state_transition_payload_decoder()
}

pub fn comment_receipt_to_json(
  comment: types.CommentReceiptPayload,
) -> json.Json {
  operation_json.comment_receipt_to_json(comment)
}

pub fn comment_receipt_decoder() -> decode.Decoder(types.CommentReceiptPayload) {
  operation_json.comment_receipt_decoder()
}

pub fn state_transition_receipt_to_json(
  transition: types.StateTransitionReceiptPayload,
) -> json.Json {
  operation_json.state_transition_receipt_to_json(transition)
}

pub fn state_transition_receipt_decoder() -> decode.Decoder(
  types.StateTransitionReceiptPayload,
) {
  operation_json.state_transition_receipt_decoder()
}
