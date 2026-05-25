import gleam/dynamic/decode
import gleam/json
import scherzo/tracker/conformance/fixture_manifest
import scherzo/tracker/conformance/http_manifest
import scherzo/tracker/conformance/operation_json
import scherzo/tracker/conformance/profile_json
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

pub fn retry_behavior_to_json(
  retry_behavior: types.RetryBehaviorConfig,
) -> json.Json {
  profile_json.retry_behavior_to_json(retry_behavior)
}

pub fn retry_behavior_decoder() -> decode.Decoder(types.RetryBehaviorConfig) {
  profile_json.retry_behavior_decoder()
}

pub fn remote_command_fetch_to_json(
  fetch: types.RemoteCommandFetchPayload,
) -> json.Json {
  operation_json.remote_command_fetch_to_json(fetch)
}

pub fn remote_command_fetch_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  operation_json.remote_command_fetch_payload_decoder()
}

pub fn remote_command_event_to_json(
  event: types.RemoteCommandEventPayload,
) -> json.Json {
  operation_json.remote_command_event_to_json(event)
}

pub fn remote_command_event_decoder() -> decode.Decoder(
  types.RemoteCommandEventPayload,
) {
  operation_json.remote_command_event_decoder()
}

pub fn remote_command_ack_to_json(
  ack: types.RemoteCommandAckPayload,
) -> json.Json {
  operation_json.remote_command_ack_to_json(ack)
}

pub fn remote_command_ack_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  operation_json.remote_command_ack_payload_decoder()
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

pub fn handoff_event_to_json(event: types.HandoffEventPayload) -> json.Json {
  operation_json.handoff_event_to_json(event)
}

pub fn handoff_report_payload_decoder() -> decode.Decoder(types.RequestPayload) {
  operation_json.handoff_report_payload_decoder()
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

pub fn handoff_report_receipt_to_json(
  receipt: types.HandoffReportReceiptPayload,
) -> json.Json {
  operation_json.handoff_report_receipt_to_json(receipt)
}

pub fn handoff_report_receipt_decoder() -> decode.Decoder(
  types.HandoffReportReceiptPayload,
) {
  operation_json.handoff_report_receipt_decoder()
}

pub fn scheduled_failure_publication_to_json(
  publication: types.ScheduledFailurePublicationPayload,
) -> json.Json {
  operation_json.scheduled_failure_publication_to_json(publication)
}

pub fn scheduled_failure_publication_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  operation_json.scheduled_failure_publication_payload_decoder()
}

pub fn scheduled_failure_receipt_to_json(
  receipt: types.ScheduledFailureReceiptPayload,
) -> json.Json {
  operation_json.scheduled_failure_receipt_to_json(receipt)
}

pub fn scheduled_failure_receipt_decoder() -> decode.Decoder(
  types.ScheduledFailureReceiptPayload,
) {
  operation_json.scheduled_failure_receipt_decoder()
}
