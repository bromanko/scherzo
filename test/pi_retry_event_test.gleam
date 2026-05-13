import gleam/option.{None, Some}
import scherzo/agent/pi_event
import scherzo/error
import scherzo/pi/protocol
import scherzo/pi/retry_event

fn decoded_record(json: String) -> protocol.RpcRecord {
  let assert Ok(record) = protocol.decode_record(json)
  record
}

pub fn pi_event_maps_auto_retry_events_test() {
  assert pi_event.from_string("auto_retry_start") == pi_event.AutoRetryStart
  assert pi_event.from_string("auto_retry_end") == pi_event.AutoRetryEnd
  assert pi_event.to_string(pi_event.AutoRetryStart) == "auto_retry_start"
  assert pi_event.to_string(pi_event.AutoRetryEnd) == "auto_retry_end"
}

pub fn retryable_pi_error_classifies_provider_transport_protocol_errors_test() {
  assert retry_event.retryable_pi_error(error.PiProtocolError(
    "provider_transport_failure: WebSocket error",
  ))
  assert retry_event.retryable_pi_error(error.PiProtocolError("ECONNRESET"))
  assert retry_event.retryable_pi_error(error.PiProtocolError("provider 503"))
  assert retry_event.retryable_pi_error(error.PiProtocolError("429 rate limit"))
}

pub fn retryable_pi_error_rejects_local_and_context_failures_test() {
  assert !retry_event.retryable_pi_error(error.PiReadTimeout)
  assert !retry_event.retryable_pi_error(error.PiTurnTimeout)
  assert !retry_event.retryable_pi_error(error.PiStallTimeout)
  assert !retry_event.retryable_pi_error(error.PiMalformedJson("bad"))
  assert !retry_event.retryable_pi_error(error.PiLaunchFailed("bad"))
  assert !retry_event.retryable_pi_error(error.PiExited(2))
  assert !retry_event.retryable_pi_error(error.PiContextWindowExhausted(
    provider: None,
    provider_code: None,
    detail: "context",
  ))
}

pub fn retry_event_parses_auto_retry_start_record_test() {
  let record =
    decoded_record(
      "{\"type\":\"auto_retry_start\",\"attempt\":1,\"maxAttempts\":3,\"delayMs\":2000,\"errorMessage\":\"WebSocket error\"}",
    )

  assert retry_event.from_record(record)
    == Some(retry_event.AutoRetryStart(
      attempt: Some(1),
      max_attempts: Some(3),
      delay_ms: Some(2000),
      error_message: Some("WebSocket error"),
    ))
}

pub fn retry_event_parses_successful_auto_retry_end_record_test() {
  let record =
    decoded_record(
      "{\"type\":\"auto_retry_end\",\"success\":true,\"attempt\":1}",
    )

  assert retry_event.from_record(record)
    == Some(retry_event.AutoRetryEnd(
      success: True,
      attempt: Some(1),
      final_error: None,
    ))
}

pub fn retry_event_parses_failed_auto_retry_end_record_test() {
  let record =
    decoded_record(
      "{\"type\":\"auto_retry_end\",\"success\":false,\"attempt\":3,\"finalError\":\"provider_transport_failure\"}",
    )

  assert retry_event.from_record(record)
    == Some(retry_event.AutoRetryEnd(
      success: False,
      attempt: Some(3),
      final_error: Some("provider_transport_failure"),
    ))
}
