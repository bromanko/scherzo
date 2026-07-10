import gleam/option.{None, Some}
import scherzo/agent/pi_event
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

pub fn retry_event_parses_agent_end_will_retry_test() {
  let retrying = decoded_record("{\"type\":\"agent_end\",\"willRetry\":true}")
  let not_retrying =
    decoded_record("{\"type\":\"agent_end\",\"willRetry\":false}")

  assert retry_event.agent_end_will_retry(retrying) == Some(True)
  assert retry_event.agent_end_will_retry(not_retrying) == Some(False)
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
