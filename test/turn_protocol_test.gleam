import gleam/string
import scherzo/agent/turn_protocol
import scherzo/error
import scherzo/pi/protocol

fn decoded_record(json: String) -> protocol.RpcRecord {
  let assert Ok(record) = protocol.decode_record(json)
  record
}

pub fn read_error_maps_closed_stream_without_turn_end_test() {
  let records = [
    decoded_record("{\"type\":\"agent_start\"}"),
    decoded_record("{\"type\":\"turn_start\"}"),
    decoded_record(
      "{\"cursor\":15044,\"type\":\"message_start\",\"message\":{\"role\":\"assistant\",\"provider\":\"openai-codex-responses\",\"stopReason\":\"stop\",\"content\":[]}}",
    ),
  ]

  let assert error.PiProtocolError(message) =
    turn_protocol.read_error(error.PiProtocolError("port closed"), records)

  assert string.contains(message, "pi_stream_ended_without_turn_end")
  assert string.contains(message, "stream_closed detail=port closed")
  assert string.contains(message, "last_event_cursor=15044")
  assert string.contains(message, "last_event_kind=assistant_message")
  assert string.contains(message, "last_event_type=message_start")
}
