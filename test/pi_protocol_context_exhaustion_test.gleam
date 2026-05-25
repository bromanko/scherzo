import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/pi/protocol
import scherzo/session/tokens as session_tokens
import simplifile

pub fn decode_current_context_length_fixture_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/pi/context-length-exceeded-current-protocol.jsonl",
    )
  let assert [line] = string.split(string.trim(contents), "\n")
  let assert Ok(record) = protocol.decode_record(line)
  let assert Some(context) = context_exhaustion.from_rpc_record(record)
  assert context.provider == Some("openai-codex")
  assert context.provider_code == Some("context_length_exceeded")
}

pub fn decode_rate_limit_fixture_is_not_context_exhaustion_test() {
  let assert Ok(contents) =
    simplifile.read("test/fixtures/pi/non-context-rate-limit.jsonl")
  let assert [line] = string.split(string.trim(contents), "\n")
  let assert Ok(record) = protocol.decode_record(line)
  assert context_exhaustion.from_rpc_record(record) == None
}

pub fn compaction_reason_ignores_malformed_retained_raw_json_test() {
  let record = minimal_record("compaction_start", "{not json")
  assert protocol.compaction_reason(record) == None
}

pub fn compaction_command_codec_and_reason_test() {
  let compact = protocol.encode_compact("7", Some("Focus on workspace state"))
  assert string.contains(compact, "compact")
  assert string.contains(compact, "customInstructions")
  assert !string.contains(
    protocol.encode_compact("8", None),
    "customInstructions",
  )
  assert string.contains(
    protocol.encode_set_auto_compaction("9", enabled: True),
    "set_auto_compaction",
  )

  let assert Ok(start) =
    protocol.decode_record(
      "{\"type\":\"compaction_start\",\"reason\":\"manual\"}",
    )
  let assert Ok(end) =
    protocol.decode_record(
      "{\"type\":\"compaction_end\",\"data\":{\"reason\":\"overflow\"}}",
    )
  assert protocol.compaction_reason(start) == Some("manual")
  assert protocol.compaction_reason(end) == Some("overflow")
}

fn minimal_record(type_: String, raw_json: String) -> protocol.RpcRecord {
  protocol.RpcRecord(
    type_: type_,
    id: None,
    command: None,
    success: None,
    session_id: None,
    session_file: None,
    cwd: None,
    delta: None,
    assistant_event_type: None,
    message: None,
    stop_reason: None,
    error_message: None,
    method: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
    tool_call_id: None,
    tool_calls: [],
    assistant_messages: [],
    raw_json: raw_json,
  )
}
